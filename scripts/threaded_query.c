// gcc -shared -fpic threaded_query.c -o threaded_query.so -llua5.1 \
//     -lmysqlclient -lrt
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

#include <mysql/mysql.h>

#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>

#define HAPPY_THREAD_EXIT       (void *)101
#define SAD_THREAD_EXIT         (void *)0x5AD

#define STOP_KILLING                                                    \
        assert(!pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL))

#define RESUME_KILLING                                                  \
        assert(!pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL))

#define DMZ(stmt)                                                   \
    STOP_KILLING;                                                   \
    (stmt);                                                         \
    RESUME_KILLING;

#define MAX_TIMEOUTS                    10
#define COMMAND_OUTPUT_COUNT            2

enum Command {QUERY, RESULTS, KILL};

// SUCCESS          : a thread was running and we killed it
// NOTHING_TO_STOP  : tried to stop a thread; but none was running
// PENDING_SELF_DESTRUCTION : a thread was running and we sent it a kill
//                            signal, but it has not yet enabled kill
//                            mode. the thread must exit at the next
//                            opportunity and clean up it's own metadata.
// FAILURE          : a thread was running, but we failed to kill it.
enum STOP_TYPE {SUCCESS, NOTHING_TO_STOP, PENDING_SELF_DESTRUCTION,
                FAILURE};

# define BOX_SIZE 100
typedef struct ValidityBox {
    bool valid;
    unsigned char *value;
} Box;

#define THD(box)                                            \
    (assert((box).valid), **(pthread_t **)&(box).value)

#define THD_ADDR(box)                                       \
    ((box).valid = true,                                    \
     *(pthread_t **)&(box).value)

#define MUTEX(box)                                          \
    (assert((box).valid), **(pthread_mutex_t **)&(box).value)

#define MUTEX_ADDR(box)                                     \
    ((box).valid = true,                                    \
     *(pthread_mutex_t **)&(box).value)

#define COND(box)                                           \
    (assert((box).valid), **(pthread_cond_t **)&(box).value)

#define COND_ADDR(box)                                      \
    ((box).valid = true,                                    \
     *(pthread_cond_t **)&(box).value)

#define NEW_BOX                                             \
    ((Box){.valid = false, .value = nullFail(calloc(BOX_SIZE, 1))})

#define ASSERT_VALID_BOX(box)                               \
{                                                           \
    assert((box).valid);                                    \
}

#define ASSERT_INVALID_BOX(box)                             \
{                                                           \
    assert(!(box).valid);                                   \
}

#define INVALIDATE(box)                                     \
{                                                           \
    ((Box*)&(box))->valid = false;                          \
    memset((void *)(box).value, 0xFFFFFFFF, BOX_SIZE);      \
}

struct HostData {
    const char *host;
    const char *user;
    const char *passwd;
    unsigned int port;
};

// survives restarts
// > must not be modified by commandHandler; these values can always be
//   meaningfully read after running issueCommand.
struct PersistentState {
    // using a pointer for 'host_data' allows for PersistentState
    // to determine if 'host_data' is valid or not.
    // > non-NULL == valid
    const struct HostData *host_data;       // deep copied

    // parameter to all commands
    lua_State *ell;

    unsigned int wait;
};

enum LuaQueryState {NORMAL, DEAD, SELF_OWNING};
struct LuaQuery {
    // internal state
    enum LuaQueryState state;
    enum Command command;
    bool command_ready;
    bool completion_signal;
    Box thread;
    struct PersistentState persist;

    // QUERY parameter
    const char *sql;                        // deep copied

    // output
    unsigned int output_count;

    // mysql may take 20 or 30 seconds to timeout on failure and we don't
    // want to kill the whole server when this happens
    bool mysql_connected;

    // Synchronization
    Box cond;
    Box mutex;
};

static struct HostData *createHostData(const char *const host,
                                       const char *const user,
                                       const char *const passwd,
                                       unsigned int port);
static void destroyHostData(struct HostData **p_host_data);

static struct LuaQuery **
createLuaQuery(struct HostData **const p_host_data, unsigned int wait);
static void destroyLuaQuery(struct LuaQuery ***const pp_lua_query);
static bool startLuaQueryThread(struct LuaQuery *const lua_query);
static enum STOP_TYPE
lowLevelStopLuaQueryThread(struct LuaQuery *const lua_query);
bool niceStopLuaQueryThread(struct LuaQuery *const lua_query);

void initLuaQuery(struct LuaQuery *const lua_query);
void finishedCommandIssue(struct LuaQuery *const lua_query);
void strangeFinishedCommandIssue(struct LuaQuery *const lua_query,
                                 bool exit_status);

static void issueCommand(lua_State *L, enum Command command,
                         struct LuaQuery **const p_lua_query);
static bool issueQuery(lua_State *L, const char *const query,
                       struct LuaQuery **const p_lua_query);
static void *commandHandler(void *const lq);
bool nonResponsiveRemote();

static void nilTheStack(lua_State *const L, int pushed);
static void nilTheStackPlus(struct LuaQuery *const lua_query,
                            int pushed);

static void pushvalue(lua_State *const L, const char *const string,
                      long int len);
static const char *luaToCharp(lua_State *const L, int index);
void *nullFail(void *p);
bool validBox(Box b);
bool saneLuaQuery(struct LuaQuery *const lua_query);
void writeQueryErrorToSecondaryLog(const char *const error);

unsigned global_timeouts = 0;

static int
start(lua_State *const L)
{
    // don't try to connect if cryptdb is being non-responsive
    if (nonResponsiveRemote()) {
        lua_pushboolean(L, false);
        lua_pushnil(L);
        return 2;
    }

    const char *host        = luaToCharp(L, 1);
    const char *user        = luaToCharp(L, 2);
    const char *passwd      = luaToCharp(L, 3);
    const unsigned int port = lua_tointeger(L, 4);
    const unsigned int wait = lua_tointeger(L, 5);

    struct HostData *host_data =
        createHostData(host, user, passwd, port);
    if (!host_data) {
        fprintf(stderr, "createHostData failed!\n");
        lua_pushboolean(L, false);
        lua_pushnil(L);
        return 2;
    }

    struct LuaQuery **const p_lua_query =
        createLuaQuery(&host_data, wait);
    if (!p_lua_query) {
        destroyHostData(&host_data);
        fprintf(stderr, "createLuaQuery failed!\n");
        lua_pushboolean(L, false);
        lua_pushnil(L);
        return 2;
    }

    lua_pushboolean(L, true);
    lua_pushnumber(L, (unsigned long)p_lua_query);
    return 2;
}

static int
query(lua_State *const L)
{
    struct LuaQuery **p_lua_query =
        (struct LuaQuery **)lua_tointeger(L, 1);
    assert(p_lua_query && *p_lua_query);

    const char *const q = luaToCharp(L, 2);

    if (false == issueQuery(L, q, p_lua_query)) {
        fprintf(stderr, "issueQuery failed!\n");
        lua_pushboolean(L, false);
        nilTheStackPlus(*p_lua_query, 1);
        return COMMAND_OUTPUT_COUNT;
    }

    assert(COMMAND_OUTPUT_COUNT == (*p_lua_query)->output_count);
    return (*p_lua_query)->output_count;
}

static int
results(lua_State *const L)
{
    struct LuaQuery **p_lua_query =
        (struct LuaQuery **)lua_tointeger(L, 1);
    assert(p_lua_query && *p_lua_query);

    issueCommand(L, RESULTS, p_lua_query);
    assert(COMMAND_OUTPUT_COUNT == (*p_lua_query)->output_count);
    return (*p_lua_query)->output_count;
}

// if it returns, it succeeded; otherwise we aborted
static int
kill(lua_State *const L)
{
    struct LuaQuery **p_lua_query =
        (struct LuaQuery **)lua_tointeger(L, 1);
    assert(p_lua_query && *p_lua_query);

    // soft kill
    issueCommand(L, KILL, p_lua_query);

    const int output_count = (*p_lua_query)->output_count;
    assert(COMMAND_OUTPUT_COUNT == output_count);

    // hard kill
    destroyLuaQuery(&p_lua_query);
    lua_pushboolean(L, true);
    nilTheStack(L, 1);
    return COMMAND_OUTPUT_COUNT;
}

// rider function
static int
_geteuid(lua_State *const L)
{
    lua_pushnumber(L, geteuid());
    return 1;
}
static const struct luaL_reg
main_lib[] = {
#define F(n) { #n, n }
    F(start),
    F(query),
    F(results),
    F(kill),
    F(_geteuid),
    {0, 0},
};

extern int
lua_main_init(lua_State *const L)
{
    luaL_openlib(L, "ThreadedQuery", main_lib, 0);
    return 1;
}


// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//                  Helpers
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

// > the caller should only directly access
//   LuaQuery::persist::output_count; the rest of LuaQuery is opaque
void
issueCommand(lua_State *const L, enum Command command,
             struct LuaQuery **const p_lua_query)
{
    assert(L && p_lua_query && *p_lua_query);
    assert(saneLuaQuery(*p_lua_query));

    assert(NULL  == (*p_lua_query)->persist.ell);
    assert(false == (*p_lua_query)->completion_signal);

    // This allows us to intelligibly return at any point hereafter
    (*p_lua_query)->command       = command;
    (*p_lua_query)->persist.ell   = L;

    if (NORMAL != (*p_lua_query)->state || nonResponsiveRemote()) {
        strangeFinishedCommandIssue(*p_lua_query, false);
        return;
    }

    // execute command
    (*p_lua_query)->command_ready = true;

    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        fprintf(stderr, "clock_gettime failed!\n");
        strangeFinishedCommandIssue(*p_lua_query, false);
        return;
    }
    ts.tv_sec += (*p_lua_query)->persist.wait;

    if (pthread_mutex_lock(MUTEX_ADDR((*p_lua_query)->mutex))) {
        perror("pthread_mutex_lock failed!\n");
        // if we don't acquire the lock we can't tell the worker thread
        // that we have work for him.
        strangeFinishedCommandIssue(*p_lua_query, false);
        return;
    }

    if (pthread_cond_signal(COND_ADDR((*p_lua_query)->cond))) {
        perror("pthread_cond_signal failed!\n");
        // if we don't signal the worker thread he won't know that we
        // have work for him
        // > we must fail to unlock the mutex, otherwise things become
        // unrecoverable.
        assert(!pthread_mutex_unlock(MUTEX_ADDR((*p_lua_query)->mutex)));
        strangeFinishedCommandIssue(*p_lua_query, false);
        return;
    }

    while (false == (*p_lua_query)->completion_signal) {
        const int error =
            pthread_cond_timedwait(COND_ADDR((*p_lua_query)->cond),
                                   MUTEX_ADDR((*p_lua_query)->mutex),
                                   &ts);
        if (0 == error) {
            // Make sure this is not a spurious wakeup.
            continue;
        }

        if (ETIMEDOUT != error) {
            perror("pthread_cond_timedwait failed,"
                   " trying to continue\n");
        }
        break;
    }
    // we must unlock the mutex otherwise things become unrecoverable
    assert(!pthread_mutex_unlock(MUTEX_ADDR((*p_lua_query)->mutex)));

    // handle result
    if (false == (*p_lua_query)->completion_signal) {
        fprintf(stderr, "no completion signal, stopping thread!\n");
        if (false == niceStopLuaQueryThread(*p_lua_query)) {
            fprintf(stderr, "niceStopLuaQueryThread failed!");
            // can't continue from here as a rogue thread has a pointer
            // into our lua_State
            exit(0);
        }
        assert((*p_lua_query)->state != NORMAL);

        ++global_timeouts;
        assert((*p_lua_query)->persist.ell);
        strangeFinishedCommandIssue(*p_lua_query, false);
        return;
    }

    assert(L        == (*p_lua_query)->persist.ell);
    assert(true     == (*p_lua_query)->completion_signal);
    assert(false    == (*p_lua_query)->command_ready);

    finishedCommandIssue((*p_lua_query));
    return;
}

bool
issueQuery(lua_State *const L, const char *const query,
           struct LuaQuery **const p_lua_query)
{
    // HACK.
    (*p_lua_query)->sql = strdup(query);
    if (NULL == (*p_lua_query)->sql) {
        perror("strdup failed!\n");
        return false;
    }

    issueCommand(L, QUERY, p_lua_query);
    return true;
}

static void
waitForCommand(struct LuaQuery *const lua_query)
{
    // > this mutex should always be available to us here.
    assert(!pthread_mutex_lock(MUTEX_ADDR(lua_query->mutex)));
    while (false == lua_query->command_ready) {
        // A cancellatin point; if killed during pthread_cond_wait the
        // mutex is reacquired before calling cleanup handlers
        const int error =
            pthread_cond_wait(COND_ADDR(lua_query->cond),
                              MUTEX_ADDR(lua_query->mutex));
        if (0 == error) {
            // check for spurious wakeups
            continue;
        }

        perror("panicking: pthread_cond_wait failed!\n");
        exit(0);
    }
    pthread_mutex_unlock(MUTEX_ADDR(lua_query->mutex));

    assert(false == lua_query->completion_signal);
    assert(lua_query->persist.ell);
    return;
}

static void
freeSQL(struct LuaQuery *const lua_query)
{
    assert(!!lua_query->sql == !!(QUERY == lua_query->command));
    if (lua_query->sql) {
        free((void *)lua_query->sql);
        lua_query->sql = NULL;
    }
}

// commandHandler signals back that it has finished
static void
completeLuaQuery(struct LuaQuery *const lua_query,
                 unsigned output_count)
{
    assert(true     == lua_query->command_ready);
    assert(false    == lua_query->completion_signal);
    assert(lua_query->persist.ell);

    nilTheStackPlus(lua_query, output_count);
    assert(COMMAND_OUTPUT_COUNT == lua_query->output_count);

    lua_query->command_ready     = false;
    lua_query->completion_signal = true;

    assert(!pthread_cond_signal(COND_ADDR(lua_query->cond)));
}

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//    issueCommand puts things back to a neutral state
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
void
finishedCommandIssue(struct LuaQuery *const lua_query)
{
    // Do the first; so we can keep asserts.
    freeSQL(lua_query);

    lua_query->command_ready        = false;
    lua_query->completion_signal    = false;
    lua_query->command              = -1;
    lua_query->persist.ell          = NULL;

    assert(saneLuaQuery(lua_query));
}

void
strangeFinishedCommandIssue(struct LuaQuery *const lua_query,
                            bool exit_status)
{
    // we may be in an abnormal state; as we likely came here after an
    // asynch kill.
    assert(lua_query->persist.ell);

    lua_pushboolean(lua_query->persist.ell, exit_status);
    nilTheStackPlus(lua_query, 1);
    assert(COMMAND_OUTPUT_COUNT == lua_query->output_count);

    finishedCommandIssue(lua_query);
}

// close the mysql connection
static void
mysql_cleanup_handler(void *const mysql_conn)
{
    assert(mysql_conn);
    mysql_close(mysql_conn);
}

static void
mutex_cleanup_handler(void *const lpq)
{
    // if we have the mutex locked; kill it during 'pthread_cond_wait' or
    // between pthread_mutex_lock and pthread_cond_wait or between
    // pthread_cond_wait and pthread_mutex_unlock or between
    // pthread_cond_wait and pthread_cond_wait during a spurious wakeup.
    // > we must unlock this mutex so that it can be destroyed

    struct LuaQuery *const lua_query = (struct LuaQuery *)lpq;
    const int err = pthread_mutex_unlock(MUTEX_ADDR(lua_query->mutex));
    assert(0 == err || EPERM == err);
}

// ???: The docs _seem_ to indicate that pthread_exit should be called
// implicity on return; but empirically this does not seem to be
// happening.
void *
commandHandler(void *const lq)
{
    // mysql_store_result, lua_newtable, lua_pushnumber, etc are
    // probably not kill safe
    // > we rely on pthread_cond_wait as a cancellation point
    assert(!pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL));
    assert(!pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL));

    struct LuaQuery *lua_query = (struct LuaQuery *)lq;
    assert(lua_query);

    // do we need to do this for each thread?
    MYSQL *const init = mysql_init(NULL);
    if (!init) {
        perror("mysql_init failed!\n");
        return SAD_THREAD_EXIT;
    }

    // mysql does not respect the timeout to the precise quantity
    unsigned int connect_timeout = 1;
    if (mysql_options(init, MYSQL_OPT_CONNECT_TIMEOUT, &connect_timeout)) {
        perror("mysql_options failed!\n");
        return SAD_THREAD_EXIT;
    }

    // hackery, we don't want to receive asynchronous cancellations until
    // after we acquire resources.  we also need to clean up our mysql
    // connection if it succeeds; given all exit paths.
    MYSQL *const conn =
        mysql_real_connect(init, lua_query->persist.host_data->host,
                           lua_query->persist.host_data->user,
                           lua_query->persist.host_data->passwd, NULL,
                           lua_query->persist.host_data->port, NULL, 0);
    if (!conn) {
        perror("mysql_real_connect failed!\n");
        return SAD_THREAD_EXIT;
    }
    lua_query->mysql_connected = true;

    // if the caller tried to kill us before this point, we should
    // immediately die after entering enable kill mode
    pthread_cleanup_push(&mysql_cleanup_handler, conn);
    pthread_cleanup_push(&mutex_cleanup_handler, lua_query);
    assert(!pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL));

    bool queried = false;
    bool query_succeeded;
    while (true) {
        // blocking
        waitForCommand(lua_query);

        assert(false == lua_query->completion_signal);
        if (KILL == lua_query->command) {
            lua_pushboolean(lua_query->persist.ell, true);
            completeLuaQuery(lua_query, 1);
            pthread_exit(HAPPY_THREAD_EXIT);
        } else if (QUERY == lua_query->command) {
            // 'queried' could be true here if we miss a RESULTS
            // signal due to the 'optimization'
            // assert(false == queried);

            assert(lua_query->sql);
            queried = true;

            // optimization: don't block the caller with mysql_query
            // > we could miss the next signal from the caller, in
            // which case he will time us out and continue executing.
            const char *const query = nullFail(strdup(lua_query->sql));
            lua_pushboolean(lua_query->persist.ell, true);
            // caller thinks we finish here
            completeLuaQuery(lua_query, 1);

            query_succeeded = !mysql_query(conn, query);
            if (false == query_succeeded) {
                assert(mysql_errno(conn));
            }
            free((void *)query);
            continue;
        }

        // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        //            fetch results
        // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        assert(RESULTS == lua_query->command);
        if (false == queried) {
            // trying to get results after thread was killed during QUERY
            lua_pushboolean(lua_query->persist.ell, false);

            completeLuaQuery(lua_query, 1);
            continue;
        }
        queried = false;

        if (false == query_succeeded) {
            // query failed
            writeQueryErrorToSecondaryLog(mysql_error(conn));
            lua_pushboolean(lua_query->persist.ell, false);
            completeLuaQuery(lua_query, 1);
            continue;
        }

        MYSQL_RES *const result = mysql_store_result(conn);
        const int field_count = mysql_field_count(conn);
        if (NULL == result) {
            if (mysql_errno(conn)) {
                // query failed
                lua_pushboolean(lua_query->persist.ell, false);

                completeLuaQuery(lua_query, 1);
                continue;
            }

            lua_pushboolean(lua_query->persist.ell, true);
            lua_pushnumber(lua_query->persist.ell,
                           mysql_affected_rows(conn));

            completeLuaQuery(lua_query, 2);
            continue;
        }
        assert(0 == mysql_errno(conn));

        // resultset
        lua_pushboolean(lua_query->persist.ell, true);
        lua_newtable(lua_query->persist.ell);

        // gives us a 2-3x speedup on queries with hundreds of results
        if (mysql_num_rows(result) > 100) {
            mysql_free_result(result);
            completeLuaQuery(lua_query, 2);
            continue;
        }

        MYSQL_ROW row;
        int row_index = 0;
        while ((row = mysql_fetch_row(result))) {
            const unsigned long *const l = mysql_fetch_lengths(result);

            // row
            lua_pushnumber(lua_query->persist.ell, row_index + 1);
            lua_newtable(lua_query->persist.ell);
            int col_index = 0;
            for (; col_index < field_count; ++col_index) {
                lua_pushnumber(lua_query->persist.ell, col_index + 1);
                pushvalue(lua_query->persist.ell, row[col_index],
                          l[col_index]);
                lua_settable(lua_query->persist.ell, -3);
            }
            lua_settable(lua_query->persist.ell, -3);

            row_index += 1;
        }

        mysql_free_result(result);
        completeLuaQuery(lua_query, 2);
    }

    // should never reach this point.
    assert(false);
    pthread_cleanup_pop(0);
    pthread_cleanup_pop(0);
}

#define IF_THEN_FREE(p)                             \
{                                                   \
    if (p) {free((void *)p);}                       \
}

struct HostData *
createHostData(const char *const host, const char *const user,
               const char *const passwd, unsigned int port)
{
    struct HostData *const host_data = malloc(sizeof(struct HostData));
    if (!host_data) {
        perror("createHostData failed!\n");
        return NULL;
    }

    host_data->host = strdup(host);
    host_data->user = strdup(user);
    host_data->passwd = strdup(passwd);
    if (!host_data->host || !host_data->user || !host_data->passwd) {
        IF_THEN_FREE(host_data->host);
        IF_THEN_FREE(host_data->user);
        IF_THEN_FREE(host_data->passwd);
        free(host_data);
        perror("strdup failed!\n");
        return NULL;
    }

    host_data->port = port;

    return host_data;
}

#undef IF_THEN_FREE

void
destroyHostData(struct HostData **p_host_data)
{
    assert(p_host_data && *p_host_data);

    free((void *)(*p_host_data)->host);
    free((void *)(*p_host_data)->user);
    free((void *)(*p_host_data)->passwd);
    free((void *)(*p_host_data));

    *p_host_data = NULL;
}

// doesn't modify persistent data
void
initLuaQuery(struct LuaQuery *const lua_query)
{
    lua_query->state                = NORMAL;
    lua_query->command_ready        = false;
    lua_query->completion_signal    = false;
    lua_query->command              = -1;
    lua_query->sql                  = NULL;

    _Static_assert(sizeof(pthread_t) <= BOX_SIZE,
                   "BOX_SIZE is too small for pthread_t!");
    lua_query->thread               = NEW_BOX;

    lua_query->output_count         = -1;
    lua_query->mysql_connected      = false;

    _Static_assert(sizeof(pthread_mutex_t) <= BOX_SIZE,
                   "BOX_SIZE is too small for pthread_mutex_t!");
    lua_query->mutex                = NEW_BOX;

    _Static_assert(sizeof(pthread_cond_t) <= BOX_SIZE,
                   "BOX_SIZE is too small for pthread_cond_t!");
    lua_query->cond                 = NEW_BOX;
}

static bool
newLuaQuery(struct LuaQuery *const lua_query,
            struct HostData *const host_data,
            unsigned int wait)
{
    memset(lua_query, 0, sizeof(struct LuaQuery));

    initLuaQuery(lua_query);

    // FIXME: 'persist' is deprecated
    lua_query->persist.host_data = host_data;
    lua_query->persist.ell       = NULL;
    lua_query->persist.wait      = wait;

    if (false == startLuaQueryThread(lua_query)) {
        fprintf(stderr, "startLuaQueryThread failed!\n");
        return false;
    }

    return true;
}

// on success, we take ownership of @p_host_data
static struct LuaQuery **
createLuaQuery(struct HostData **const p_host_data, unsigned int wait)
{
    assert(p_host_data && *p_host_data);

    struct LuaQuery **p_lua_query = malloc(sizeof(struct LuaQuery *));
    if (!p_lua_query) {
        perror("malloc failed!\n");
        return NULL;
    }

    struct LuaQuery *const lua_query = malloc(sizeof(struct LuaQuery));
    if (!lua_query) {
        free(p_lua_query);
        perror("malloc failed!\n");
        return NULL;
    }
    *p_lua_query = lua_query;

    if (false == newLuaQuery(lua_query, *p_host_data, wait)) {
        free(p_lua_query);
        free(lua_query);
        fprintf(stderr, "newLuaQuery failed!\n");
        return NULL;
    }

    // caller loses ownership
    *p_host_data = NULL;

    return p_lua_query;
}
bool
startLuaQueryThread(struct LuaQuery *const lua_query)
{
    ASSERT_INVALID_BOX(lua_query->thread);
    ASSERT_INVALID_BOX(lua_query->mutex);
    ASSERT_INVALID_BOX(lua_query->cond);

    if (pthread_mutex_init(MUTEX_ADDR(lua_query->mutex), NULL)) {
        INVALIDATE(lua_query->mutex);

        perror("pthread_mutex_init failed!\n");
        return false;
    }

    if (pthread_cond_init(COND_ADDR(lua_query->cond), NULL)) {
        pthread_mutex_destroy(MUTEX_ADDR(lua_query->mutex));
        INVALIDATE(lua_query->mutex);
        INVALIDATE(lua_query->cond);

        perror("pthread_cond_init failed!\n");
        return false;
    }

    if (pthread_create(THD_ADDR(lua_query->thread), NULL, commandHandler,
                       (void *)lua_query)) {
        pthread_mutex_destroy(MUTEX_ADDR(lua_query->mutex));
        pthread_cond_destroy(COND_ADDR(lua_query->cond));
        INVALIDATE(lua_query->mutex);
        INVALIDATE(lua_query->cond);
        INVALIDATE(lua_query->thread);

        perror("pthread_create failed!\n");
        return false;
    }

    ASSERT_VALID_BOX(lua_query->thread);
    ASSERT_VALID_BOX(lua_query->mutex);
    ASSERT_VALID_BOX(lua_query->cond);

    return true;
}

// NOTE: Callers are apt to fault if this function does not complete
// successfully.
enum STOP_TYPE
lowLevelStopLuaQueryThread(struct LuaQuery *const lua_query)
{
    ASSERT_VALID_BOX(lua_query->mutex);
    ASSERT_VALID_BOX(lua_query->cond);

    // kill(...) issues a soft kill that then a hardkill, so we don't
    // do anything if the soft kill succeeded
    if (false == validBox(lua_query->thread)) {
        return NOTHING_TO_STOP;
    }

    // try to cancel the thread if it hasn't already exited
    // > not safe to cancel a thread after joining it as system may reuse
    //   id
    const int why = pthread_cancel(THD(lua_query->thread));
    if (why != 0 && why != ESRCH) {
        perror("pthread_cancel did something weird!!\n");
        return FAILURE;
    }

    // Don't wait for mysql to connect (or fail to connect)
    // > Once it's done connecting the thread will either exit because
    //   it failed to connect, or because pthread_cancel has queued
    //   a kill request for it.
    if (false == lua_query->mysql_connected) {
        assert(!pthread_detach(THD(lua_query->thread)));
        INVALIDATE(lua_query->thread);
        return PENDING_SELF_DESTRUCTION;
    }

    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        fprintf(stderr, "clock_gettime failed!\n");
        return FAILURE;
    }

    ts.tv_sec += lua_query->persist.wait;

    void *exit_code;
    if (pthread_timedjoin_np(THD(lua_query->thread), &exit_code, &ts)) {
        perror("pthread_timedjoin_np failed!\n");
        return FAILURE;
    }

    if (exit_code != PTHREAD_CANCELED && exit_code != HAPPY_THREAD_EXIT
        && exit_code != SAD_THREAD_EXIT) {
        fprintf(stderr, "unclear why thread exited: %p\n", exit_code);
        return FAILURE;
    }

    INVALIDATE(lua_query->thread);
    return SUCCESS;
}

bool
niceStopLuaQueryThread(struct LuaQuery *p_lua_query)
{
    // Until we explicitly stop the thread, *p_lua_query can be modified
    // from under us.
    assert(p_lua_query);

    // now we can stop the thread
    const enum STOP_TYPE stop_type =
        lowLevelStopLuaQueryThread(p_lua_query);
    switch (stop_type) {
        case FAILURE:
            fprintf(stderr, "lowLevelStopLuaQuery failed\n");
            return false;
        case PENDING_SELF_DESTRUCTION:
            // we lost ownership of original metadata
            // > MEMLEAK
            p_lua_query->state = SELF_OWNING;
            break;
        case SUCCESS:
        case NOTHING_TO_STOP:
            // we retained ownership of original metadata
            p_lua_query->state = DEAD;
            break;
        default:
            fprintf(stderr, "unknown stop type!\n");
            return false;
    }
    ASSERT_INVALID_BOX(p_lua_query->thread);

    return true;
}

static void
destroyLuaQueryMetaData(struct LuaQuery ***const pp_lua_query)
{
    assert(pp_lua_query && *pp_lua_query && **pp_lua_query);

    freeSQL(**pp_lua_query);
    destroyHostData((struct HostData **)&(**pp_lua_query)->persist.host_data);

    // sanity check
    assert(!pthread_mutex_trylock(MUTEX_ADDR((**pp_lua_query)->mutex)));
    assert(!pthread_mutex_unlock(MUTEX_ADDR((**pp_lua_query)->mutex)));

    // FIXME: This destroy fails; must determine why.
    pthread_mutex_destroy(MUTEX_ADDR((**pp_lua_query)->mutex));

    free((**pp_lua_query)->mutex.value);
    (**pp_lua_query)->mutex.value = NULL;

    assert(!pthread_cond_destroy(COND_ADDR((**pp_lua_query)->cond)));
    free((**pp_lua_query)->cond.value);
    (**pp_lua_query)->cond.value = NULL;

    free(**pp_lua_query);
    **pp_lua_query = NULL;

    free(*pp_lua_query);
    *pp_lua_query = NULL;
}

static void
destroyLuaQuery(struct LuaQuery ***const pp_lua_query)
{
    assert(pp_lua_query && *pp_lua_query && **pp_lua_query);

    // if necessary, stop the thread first as this could potentially
    // change state
    if (NORMAL == (**pp_lua_query)->state) {
        if (false == niceStopLuaQueryThread(**pp_lua_query)) {
            fprintf(stderr, "niceStopLuaQueryThread failed!\n");
            exit(0);
        }
    }
    assert(NORMAL != (**pp_lua_query)->state);

    switch ((**pp_lua_query)->state) {
        case DEAD:
            ASSERT_INVALID_BOX((**pp_lua_query)->thread);
            destroyLuaQueryMetaData(pp_lua_query);
            return;
        case SELF_OWNING:
            // Must clean it's self up
            // FIXME: Should this be invalid?
            ASSERT_INVALID_BOX((**pp_lua_query)->thread);
            *pp_lua_query = NULL;
            return;
        default:
            fprintf(stderr, "bad state in destroyLuaQuery!");
            exit(0);
    }
}

bool
nonResponsiveRemote()
{
    assert(global_timeouts <= MAX_TIMEOUTS);
    return MAX_TIMEOUTS == global_timeouts;
}

void
nilTheStack(lua_State *const L, int pushed)
{
    for (; pushed < COMMAND_OUTPUT_COUNT; ++pushed) {
        lua_pushnil(L);
    }
}

void
nilTheStackPlus(struct LuaQuery *const lua_query, int pushed)
{
    nilTheStack(lua_query->persist.ell, pushed);
    lua_query->output_count = COMMAND_OUTPUT_COUNT;
}

// taken from luasql-mysql
void
pushvalue(lua_State *const L, const char *const string, long int len)
{
    if (string == NULL) {
        lua_pushnil(L);
    } else {
        lua_pushlstring(L, string, len);
    }
}

const char *
luaToCharp(lua_State *const L, int index)
{
    size_t length;
    const char *const p = lua_tolstring(L, index, &length);
    if (!p) {
        return "";
    }

    assert(0 == p[length]);

    return p;
}

// HACK.
void *
nullFail(void *p)
{
    if (!p) {exit(EXIT_FAILURE);}

    return p;
}

bool
validBox(Box b)
{
    return b.valid;
}

bool
saneLuaQuery(struct LuaQuery *const lua_query)
{
    return !!validBox(lua_query->thread)
       != !!(DEAD == lua_query->state || SELF_OWNING == lua_query->state);
}

// fails silently
// > not thread safe
void
writeQueryErrorToSecondaryLog(const char *const error)
{
    const char *const cryptdb_path = getenv("EDBDIR");
    if (NULL == cryptdb_path) {
        return;
    }

    char *const secondary_log_path =
        calloc(strlen(cryptdb_path) + 100, sizeof(char));
    if (NULL == secondary_log_path) {
        return;
    }

    if (0 > sprintf(secondary_log_path, "%s/logs/secondary.log", cryptdb_path)) {
        free(secondary_log_path);
        return;
    }

    FILE *const f = fopen(secondary_log_path, "a");
    if (NULL == f) {
        free(secondary_log_path);
        return;
    }

    time_t rawtime;
    char current_time[100] = {0};

    time (&rawtime);
    struct tm *const timeinfo = localtime(&rawtime);
    if (0 == strftime(current_time, 100, "%D  %T", timeinfo)) {
        fclose(f);
        free(secondary_log_path);
        return;
    }

    fprintf(f, "cryptdb error[%s]\n%s\n\n", current_time, error);
    fclose(f);
    free(secondary_log_path);
}

#include "threaded_query_tests.c"
