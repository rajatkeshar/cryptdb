#include "testing_dsl.h"

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//                Unit Tests
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TEST(test_pushvalue)
    const char *s0 = "hello";
    const char *s1 = "world\0w0rld"; unsigned int s1_len = 11;
    const char *s2 = "paradise";
    pushvalue(L, s0, strlen(s0));
    pushvalue(L, NULL, -1);
    pushvalue(L, s1, s1_len);
    pushvalue(L, s1, strlen(s1));
    pushvalue(L, NULL, 1);
    pushvalue(L, s2, strlen(s2));

    TEST_ASSERT(!strcmp(s2, lua_tolstring(L, -1, NULL)));
    TEST_ASSERT(NULL == lua_tolstring(L, -2, NULL));
    TEST_ASSERT(!strcmp(s1, lua_tolstring(L, -3, NULL)));
    TEST_ASSERT(!memcmp(s1, lua_tolstring(L, -4, NULL), s1_len));
    TEST_ASSERT(NULL == lua_tolstring(L, -5, NULL));
    TEST_ASSERT(!strcmp(s0, lua_tolstring(L, -6, NULL)));
END_TEST

TEST(test_luaToCharp)
    const unsigned int number = 10;
    const char *const s0      = "red";
    const char *const s1      = "green\0purple"; unsigned s1_len = 11;
    const char *const s2      = "blue";

    lua_pushnil(L);
    lua_pushstring(L, s0);
    lua_pushlstring(L, s1, s1_len);
    lua_pushstring(L, s1);
    lua_pushnumber(L, number);

    lua_pushstring(L, s2);
    TEST_ASSERT(!strcmp(luaToCharp(L, -1), s2));

    lua_pushstring(L, s0);
    TEST_ASSERT(!strcmp(luaToCharp(L, -1), s0));

    TEST_ASSERT(number == lua_tointeger(L, -3));
    TEST_ASSERT(!strcmp(luaToCharp(L, -4), s1));
    TEST_ASSERT(!memcmp(luaToCharp(L, -5), s1, s1_len));
    TEST_ASSERT(!strcmp(luaToCharp(L, -6), s0));
    TEST_ASSERT(!strcmp(luaToCharp(L, -7), ""));
END_TEST

TEST(test_waitForCommand)
    struct LuaQuery lua_query;
    lua_query.persist.ell = (void *)0x01;
    lua_query.command_ready = true;
    lua_query.mutex = NEW_BOX;
    waitForCommand(&lua_query);
END_TEST

static void *
waitThread(void *plq)
{
    TEST_SUPPLEMENT(test_completeLuaQuery);

    struct LuaQuery **p_lua_query = (struct LuaQuery **)plq;
    TEST_ASSERT(p_lua_query && *p_lua_query);

    // Prepare to receive a signal from completeLuaQuery
    TEST_ASSERT(!pthread_mutex_trylock(MUTEX_ADDR((*p_lua_query)->mutex)));

    while (false == (*p_lua_query)->completion_signal) {
        TEST_ASSERT(!pthread_cond_wait(COND_ADDR((*p_lua_query)->cond),
                                       MUTEX_ADDR((*p_lua_query)->mutex)));
    }

    TEST_ASSERT(!pthread_mutex_unlock(MUTEX_ADDR((*p_lua_query)->mutex)));
    return (void *)true;
}

TEST(test_completeLuaQuery)
    const char *const init_host     = strdup(fast_bad_host);
    const char *const init_user     = strdup("skyscraper");
    const char *const init_passwd   = strdup("windows");
    const unsigned int init_port    = 0x7656;

    // emulate createLuaQuery without actually starting a thread
    struct LuaQuery **p_lua_query = malloc(sizeof(struct LuaQuery *));
    TEST_ASSERT(p_lua_query);

    *p_lua_query = malloc(sizeof(struct LuaQuery));
    TEST_ASSERT(*p_lua_query);

    initLuaQuery(*p_lua_query);

    struct HostData *host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data);

    const unsigned int init_wait    = 1;
    (*p_lua_query)->persist.host_data   = host_data;
    (*p_lua_query)->persist.ell         = NULL;
    (*p_lua_query)->persist.wait        = init_wait;

    TEST_ASSERT(!pthread_mutex_init(MUTEX_ADDR((*p_lua_query)->mutex),
                                               NULL));
    TEST_ASSERT(validBox((*p_lua_query)->mutex));
    TEST_ASSERT(!pthread_cond_init(COND_ADDR((*p_lua_query)->cond),
                                              NULL));
    TEST_ASSERT(validBox((*p_lua_query)->cond));

    // prepare a command
    (*p_lua_query)->persist.ell     = L;
    (*p_lua_query)->command         = RESULTS;

    // Start a thread that will receive the signal from this one
    pthread_t thd;
    TEST_ASSERT(!pthread_create(&thd, NULL, waitThread, p_lua_query));

    (*p_lua_query)->command_ready   = true;

    const unsigned init_output_count = 4;
    completeLuaQuery(*p_lua_query, init_output_count);
    UGLY_SLEEP

    void *ret;
    TEST_ASSERT(!pthread_tryjoin_np(thd, &ret));
    TEST_ASSERT(true == (bool)ret);
END_TEST

TEST(test_newLuaQuery)
    struct LuaQuery *const lua_query = malloc(sizeof(struct LuaQuery));
    memset(lua_query, 0xDEADBEEF, sizeof(struct LuaQuery));

    const char *const init_host     = strdup(fast_bad_host);
    const char *const init_user     = strdup("martianinvader");
    const char *const init_passwd   = strdup("lophtcrak");
    const unsigned int init_port    = 0x1111;

    struct HostData *host_data = malloc(sizeof(struct HostData));
    TEST_ASSERT(host_data);
    *host_data = (struct HostData){
        .host       = init_host,
        .user       = init_user,
        .passwd     = init_passwd,
        .port       = init_port
    };

    const unsigned int init_wait    = 1;

    TEST_ASSERT(newLuaQuery(lua_query, host_data, init_wait));

    TEST_ASSERT(-1                   == lua_query->command);
    TEST_ASSERT(false                == lua_query->command_ready);
    TEST_ASSERT(false                == lua_query->completion_signal);
    TEST_ASSERT(true                 == validBox(lua_query->thread));
    TEST_ASSERT(host_data            == lua_query->persist.host_data);
    TEST_ASSERT(NULL                 == lua_query->persist.ell);
    TEST_ASSERT(init_wait            == lua_query->persist.wait);
    TEST_ASSERT(NULL                 == lua_query->sql);
    TEST_ASSERT(-1                   == lua_query->output_count);
    TEST_ASSERT(false                == lua_query->mysql_connected);

    pthread_t thd = THD(lua_query->thread);
    // the thread is using data allocated from this thread and will
    // segfault and shutdown if we do not manually kill.
    TEST_ASSERT(true == niceStopLuaQueryThread(lua_query));
END_TEST

TEST(test_createLuaQuery)
    const char *const init_host     = strdup(fast_bad_host);
    const char *const init_user     = strdup("rainbow");
    const char *const init_passwd   = strdup("jetplane");
    const unsigned int init_port    = 0x2222;

    struct HostData *host_data = malloc(sizeof(struct HostData));
    TEST_ASSERT(host_data);
    *host_data = (struct HostData){
        .host       = init_host,
        .user       = init_user,
        .passwd     = init_passwd,
        .port       = init_port
    };
    struct HostData *const orig_host_data = host_data;

    const unsigned int init_wait    = 1;

    struct LuaQuery **const p_lua_query =
        createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query);
    struct LuaQuery *const lua_query = *p_lua_query;

    TEST_ASSERT(-1                   == lua_query->command);
    TEST_ASSERT(false                == lua_query->command_ready);
    TEST_ASSERT(false                == lua_query->completion_signal);
    TEST_ASSERT(true                 == validBox(lua_query->thread));
    TEST_ASSERT(orig_host_data       == lua_query->persist.host_data);
    TEST_ASSERT(NULL                 == lua_query->persist.ell);
    TEST_ASSERT(init_wait            == lua_query->persist.wait);
    TEST_ASSERT(NULL                 == lua_query->sql);
    TEST_ASSERT(-1                   == lua_query->output_count);
    TEST_ASSERT(false                == lua_query->mysql_connected);

    TEST_ASSERT(NULL                 == host_data);
    TEST_ASSERT(orig_host_data->host ==
                    lua_query->persist.host_data->host);
    TEST_ASSERT(orig_host_data->user ==
                    lua_query->persist.host_data->user);
    TEST_ASSERT(orig_host_data->passwd ==
                    lua_query->persist.host_data->passwd);
    TEST_ASSERT(orig_host_data->port ==
                    lua_query->persist.host_data->port);

    TEST_ASSERT(true == niceStopLuaQueryThread(lua_query));
    // this threads memory will be reclaimed and it will do bad things
    // if the other thread is self owning
END_TEST

TEST(test_lowLevelStopLuaQueryThread)
    struct LuaQuery lua_query, test_lua_query;
    memset(&lua_query, 0xFEFEFEFE, sizeof(struct LuaQuery));
    memcpy(&test_lua_query, &lua_query, sizeof(struct LuaQuery));

    test_lua_query.mysql_connected = lua_query.mysql_connected = true;
    size_t alloc_size = 20;
    test_lua_query.thread = lua_query.thread = NEW_BOX;
    test_lua_query.persist.wait = lua_query.persist.wait = 1;

    TEST_ASSERT(NOTHING_TO_STOP == lowLevelStopLuaQueryThread(&lua_query));
    TEST_ASSERT(!memcmp(&lua_query, &test_lua_query,
                        sizeof(struct LuaQuery)));
END_TEST

TEST(test_lowLevelStopLuaQueryThread2)
    const char *const init_host     = strdup(fast_bad_host);
    const char *const init_user     = strdup("buildings");
    const char *const init_passwd   = strdup("cars");
    const unsigned int init_port    = 0x3333;

    struct HostData *host_data = malloc(sizeof(struct HostData));
    *host_data = (struct HostData){
        .host       = init_host,
        .user       = init_user,
        .passwd     = init_passwd,
        .port       = init_port
    };

    const unsigned int init_wait = 1;
    struct LuaQuery **const p_lua_query =
        createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query && *p_lua_query);
    TEST_ASSERT(NULL        == host_data);

    struct LuaQuery *const test_lua_query =
        malloc(sizeof(struct LuaQuery));
    TEST_ASSERT(test_lua_query);
    memcpy(test_lua_query, *p_lua_query, sizeof(struct LuaQuery));

    TEST_ASSERT(FAILURE != lowLevelStopLuaQueryThread(*p_lua_query));

    // test will be impossible if the self owning thread _actually_ cleans
    // itself up
    test_lua_query->thread = (*p_lua_query)->thread;
    TEST_ASSERT(!memcmp(*p_lua_query, test_lua_query,
                        sizeof(struct LuaQuery)));
END_TEST

TEST(test_destroyLuaQuery)
    struct HostData *host_data =
        createHostData(fast_bad_host, "else", "isunderthebed!", 199);
    TEST_ASSERT(host_data);

    const unsigned int init_wait = 1;
    struct LuaQuery **p_lua_query = createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query && *p_lua_query);

    destroyLuaQuery(&p_lua_query);

    TEST_ASSERT(NULL == p_lua_query);
END_TEST

void *
dummyThread(void *const unused)
{
    assert(!pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL));
    assert(!pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL));

    while (1);
}

TEST(test_badIssueCommand)
    const char *const init_host     = fast_bad_host;
    const char *const init_user     = "going";
    const char *const init_passwd   = "tophail?";
    const unsigned int init_port    = 0xEEEE;

    struct HostData *host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data);

    const unsigned int init_wait    = 1;
    struct LuaQuery **p_lua_query = createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query && *p_lua_query);

    // soft kill should fail, then hard kill should succeed
    (*p_lua_query)->mysql_connected = true;
    TEST_ASSERT(!pthread_create(THD_ADDR((*p_lua_query)->thread),
                                NULL, dummyThread, NULL));
    issueCommand(L, KILL, p_lua_query);
    TEST_ASSERT(false          == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(false          == validBox((*p_lua_query)->thread));
    TEST_ASSERT(DEAD                    == (*p_lua_query)->state);
    TEST_ASSERT(false                   == (*p_lua_query)->command_ready);
    TEST_ASSERT(false                == (*p_lua_query)->completion_signal);
    TEST_ASSERT(COMMAND_OUTPUT_COUNT    == (*p_lua_query)->output_count);
    TEST_ASSERT(true                  == (*p_lua_query)->mysql_connected);
    TEST_ASSERT(NULL                    == (*p_lua_query)->persist.ell);
    TEST_ASSERT(init_wait               == (*p_lua_query)->persist.wait);

    issueQuery(L, "SELECT SOMETHING", p_lua_query);
    TEST_ASSERT(false      == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(false      == validBox((*p_lua_query)->thread));
    TEST_ASSERT(COMMAND_OUTPUT_COUNT  == (*p_lua_query)->output_count);
    TEST_ASSERT(true      == (*p_lua_query)->mysql_connected);
    TEST_ASSERT(NULL       == (*p_lua_query)->persist.ell);
    TEST_ASSERT(init_wait  == (*p_lua_query)->persist.wait);

    TEST_ASSERT(NOTHING_TO_STOP
                           == lowLevelStopLuaQueryThread(*p_lua_query));
    TEST_ASSERT(false      == validBox((*p_lua_query)->thread));
    TEST_ASSERT(COMMAND_OUTPUT_COUNT  == (*p_lua_query)->output_count);
    TEST_ASSERT(true      == (*p_lua_query)->mysql_connected);

    TEST_ASSERT(!strcmp(init_host,
                        (*p_lua_query)->persist.host_data->host));
    TEST_ASSERT(!strcmp(init_user,
                        (*p_lua_query)->persist.host_data->user));
    TEST_ASSERT(!strcmp(init_passwd,
                        (*p_lua_query)->persist.host_data->passwd));
    TEST_ASSERT(init_port == (*p_lua_query)->persist.host_data->port);
END_TEST

TEST(test_noReconnectAfterFailure)
    const char *const init_host     = slow_bad_host;
    const char *const init_user     = real_user;
    const char *const init_passwd   = real_passwd;
    const unsigned int init_port    = real_port;

    struct HostData *bad_host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(bad_host_data);

    const unsigned int init_wait    = 1;
    struct LuaQuery **p_bad_lua_query =
        createLuaQuery(&bad_host_data, init_wait);
    TEST_ASSERT(p_bad_lua_query && *p_bad_lua_query);

    issueQuery(L, "do 0", p_bad_lua_query);
    TEST_ASSERT(false         == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(false         == validBox((*p_bad_lua_query)->thread));
    TEST_ASSERT(COMMAND_OUTPUT_COUNT == (*p_bad_lua_query)->output_count);
    TEST_ASSERT(false         == (*p_bad_lua_query)->mysql_connected);
    TEST_ASSERT(NULL                 == (*p_bad_lua_query)->persist.ell);
    TEST_ASSERT(init_wait            == (*p_bad_lua_query)->persist.wait);

    struct HostData *good_host_data =
        createHostData(real_host, real_user, real_passwd, real_port);
    TEST_ASSERT(good_host_data);

    struct LuaQuery *good_lua_query = malloc(sizeof(struct LuaQuery));
    TEST_ASSERT(good_lua_query);
    memcpy(good_lua_query, *p_bad_lua_query, sizeof(struct LuaQuery));
    good_lua_query->persist.host_data = good_host_data;

    issueCommand(L, RESULTS, &good_lua_query);

    TEST_ASSERT(false          == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(false                == validBox(good_lua_query->thread));
    TEST_ASSERT(COMMAND_OUTPUT_COUNT == good_lua_query->output_count);
    TEST_ASSERT(false                == good_lua_query->mysql_connected);
    TEST_ASSERT(NULL                 == good_lua_query->persist.ell);
    TEST_ASSERT(init_wait            == good_lua_query->persist.wait);
END_TEST

TEST(test_condVar)
    const char *const init_host     = real_host;
    const char *const init_user     = real_user;
    const char *const init_passwd   = real_passwd;
    const unsigned int init_port    = real_port;

    struct HostData *host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data);

    const unsigned init_wait = 1;
    struct LuaQuery **p_lua_query = createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query && *p_lua_query);

    issueCommand(L, RESULTS, p_lua_query);
    TEST_ASSERT(false          == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(true                 == validBox((*p_lua_query)->thread));
    TEST_ASSERT(COMMAND_OUTPUT_COUNT == (*p_lua_query)->output_count);
    TEST_ASSERT(true                 == (*p_lua_query)->mysql_connected);
    TEST_ASSERT(NULL                 == (*p_lua_query)->persist.ell);
    TEST_ASSERT(init_wait            == (*p_lua_query)->persist.wait);

    TEST_ASSERT(SUCCESS == lowLevelStopLuaQueryThread(*p_lua_query));
    TEST_ASSERT(false   == validBox((*p_lua_query)->thread));
END_TEST

TEST(test_nonResponsiveRemote)
    global_timeouts = 0;
    TEST_ASSERT(false == nonResponsiveRemote());
    global_timeouts = MAX_TIMEOUTS;
    TEST_ASSERT(true  == nonResponsiveRemote());
END_TEST

TEST(test_createHostData)
    const char *const init_host     = "birdsandplains";
    const char *const init_user     = "automobiles";
    const char *const init_passwd   = "sendandreceive";
    const unsigned int init_port    = 0x9898;

    const struct HostData test_host_data = {
        .host           = init_host,
        .user           = init_user,
        .passwd         = init_passwd,
        .port           = init_port
    };

    struct HostData *const host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data);

    TEST_ASSERT(!strcmp(test_host_data.host, host_data->host));
    TEST_ASSERT(!strcmp(test_host_data.user, host_data->user));
    TEST_ASSERT(!strcmp(test_host_data.passwd, host_data->passwd));
    TEST_ASSERT(test_host_data.port == host_data->port);
    TEST_ASSERT(init_port           == host_data->port);

    // make sure createHostData didn't mutate values it doesn't own
    TEST_ASSERT(init_host    != host_data->host);
    TEST_ASSERT(init_user    != host_data->user);
    TEST_ASSERT(init_passwd  != host_data->passwd);

    TEST_ASSERT(!strcmp(test_host_data.host, init_host));
    TEST_ASSERT(!strcmp(test_host_data.user, init_user));
    TEST_ASSERT(!strcmp(test_host_data.passwd, init_passwd));
END_TEST

TEST(test_destroyHostData)
    struct HostData *host_data =
        createHostData("some", "data", "willdo", 1005);
    TEST_ASSERT(host_data);

    destroyHostData(&host_data);
    TEST_ASSERT(NULL == host_data);
END_TEST

TEST(test_killThreadOwningMutex)
    const char *const init_host         = real_host;
    const char *const init_user         = real_user;
    const char *const init_passwd       = real_passwd;
    const unsigned int init_port        = real_port;

    struct HostData *host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data);

    const unsigned int init_wait        = 1;
    struct LuaQuery **p_lua_query = createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query && *p_lua_query);

    destroyLuaQuery(&p_lua_query);
    TEST_ASSERT(NULL == p_lua_query);
END_TEST

TEST(test_noMoreConnects)
    const char *const init_host         = real_host;
    const char *const init_user         = real_user;
    const char *const init_passwd       = real_passwd;
    const unsigned int init_port        = real_port;

    struct HostData *host_data =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data);

    const unsigned int init_wait        = 1;
    struct LuaQuery **p_lua_query = createLuaQuery(&host_data, init_wait);
    TEST_ASSERT(p_lua_query && *p_lua_query);

    // do a query; the thread is operational
    issueQuery(L, "SHOW DATABASES", p_lua_query);
    TEST_ASSERT(true           == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(true           == validBox((*p_lua_query)->thread));
    TEST_ASSERT(NORMAL         == (*p_lua_query)->state);

    // kill the thread to emulate it being non-responsive
    assert(!pthread_cancel(THD((*p_lua_query)->thread)));

    // try to get your results and fail
    issueCommand(L, RESULTS, p_lua_query);
    TEST_ASSERT(false          == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(false          == validBox((*p_lua_query)->thread));
    TEST_ASSERT(DEAD           == (*p_lua_query)->state);

    // clean it up
    destroyLuaQuery(&p_lua_query);
    TEST_ASSERT(NULL           == p_lua_query);

    // start another thread
    struct HostData *host_data2 =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data2);

    struct LuaQuery **p_lua_query2 =
        createLuaQuery(&host_data2, init_wait);
    TEST_ASSERT(p_lua_query2 && *p_lua_query2);

    // this thread should also be functional
    issueQuery(L, "SHOW DATABASES", p_lua_query2);
    TEST_ASSERT(true           == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(true           == validBox((*p_lua_query2)->thread));
    TEST_ASSERT(NORMAL         == (*p_lua_query2)->state);

    issueCommand(L, RESULTS, p_lua_query2);
    TEST_ASSERT(true           == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(true           == validBox((*p_lua_query2)->thread));
    TEST_ASSERT(NORMAL         == (*p_lua_query2)->state);

    // now kill it
    issueCommand(L, KILL, p_lua_query2);
    TEST_ASSERT(true           == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    // issueCommand does not update thread state metadata unless it has
    // to hard kill the thread
    TEST_ASSERT(true           == validBox((*p_lua_query2)->thread));
    TEST_ASSERT(NORMAL         == (*p_lua_query2)->state);

    // now start another connection
    struct HostData *host_data3 =
        createHostData(init_host, init_user, init_passwd, init_port);
    TEST_ASSERT(host_data3);

    struct LuaQuery **p_lua_query3 =
        createLuaQuery(&host_data3, init_wait);
    TEST_ASSERT(p_lua_query3 && *p_lua_query3);

    // fudge the global timeouts so that it shouldn't try to query
    global_timeouts = MAX_TIMEOUTS;
    issueQuery(L, "SHOW DATABASES", p_lua_query3);
    TEST_ASSERT(false          == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(true           == validBox((*p_lua_query3)->thread));
    TEST_ASSERT(NORMAL         == (*p_lua_query3)->state);

    // kill should fail as well
    issueCommand(L, KILL, p_lua_query3);
    TEST_ASSERT(false          == lua_toboolean(L, -COMMAND_OUTPUT_COUNT));
    TEST_ASSERT(true           == validBox((*p_lua_query3)->thread));
    TEST_ASSERT(NORMAL         == (*p_lua_query3)->state);

    // now clean it up
    TEST_ASSERT(true           == niceStopLuaQueryThread(*p_lua_query3));
    destroyLuaQuery(&p_lua_query3);
    TEST_ASSERT(NULL           == p_lua_query3);

    TEST_ASSERT(true           == nonResponsiveRemote());
END_TEST

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//                 Helpers
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// HACK: this function does not return
static int
all(struct lua_State *const L)
{
    // tests
    test_pushvalue(L);
    test_luaToCharp(L);
    test_waitForCommand(L);
    test_completeLuaQuery(L);
    test_newLuaQuery(L);
    test_createLuaQuery(L);
    test_lowLevelStopLuaQueryThread(L);
    test_lowLevelStopLuaQueryThread2(L);
    test_destroyLuaQuery(L);
    test_badIssueCommand(L);
    test_noReconnectAfterFailure(L);
    test_condVar(L);
    test_nonResponsiveRemote(L);
    test_createHostData(L);
    test_destroyHostData(L);
    test_killThreadOwningMutex(L);
    test_noMoreConnects(L);

    // bookkeeping
    printTestStats();
    // waitForSelfOwningThreads();

    printf("#### waiting for detached threads ####\n");
    // this gives our detached threads a chance to finish
    pthread_exit(0);
}


// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//                 Exports
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

static const struct luaL_reg
test_lib[] = {
#define F(n) { #n, n }
    F(test_pushvalue),
    F(all),
    {0, 0},
};

extern int
lua_test_init(lua_State *const L)
{
    luaL_openlib(L, "TestThreadedQuery", test_lib, 0);

    return 1;
}


