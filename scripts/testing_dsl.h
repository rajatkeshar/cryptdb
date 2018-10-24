#define COLOR_END       "\033[0m"
#define GREEN_BEGIN     "\033[1;92m"
#define RED_BEGIN       "\033[1;31m"
#define PURPLE_BEGIN    "\033[1;35m"

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//             Login Credentials
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
const char *const real_host         = "127.0.0.1";
const char *const real_user         = "root";
const char *const real_passwd       = "letmein";
const unsigned int real_port        = 3306;

const char *const fast_bad_host     = "127.0.0.1";
const char *const slow_bad_host     = "google.com";

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//            Simple Testing DSL
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define MAX_SELF_OWNING_THREADS 100
static pthread_t __self_owning_threads[MAX_SELF_OWNING_THREADS];
static unsigned __self_owning_threads_counter   = 0;

static unsigned __total_tests                   = 0;
static unsigned __passed_tests                  = 0;

static unsigned __total_asserts                 = 0;
static unsigned __passed_asserts                = 0;

#define UGLY_SLEEP                                                  \
{                                                                   \
    fprintf(stderr, "\t\t%swarning: test requires"                  \
                    " non-deterministic sleep!%s\n",                \
            PURPLE_BEGIN, COLOR_END);                               \
    system("sleep 2");                                              \
}

#define NON_DETERMINISM                                             \
{                                                                   \
    fprintf(stderr, "\t\t%swarning: test does something"            \
                    " non-deterministic!%s\n",                      \
            PURPLE_BEGIN, COLOR_END);                               \
}

#define TEST_SUCCESS                                \
{                                                   \
   printf("%s%s succeeded!%s\n", GREEN_BEGIN,       \
                                (__test_name),      \
                                COLOR_END);         \
}

#define TEST_FAILURE                                \
{                                                   \
   printf("%s%s failed!%s\n", RED_BEGIN,            \
                              (__test_name),        \
                              COLOR_END);           \
}

#define TEST_ASSERT(expr)                           \
{                                                   \
    ++__total_asserts;                              \
    if (!(expr)) {                                  \
        TEST_FAILURE                                \
        return 0;                                   \
    }                                               \
    ++__passed_asserts;                             \
}

#define TEST(name)                                  \
    static int                                      \
    name(struct lua_State *const L)                 \
    {                                               \
        global_timeouts = 0;                        \
        const char *const __test_name = #name;      \
        ++__total_tests;

#define TEST_SUPPLEMENT(name)                       \
    const char *const __test_name = "$supplementing$ "#name;

#define END_TEST                                    \
        TEST_SUCCESS                                \
        ++__passed_tests;                           \
    }

#define POSSIBLE_SELF_OWNING_THREAD(thread)                             \
{                                                                       \
    assert(__self_owning_threads_counter < MAX_SELF_OWNING_THREADS);    \
    __self_owning_threads[__self_owning_threads_counter++] = (thread);  \
}

static void
waitForSelfOwningThreads()
{
    printf("%d possible self owning threads!\n",
            __self_owning_threads_counter);
    // wait for possibly self owning threads to exit.
    size_t i = 0;
    for (; i < __self_owning_threads_counter; ++i) {
        struct timespec ts;
        assert(!clock_gettime(CLOCK_REALTIME, &ts));

        ts.tv_sec += 30;

        void *exit_code;
        pthread_timedjoin_np(__self_owning_threads[i], &exit_code, &ts);
    }

    printf("\n### done waiting on threads! ###\n");
}

static void
printTestStats()
{
    printf("\n"
           "################################\n"
           "     Passed %d/%d Unit Tests\n"
           "       %d/%d Assertions\n"
           "################################\n\n",
           __passed_tests, __total_tests,
           __passed_asserts, __total_asserts);
}

