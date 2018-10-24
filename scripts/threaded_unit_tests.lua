local unit_lib =
    assert(package.loadlib("/home/burrows/code/cryptdb/scripts/threaded_query.so",
                           "lua_test_init"))
unit_lib()

TestThreadedQuery.all()
