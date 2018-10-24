-- script will likely require root privileges

local COLOR_END         = string.char(27) .. "[0m"
local GREEN             = string.char(27) .. "[1;92m"
local RED               = string.char(27) .. "[1;31m"
local PURPLE            = string.char(27) .. "[1;35m"

local main_lib =
    assert(package.loadlib("/home/burrows/code/cryptdb/scripts/threaded_query.so",
                           "lua_main_init"))
main_lib()

function main()
    if 0 ~= ThreadedQuery._geteuid() then
        print(RED .. "exiting: integration tests require root!" .. COLOR_END)
        return
    end
    --
    -- integration tests
    --
    -- > these tests have the potential to segfault if self owning threads
    --   don't finish cleanp before program exit.
    os.execute("service mysql stop")
    os.execute("mysqld --bind-address=127.0.0.1 &")
    os.execute("sleep 3")

    os.execute("mysql -uroot -pletmein -e \"drop database if exists lua_test\"")
    os.execute("mysql -uroot -pletmein -e \"create database if not exists lua_test\"")

    total_integrations      = 0
    passed_integrations     = 0

    total_integrations = total_integrations + 1
    if test_normalQueryExecution() then
        print(GREEN .. "test_normalQueryExecution succeeded!" .. COLOR_END)
        passed_integrations = passed_integrations + 1
    else
        print(RED .. "test_normalQueryExecution failed!" .. COLOR_END)
    end

    total_integrations = total_integrations + 1
    if test_failedQueryExecution() then
        print(GREEN .."test_failedQueryExecution succeeded!".. COLOR_END)
        passed_integrations = passed_integrations + 1
    else
        print(RED .. "test_failedQueryExecution failed!" .. COLOR_END)
    end

    total_integrations = total_integrations + 1
    if test_doubleQuery() then
        print(GREEN .."test_doubleQuery succeeded!".. COLOR_END)
        passed_integrations = passed_integrations + 1
    else
        print(RED .. "test_doubleQuery failed!" .. COLOR_END)
    end

    print("\n" ..
          "################################\n" ..
          "  Passed " .. passed_integrations .."/" .. total_integrations ..
                " Integration Tests\n" ..
          "################################\n")

    os.execute("mysql -uroot -pletmein -e \"drop database lua_test\"")
    os.execute("pkill -9 mysqld")
    os.execute("service mysql start")

    warnedSleep()
end

function warnedSleep()
    print(PURPLE .. "warning: integration tests require non-determinstic sleep!\n" ..
          COLOR_END)
    os.execute("sleep 5")
end

-- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
--             Integration Tests
-- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function test_normalQueryExecution()
    os.execute("mysql -uroot -pletmein -e \"create table lua_test.t (x integer, y integer)\"")
    os.execute("mysql -uroot -pletmein -e \"insert into lua_test.t VALUES (1, 2), (3, 4), (4, 3)\"")

    status, lua_query =
        ThreadedQuery.start("127.0.0.1", "root", "letmein", 3306, 2)
    if not (status and lua_query) then
        return false
    end

    -- do a query
    status = ThreadedQuery.query(lua_query, "SELECT * FROM lua_test.t")
    if not status then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status, result = ThreadedQuery.results(lua_query)
    if not (status and type(result) == "table") then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- do it again
    status = ThreadedQuery.query(lua_query, "SELECT * FROM lua_test.t")
    if not assert(status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status, result = ThreadedQuery.results(lua_query)
    if not (status and type(result) == "table") then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- different query
    status = ThreadedQuery.query(lua_query, "UPDATE lua_test.t SET x = 15")
    if not (status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status, result = ThreadedQuery.results(lua_query)
    if not (status and type(result) == "number") then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- issue a malformed query
    -- > optimization makes it still succeed initially
    status = ThreadedQuery.query(lua_query, "SELECT * FROM lua_test.tt")
    if not (status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status, result = ThreadedQuery.results(lua_query)
    if not (not status and result == nil) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status = ThreadedQuery.kill(lua_query)
    if not (status) then
        print("failed to kill threads; likely segfault!")
        return false
    end

    return true
end

function test_failedQueryExecution()
    os.execute("mysql -uroot -pletmein -e \"create table lua_test.t2 (x integer, y integer)\"")
    os.execute("mysql -uroot -pletmein -e \"insert into lua_test.t2 VALUES (1, 2), (3, 4), (4, 3)\"")

    os.execute("pkill -9 mysqld")
    -- should fail to connect
    status, lua_query =
        ThreadedQuery.start("127.0.0.1", "root", "letmein", 3306, 2)
    if not (status and lua_query) then
        return false
    end

    -- we're dead so do nothing
    status = ThreadedQuery.query(lua_query, "SELECT * FROM lua_test.t2")
    if not (not status) then
        return false
    end

    os.execute("mysqld --bind-address=127.0.0.1 &")
    os.execute("sleep 3")

    -- stay dead
    status, result = ThreadedQuery.results(lua_query)
    if not (not status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- night of the living dead
    status = ThreadedQuery.query(lua_query, "SELECT * FROM lua_test.t2")
    if not (not status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- dead dead dead
    status, result = ThreadedQuery.results(lua_query)
    if not (not status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- easy to kill dead things
    status = ThreadedQuery.kill(lua_query)
    if not (status) then
        print("failed to kill threads; expect segfault!")
        return false
    end

    return true
end

function test_doubleQuery()
    os.execute("mysql -uroot -pletmein -e \"create table lua_test.t3 (x integer, y integer)\"")
    os.execute("mysql -uroot -pletmein -e \"insert into lua_test.t3 VALUES (2, 12), (40, 15), (5, 38)\"")

    status, lua_query =
        ThreadedQuery.start("127.0.0.1", "root", "letmein", 3306, 2)
    if not (status and lua_query) then
        return false
    end

    status =
        ThreadedQuery.query(lua_query,
                            "INSERT INTO lua_test.t3 VALUES (1, 9)")
    if not (status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    -- issue another query to emulate commandHandler missing the
    -- RESULTS signal
    status =
        ThreadedQuery.query(lua_query, "SELECT * FROM lua_test.t3")
    if not (status) then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status, result = ThreadedQuery.results(lua_query)
    if not (status and type(result) == "table") then
        ThreadedQuery.kill(lua_query)
        return false
    end

    status = ThreadedQuery.kill(lua_query)
    if not (status) then
        print("failed to kill threads; likely segfault!")
        return false
    end

    return true
end

main()
