-- This code is ugly; but it is largely an attempt to deal with disparate
-- datastructures that are essentially isomorphic with a minimal amount
-- of looping.

-- GLOBAL STATE
local new_session       = true            -- wordpress testing hack
local result_set_count  = nil
local result_set_index  = nil

package.cpath = package.cpath .. ";/usr/local/lib/lua/5.1/?.so"
package.path  = package.path .. ";/usr/local/share/lua/5.1/?.lua"

local CRYPTDB_DIR = os.getenv("EDBDIR")

local threaded_query =
    assert(package.loadlib(CRYPTDB_DIR .. "/scripts/threaded_query.so",
                           "lua_main_init"))
threaded_query()

local proto  = assert(require("mysql.proto"))

local LOG_FILE_PATH      = CRYPTDB_DIR .. "/logs/double.log"
local tquery             = nil
local log_file_h         = nil

local RESULTS_QUEUE      = "results"
local QUERY_QUEUE        = "query"

-- special messages sent on the RESULTS_QUEUE cannot be nil, tables or
-- numbers.
local CRYPTDB_IS_DEAD    = "cryptdb is dead"

function connect_server()
    print("Double Connection.")

    -- initialize and start pre-emptive thread
    status, tquery =
        ThreadedQuery.start("127.0.0.1", "root", "letmein", 3307, 5)
    if nil == status then
        tquery = nil
    end

    -- open log file
    log_file_h = assert(io.open(LOG_FILE_PATH, "a"))
end

function disconnect_client()
    if log_file_h then
        log_file_h:close()
    end

    if tquery then
        assert(ThreadedQuery.kill(tquery))
    end
end

function read_query(packet)
    result_set_count = 0
    result_set_index = 0

    local query = string.sub(packet, 2)

    -- bad queries don't properly trigger read_query_result
    if not valid_packet(packet) then
        return nil
    end

    -- HACK: turns off strict mode for wordpress because
    -- the testing database runs in strict mode.
    -- > Don't send to CryptDB.
    if true == new_session then
        mode = "SET @@session.sql_mode := ''"
        proxy.queries:append(1337, string.char(proxy.COM_QUERY) .. mode,
                             {resultset_is_needed = true})
        new_session = false
        result_set_count = result_set_count + 1
    end

    -- forward the query as is to the regular database
    proxy.queries:append(42, packet, {resultset_is_needed = true})
    result_set_count = result_set_count + 1

    -- build the query for cryptdb
    if string.byte(packet) == proxy.COM_INIT_DB then
        cryptdb_query = "USE `" .. query .. "`"
    else
        cryptdb_query = query
    end

    -- Send the new query
    if tquery then
        ThreadedQuery.query(tquery, cryptdb_query);
    end

    return proxy.PROXY_SEND_QUERY
end

function read_query_result(inj)
    result_set_index = result_set_index + 1
    if result_set_index < result_set_count then
        return proxy.PROXY_IGNORE_RESULT
    end

    local client_name = proxy.connection.client.src.name
    local query = string.sub(inj.query, 2)
    local out_status = nil

    -- > somemtimes this is a table (ie SELECT), sometimes it's nil (query
    --   error), sometimes it's number of rows affected by command
    if result_set_count == result_set_index then
        if tquery then
            status, cryptdb_results = ThreadedQuery.results(tquery)
        else
            status, cryptdb_results = nil, {}
        end
    else
        status = true
        cryptdb_results = {}
        return
    end

    local cryptdb_error = not status
    local regular_error = proxy.MYSQLD_PACKET_ERR ==
                                inj.resultset.query_status

    if regular_error or cryptdb_error then
        out_status = "error"
    elseif "number" == type(cryptdb_results) then
        -- WARN: this is always going to give a nonsensical result
        -- for UPDATE and DDL queries.
        out_status =
            get_match_text(cryptdb_results == inj.resultset.affected_rows)
    elseif not inj.resultset or not inj.resultset.rows then
        out_status = "no plaintext data"
    else
        -- do the naive comparison while gathering regular results,
        -- then if it fails, do the slow comparison
        local regular_results = {}

        -- HACK/CARE: double iteration
        local index = 1
        local no_fast_match = false
        for regular_row in inj.resultset.rows do
            if false == no_fast_match then
                -- don't do table_test if we already know we dont have
                -- a fast match
                if false == table_test(cryptdb_results[index], regular_row) then
                    no_fast_match = true
                end
            end

            regular_results[index] = regular_row
            index = index + 1
        end

        if #regular_results ~= #cryptdb_results then
            out_status = get_match_text(false)
        elseif false == no_fast_match then
            out_status = get_match_text(true)
        else
            -- do slow, unordered matching
            local test = slow_test(regular_results, cryptdb_results)
            out_status = get_match_text(test)
        end
    end

    create_log_entry(log_file_h, client_name, query, cryptdb_error,
                     regular_error, out_status)

    return
end

function create_log_entry(file, client, query, cryptdb_error,
                          regular_error, status)
    if file then
        file:write(client .. "," .. csv_escape(query) .. "," .. os.date("%c") .. "," .. ppbool(cryptdb_error) .. "," .. ppbool(regular_error) .. "," .. status .. "\n")
        file:flush()
    end
end

function table_test(results_a, results_b)
    if type(results_a) == "nil" or type(results_b) == "nil" then
        return false
    end

    if table.getn(results_a) ~= table.getn(results_b) then
        return false
    end

    for i = 1, #results_a do
        if results_a[i] ~= results_b[i] then
            return false
        end
    end

    return true
end

-- FIXME: remove matched elements
function slow_test(results_a, results_b)
    if table.getn(results_a) ~= table.getn(results_b) then
        return false
    end

    for a_index = 1, #results_a do
        local matched = false
        for b_index = 1, #results_b do
            if table_test(results_a[a_index], results_b[b_index]) then
                matched = true
                break
            end
        end

        if false == matched then
            return false
        end
    end

    return true
end

function get_match_text(b)
    if true == b then
        return "matched"
    else
        return "diverged"
    end
end

function ppbool(b)
    if true == b then
        return "true"
    else
        return "false"
    end
end

function valid_packet(packet)
    if not packet:match("[%w%p]+") then
        return false
    end

    local opcode = string.byte(packet)
    if opcode ~= proxy.COM_INIT_DB and opcode ~= proxy.COM_QUERY then
        return false;
    end

    return true
end

-- FIXME: Implement this if it actually matters; will make code slower.
-- VAPORWARE
function csv_escape(string)
    return string
end
