#pragma once

/*
 * test_utils.h
 *
 * Created on: Jul 18. 2011
 *   Author: cat_red
 */

#include <algorithm>
#include <string>
#include <assert.h>
#include <memory>

#include <util/util.hh>
#include <parser/sql_utils.hh>
#include <main/rewrite_main.hh>

typedef std::map<const std::string, const std::string> FieldOnionState;
typedef std::map<const std::string, FieldOnionState> TableOnionState;
typedef std::map<const std::string, TableOnionState> DBOnionState;

class TestConfig {
 public:
    TestConfig() {
        // default values
        user = "root";
        pass = "letmein";
        host = "localhost";
        db   = "cryptdbtest";
        shadowdb_dir = std::string(getenv("EDBDIR")) + "/shadow";
        port = 3306;
        stop_if_fail = false;

        // hack to find current dir
        char buf[1024];
        ssize_t n = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
        assert(n > 0);
        buf[n] = '\0';

        std::string s(buf, n);
        auto i = s.find_last_of('/');
        assert(i != s.npos);

        edbdir = s.substr(0, i) + "/..";
    }

    std::string user;
    std::string pass;
    std::string host;
    std::string db;
    std::string shadowdb_dir;
    uint port;

    bool stop_if_fail;

    std::string edbdir;
};

struct CrashPoint {
    std::string name;
    bool executed_query;

    CrashPoint(std::string namearg, bool eq)
        : name(namearg), executed_query(eq)
    {}
};

struct Query {
    std::string query;
    CrashPoint * crash_point;
    DBOnionState * onion_states;
    enum class WHERE_EXEC {TEST, CONTROL, BOTH};
    WHERE_EXEC where_exec;

    Query(const std::string &q, WHERE_EXEC where_exec=WHERE_EXEC::BOTH)
        : query(q), crash_point(NULL), onion_states(NULL),
          where_exec(where_exec) {}

    Query(const std::string &q, CrashPoint *const cp)
        : query(q), crash_point(cp), onion_states(NULL),
          where_exec(WHERE_EXEC::BOTH) {}

    Query(const std::string &q, DBOnionState *const os)
        : query(q), crash_point(NULL), onion_states(os),
          where_exec(WHERE_EXEC::BOTH) {}

    Query(const std::string &q, CrashPoint *const cp, DBOnionState *const os)
        : query(q), crash_point(cp), onion_states(os),
          where_exec(WHERE_EXEC::BOTH) {}
};

#define PLAIN 0

void PrintRes(const ResType &res);
void displayLoading(bool mode);

template <int N> ResType convert(std::string rows[][N], int num_rows);

//ResType myExecute(EDBProxy * cl, std::string query);

//ResType myCreate(EDBProxy * cl, std::string annotated_query, std::string plain_query);

static inline void
assert_res(const ResType &r, const char *msg)
{
    assert_s(r.ok, msg);
}

static inline bool
slowMatch(ResType res, ResType expected)
{
    /*
    if (res.names != expected.names
        || res.rows.size() != expected.rows.size()) {

        return false;
    }

    for (unsigned int row = 0; row < res.rows.size(); ++row) {
        bool matched = false;
        for (unsigned int exp_row = 0; exp_row < expected.rows.size();
                ++exp_row) {
            matched = true;
            for (unsigned int field = 0; field < res.rows[row].size();
                    ++field) {
                if (ItemToString(*res.rows.at(row).at(field)) != ItemToString(*expected.rows.at(exp_row).at(field))) {
                    matched = false;
                    break;
                }
            }
            if (true == matched) {
                expected.rows.erase(expected.rows.begin() + exp_row);
                break;
            }
        }

        if (false == matched) {
            return false;
        }
    }
    */

    return false;
}

static inline bool
match(const ResType &res, const ResType &expected)
{
    if (res.names != expected.names || res.rows.size() != expected.rows.size()) {
        return false;
    }
    for (unsigned int i = 0; i < res.rows.size(); i++) {
        for (unsigned int j = 0; j < res.rows.at(i).size(); j++) {
            if (ItemToString(*res.rows.at(i).at(j)) != ItemToString(*expected.rows.at(i).at(j))) {
                return slowMatch(res, expected);
            }
        }
    }
    return true;
}

static inline std::shared_ptr<Item>
sp(const std::string &s)
{
    return std::shared_ptr<Item>(make_item_string(s));
}

inline bool
testSlowMatch()
{
    /*
    const std::vector<std::shared_ptr<Item> > row0 =
        {sp("box"), sp("rocks"), sp("candy")};
    const std::vector<std::shared_ptr<Item> > row1 =
        {sp("box"), sp("noodles"), sp("candy")};
    const std::vector<std::shared_ptr<Item> > row2 =
        {sp("box"), sp("candy"), sp("rocks")};
    const std::vector<std::string> fields({"red", "green", "black"});

    ResType expected0(true);
    ResType res0(true);
    res0.names = expected0.names = fields;
    res0.rows = expected0.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row0, row1, row2});

    ResType expected1(true);
    ResType res1(true);
    res1.names = expected1.names = fields;
    res1.rows = expected1.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row0, row1, row0});

    ResType expected2(true);
    ResType res2(true);
    res2.names = expected2.names = fields;
    expected2.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row0, row1, row0});
    res2.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row0, row1, row1});

    ResType expected3(true);
    ResType res3(true);
    res3.names = expected3.names = fields;
    expected3.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row0, row1, row2});
    res3.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row2, row1, row0});

    ResType expected4(true);
    ResType res4(true);
    res4.names = expected4.names = fields;
    expected4.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row0, row1, row1});
    res4.rows =
        std::vector<std::vector<std::shared_ptr<Item> > > ({
            row1, row1, row1});

    return true == slowMatch(res0, expected0) &&
           true == slowMatch(expected0, res0) &&
           true == slowMatch(res1, expected1) &&
           true == slowMatch(expected1, res1) &&
           false == slowMatch(res2, expected2) &&
           false == slowMatch(expected2, res2) &&
           true == slowMatch(res3, expected3) &&
           true == slowMatch(expected3, res3) &&
           false == slowMatch(res4, expected4) &&
           false == slowMatch(expected4, res4);
    */

    return false;
}
