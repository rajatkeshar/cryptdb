/*
 * TestQueries.cc
 *  -- end to end query and result test, independant of connection process
 */

#include <algorithm>
#include <stdexcept>
#include <netinet/in.h>

#include <util/errstream.hh>
#include <util/cleanup.hh>
#include <util/cryptdb_log.hh>
#include <main/rewrite_main.hh>

#include <test/TestQueries.hh>


static test_mode control_type;
static test_mode test_type;
static uint64_t no_conn = 1;
static Connection * control;
static Connection * test;


static FieldOnionState num_os =
    {{"oEq", "RND"}, {"oOrder", "RND"}, {"oADD", "HOM"}, {"oPLAIN", "RND"}};
static FieldOnionState str_os =
    {{"oEq", "RND"}, {"oOrder", "RND"}, {"oPLAIN", "RND"}};
static DBOnionState insert_os =
    {{"test_insert", {{"id", num_os}, {"age", num_os}, {"salary", num_os}, {"address", str_os}, {"name", str_os}}}};

static QueryList Insert = QueryList("SingleInsert",
    { Query("CREATE TABLE test_insert (id integer , age integer, salary integer, address text, name text)"),
      Query("INSERT INTO test_insert VALUES (1, 21, 100, '24 Rosedale, Toronto, ONT', 'Pat Carlson')"),
      Query("SELECT * FROM test_insert"),
      Query("INSERT INTO test_insert (id, age, salary, address, name) VALUES (2, 23, 101, '25 Rosedale, Toronto, ONT', 'Pat Carlson2')"),
      Query("SELECT * FROM test_insert"),
      Query("INSERT INTO test_insert (age, address, salary, name, id) VALUES (25, '26 Rosedale, Toronto, ONT', 102, 'Pat2 Carlson', 3)"),
      Query("SELECT * FROM test_insert"),
      Query("INSERT INTO test_insert (age, address, salary, name) VALUES (26, 'test address', 30, 'test name')"),
      Query("SELECT * FROM test_insert"),
      Query("INSERT INTO test_insert (age, address, salary, name) VALUES (27, 'test address2', 31, 'test name')"),
      Query("INSERT INTO test_insert (age) VALUES (40)"),
      Query("SELECT age FROM test_insert"),
      Query("INSERT INTO test_insert (name) VALUES ('Wendy')"),
      Query("SELECT name FROM test_insert WHERE id=10"),
      Query("INSERT INTO test_insert (name, address, id, age) VALUES ('Peter Pan', 'first star to the right and straight on till morning', 42, 10)"),
      Query("SELECT name, address, age FROM test_insert WHERE id=42"),
      Query("DROP TABLE test_insert") });

static QueryList Select = QueryList("SingleSelect",
    { Query("CREATE TABLE IF NOT EXISTS test_select (id integer, age integer, salary integer, address text, name text)"),
      Query("INSERT INTO test_select VALUES (1, 10, 0, 'first star to the right and straight on till morning', 'Peter Pan')"),
      Query("INSERT INTO test_select VALUES (2, 16, 1000, 'Green Gables', 'Anne Shirley')"),
      Query("INSERT INTO test_select VALUES (3, 8, 0, 'London', 'Lucy')"),
      Query("INSERT INTO test_select VALUES (4, 10, 0, 'London', 'Edmund')"),
      Query("INSERT INTO test_select VALUES (5, 30, 100000, '221B Baker Street', 'Sherlock Holmes')"),
      Query("SELECT * FROM test_select WHERE id IN (1, 2, 10, 20, 30)"),
      Query("SELECT * FROM test_select WHERE id BETWEEN 3 AND 5"),
      Query("SELECT NULLIF(1, id) FROM test_select"),
      Query("SELECT NULLIF(id, 1) FROM test_select"),
      Query("SELECT NULLIF(id, id) FROM test_select"),
      Query("SELECT NULLIF(1, 2) FROM test_select"),
      Query("SELECT NULLIF(1, 1) FROM test_select"),
      Query("SELECT * FROM test_select"),
      Query("SELECT max(id) FROM test_select"),
      Query("SELECT max(salary) FROM test_select"),
      Query("SELECT COUNT(*) FROM test_select"),
      Query("SELECT COUNT(DISTINCT age) FROM test_select"),
      Query("SELECT COUNT(DISTINCT(address)) FROM test_select"),
      Query("SELECT name FROM test_select"),
      Query("SELECT address FROM test_select"),
      Query("SELECT * FROM test_select WHERE id>3"),
      Query("SELECT * FROM test_select WHERE age = 8"),
      Query("SELECT * FROM test_select WHERE salary=15"),
      Query("SELECT * FROM test_select WHERE age > 10"),
      Query("SELECT * FROM test_select WHERE age = 10 AND salary = 0"),
      Query("SELECT * FROM test_select WHERE age = 10 OR salary = 0"),
      Query("SELECT * FROM test_select WHERE name = 'Peter Pan'"),
      Query("SELECT * FROM test_select WHERE address='Green Gables'"),
      Query("SELECT * FROM test_select WHERE address <= '221C'"),
      Query("SELECT * FROM test_select WHERE address >= 'Green Gables' AND age > 9"),
      Query("SELECT * FROM test_select WHERE address >= 'Green Gables' OR age > 9"),
      Query("SELECT * FROM test_select WHERE address < 'ffFFF'"),
      Query("SELECT * FROM test_select ORDER BY id"),
      Query("SELECT * FROM test_select ORDER BY salary"),
      Query("SELECT * FROM test_select ORDER BY name"),
      Query("SELECT * FROM test_select ORDER BY address"),
      Query("SELECT sum(age) FROM test_select GROUP BY address ORDER BY address"),
      Query("SELECT salary, max(id) FROM test_select GROUP BY salary ORDER BY salary"),
      Query("SELECT * FROM test_select GROUP BY age ORDER BY age"),
      Query("SELECT * FROM test_select ORDER BY age ASC"),
      Query("SELECT * FROM test_select ORDER BY address DESC"),
      Query("SELECT sum(age) as z FROM test_select"),
      Query("SELECT sum(age) z FROM test_select"),
      Query("SELECT min(t.id) a FROM test_select AS t"),
      Query("SELECT t.address AS b FROM test_select t"),
      Query("SELECT * FROM test_select HAVING age"),
      Query("SELECT * FROM test_select HAVING age && id"),
      Query("DROP TABLE test_select") });

static QueryList SubQuery = QueryList("SubQuery",
    { Query("CREATE TABLE subqueryphun (morse integer, code integer)"),
      Query("CREATE TABLE numerouno (uno integer, dos integer, tres integer)"),
      Query("INSERT INTO subqueryphun VALUES (100, 200), (1000, 2000),"
            "                                (200, 100), (25, 25), (50, 50)"),
      Query("INSERT INTO numerouno VALUES (1, 2, 3), (100, 200, 300),"
            "                             (1000, 2000, 3000)"),
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      //     single row subqueries
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      // bad query, subquery returns multiple rows
      Query("SELECT * FROM subqueryphun"
            " WHERE (SELECT code FROM subqueryphun)"),
      Query("SELECT * FROM subqueryphun"
            " WHERE (SELECT code FROM subqueryphun LIMIT 1)"),
      Query("SELECT * FROM subqueryphun"
            " WHERE morse IN (SELECT morse FROM subqueryphun)"),
      Query("SELECT * FROM subqueryphun"
            " wHERE morse IN (SELECT 100 FROM subqueryphun)"),
      Query("SELECT * FROM subqueryphun"
            " WHERE (SELECT 1 FROM subqueryphun AS q WHERE q.morse"
            "         LIMIT 1)"),
      // use a table from higher level select in subquery
      Query("SELECT * FROM numerouno"
            " WHERE (SELECT dos FROM subqueryphun"
            "         WHERE subqueryphun.morse = numerouno.uno"
            "         LIMIT 1)"),
      // use an alias from higher level select in subquery
      Query("SELECT * FROM numerouno AS n1"
            " WHERE (SELECT tres FROM subqueryphun"
            "         WHERE subqueryphun.morse = n1.uno"
            "         LIMIT 1)"),
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      //         IN subqueries
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      Query("SELECT * FROM subqueryphun"
            " WHERE morse IN (SELECT morse FROM subqueryphun)"),
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      //       EXISTS subqueries
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      Query("SELECT * FROM subqueryphun"
            " WHERE EXISTS(SELECT * FROM subqueryphun)"),
      Query("SELECT * FROM numerouno as n1"
            " WHERE EXISTS(SELECT * FROM subqueryphun"
            "               WHERE subqueryphun.morse = n1.uno)"),
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      //      misc subquery bugs
      // ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      // summation in subquery was causing segfault
      Query("SELECT (SELECT SUM(uno) FROM numerouno)"),
      Query("SELECT * FROM numerouno WHERE (SELECT SUM(uno) FROM numerouno)"),
      Query("DROP TABLE subqueryphun"),
      Query("DROP TABLE numerouno")
    });

static QueryList Join = QueryList("SingleJoin",
    { Query("CREATE TABLE test_join1 (id integer, age integer, salary integer, address text, name text)"),
      Query("CREATE TABLE test_join2 (id integer, books integer, name text)"),
      Query("INSERT INTO test_join1 VALUES (1, 10, 0, 'first star to the right and straight on till morning','Peter Pan')"),
      Query("INSERT INTO test_join1 VALUES (2, 16, 1000, 'Green Gables', 'Anne Shirley')"),
      Query("INSERT INTO test_join1 VALUES (3, 8, 0, 'London', 'Lucy')"),
      Query("INSERT INTO test_join1 VALUES (4, 10, 0, 'London', 'Edmund')"),
      Query("INSERT INTO test_join1 VALUES (5, 30, 100000, '221B Baker Street', 'Sherlock Holmes')"),
      Query("INSERT INTO test_join2 VALUES (1, 6, 'Peter Pan')"),
      Query("INSERT INTO test_join2 VALUES (2, 8, 'Anne Shirley')"),
      Query("INSERT INTO test_join2 VALUES (3, 7, 'Lucy')"),
      Query("INSERT INTO test_join2 VALUES (4, 7, 'Edmund')"),
      Query("INSERT INTO test_join2 VALUES (10, 4, '221B Baker Street')"),
      Query("SELECT address FROM test_join1, test_join2 WHERE test_join1.id=test_join2.id"),
      Query("SELECT test_join1.id, test_join2.id, age, books, test_join2.name FROM test_join1, test_join2 WHERE test_join1.id = test_join2.id"),
      Query("SELECT test_join1.name, age, salary, test_join2.name, books FROM test_join1, test_join2 WHERE test_join1.age=test_join2.books"),
      Query("SELECT * FROM test_join1, test_join2 WHERE test_join1.name=test_join2.name"),
      Query("SELECT * FROM test_join1, test_join2 WHERE test_join1.address=test_join2.name"),
      Query("SELECT address FROM test_join1 AS a, test_join2 WHERE a.id=test_join2.id"),
      Query("SELECT a.id, b.id, age, books, b.name FROM test_join1 a, test_join2 AS b WHERE a.id=b.id"),
      Query("SELECT test_join1.name, age, salary, b.name, books FROM test_join1, test_join2 b WHERE test_join1.age = b.books"),
      Query("DROP TABLE test_join1"),
      Query("DROP TABLE test_join2") });

static QueryList Update = QueryList("SingleUpdate",
    { Query("CREATE TABLE test_update (id integer, age integer, salary integer, address text, name text)"),
      Query("INSERT INTO test_update VALUES (1, 10, 0, 'first star to the right and straight on till morning','Peter Pan')"),
      Query("INSERT INTO test_update VALUES (2, 16, 1000, 'Green Gables', 'Anne Shirley')"),
      Query("INSERT INTO test_update VALUES (3, 8, 0, 'London', 'Lucy')"),
      Query("INSERT INTO test_update VALUES (4, 10, 0, 'London', 'Edmund')"),
      Query("INSERT INTO test_update VALUES (5, 30, 100000, '221B Baker Street', 'Sherlock Holmes')"),
      Query("INSERT INTO test_update VALUES (6, 11, 0 , 'hi', 'no one')"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET age = age, address = name"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET name = address"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET salary=0"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET age=21 WHERE id = 6"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET address='Pemberly', name='Elizabeth Darcy' WHERE id=6"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET salary=55000 WHERE age=30"),
      Query("SELECT * FROM test_update"),
      Query("UPDATE test_update SET salary=20000 WHERE address='Pemberly'"),
      Query("SELECT * FROM test_update"),
      Query("SELECT age FROM test_update WHERE age > 20"),
      Query("SELECT id FROM test_update"),
      Query("SELECT sum(age) FROM test_update"),
      Query("UPDATE test_update SET age=20 WHERE name='Elizabeth Darcy'"),
      Query("SELECT * FROM test_update WHERE age > 20"),
      Query("SELECT sum(age) FROM test_update"),
      Query("UPDATE test_update SET age = age + 2"),
      Query("SELECT age FROM test_update"),
      Query("UPDATE test_update SET id = id + 10, salary = salary + 19, name = 'xxx', address = 'foo' WHERE address = 'London'"),
      Query("SELECT * FROM test_update"),
      Query("SELECT * FROM test_update WHERE address < 'fml'"),
      Query("UPDATE test_update SET address = 'Neverland' WHERE id=1"),
      Query("SELECT * FROM test_update"),
      Query("DROP TABLE test_update") });

static QueryList HOM = QueryList("HOMAdd",
    { Query("CREATE TABLE test_HOM (id integer, age integer, salary integer, address text, name text)"),
      Query("INSERT INTO test_HOM VALUES (1, 10, 0, 'first star to the right and straight on till morning','Peter Pan')"),
      Query("INSERT INTO test_HOM VALUES (2, 16, 1000, 'Green Gables', 'Anne Shirley')"),
      Query("INSERT INTO test_HOM VALUES (3, 8, 0, 'London', 'Lucy')"),
      Query("INSERT INTO test_HOM VALUES (4, 10, 0, 'London', 'Edmund')"),
      Query("INSERT INTO test_HOM VALUES (5, 30, 100000, '221B Baker Street', 'Sherlock Holmes')"),
      Query("INSERT INTO test_HOM VALUES (6, 21, 2000, 'Pemberly', 'Elizabeth')"),
      Query("INSERT INTO test_HOM VALUES (7, 10000, 1, 'Mordor', 'Sauron')"),
      Query("INSERT INTO test_HOM VALUES (8, 25, 100, 'The Heath', 'Eustacia Vye')"),
      Query("INSERT INTO test_HOM VALUES (12, NULL, NULL, 'nucwinter', 'gasmask++')"),
      Query("SELECT * FROM test_HOM"),
      Query("SELECT SUM(age) FROM test_HOM"),
      Query("SELECT * FROM test_HOM"),
      Query("SELECT SUM(age) FROM test_HOM"),
      Query("UPDATE test_HOM SET age = age + 1"),
      Query("SELECT SUM(age) FROM test_HOM"),
      Query("SELECT * FROM test_HOM"),
      Query("UPDATE test_HOM SET age = age + 3 WHERE id=1"),
      Query("SELECT * FROM test_HOM"),
      Query("UPDATE test_HOM SET age = 100 WHERE id = 1"),
      Query("SELECT * FROM test_HOM WHERE age = 100"),
      Query("SELECT COUNT(*) FROM test_HOM WHERE age > 100"),
      Query("SELECT COUNT(*) FROM test_HOM WHERE age < 100"),
      Query("SELECT COUNT(*) FROM test_HOM WHERE age <= 100"),
      Query("SELECT COUNT(*) FROM test_HOM WHERE age >= 100"),
      Query("SELECT COUNT(*) FROM test_HOM WHERE age = 100"),
      Query("SELECT SUM(address) FROM test_HOM"),
      Query("DROP TABLE test_HOM") });

static QueryList Delete = QueryList("SingleDelete",
    { Query("CREATE TABLE test_delete (id integer, age integer, salary integer, address text, name text)"),
      Query("INSERT INTO test_delete VALUES (1, 10, 0, 'first star to the right and straight on till morning','Peter Pan')"),
      Query("INSERT INTO test_delete VALUES (2, 16, 1000, 'Green Gables', 'Anne Shirley')"),
      Query("INSERT INTO test_delete VALUES (3, 8, 0, 'London', 'Lucy')"),
      Query("INSERT INTO test_delete VALUES (4, 10, 0, 'London', 'Edmund')"),
      Query("INSERT INTO test_delete VALUES (5, 30, 100000, '221B Baker Street', 'Sherlock Holmes')"),
      Query("INSERT INTO test_delete VALUES (6, 21, 2000, 'Pemberly', 'Elizabeth')"),
      Query("INSERT INTO test_delete VALUES (7, 10000, 1, 'Mordor', 'Sauron')"),
      Query("INSERT INTO test_delete VALUES (8, 25, 100, 'The Heath', 'Eustacia Vye')"),
      Query("DELETE FROM test_delete WHERE id=1"),
      Query("SELECT * FROM test_delete"),
      Query("DELETE FROM test_delete WHERE age=30"),
      Query("SELECT * FROM test_delete"),
      Query("DELETE FROM test_delete WHERE name='Eustacia Vye'"),
      Query("SELECT * FROM test_delete"),
      Query("DELETE FROM test_delete WHERE address='London'"),
      Query("SELECT * FROM test_delete"),
      Query("DELETE FROM test_delete WHERE salary = 1"),
      Query("SELECT * FROM test_delete"),
      Query("INSERT INTO test_delete VALUES (1, 10, 0, 'first star to the right and straight on till morning','Peter Pan')"),
      Query("SELECT * FROM test_delete"),
      Query("DELETE FROM test_delete"),
      Query("SELECT * FROM test_delete"),
      Query("DROP TABLE test_delete") });

static QueryList MultiDelete = QueryList("MultiDelete",
    { Query("CREATE TABLE gasoline (x integer)"),
      Query("CREATE TABLE coal (y integer)"),
      Query("CREATE TABLE nuclear (z integer)"),
      Query("INSERT INTO gasoline VALUES (1), (100), (1000), (10000)"),
      Query("INSERT INTO coal VALUES (1), (101), (1001), (10000)"),
      Query("INSERT INTO nuclear VALUES (2), (200), (2000), (20000)"),
      // no aliases
      Query("DELETE gasoline, coal"
            " FROM gasoline INNER JOIN coal ON gasoline.x = coal.y"),
      Query("SELECT * FROM gasoline, coal, nuclear"),
      // alias
      Query("DELETE g, nuclear FROM gasoline g, nuclear"),
      Query("SELECT * FROM gasoline, coal, nuclear"),

      // repopulate with data
      Query("INSERT INTO gasoline VALUES (16), (116), (1016), (10016)"),
      Query("INSERT INTO coal VALUES (16), (116), (1016), (10016)"),
      Query("INSERT INTO nuclear VALUES (16), (216), (2016), (20016)"),
      Query("SELECT count(*) FROM gasoline, coal, nuclear"),

      // alias with where clause
      Query("DELETE g, nuclear FROM gasoline g, nuclear"
            " WHERE nuclear.z < g.x"),
      Query("SELECT * FROM gasoline, coal, nuclear"),
      // alias with same name as table
      Query("DELETE coal, gasoline FROM coal coal, gasoline"),
      Query("SELECT * FROM gasoline, coal, nuclear"),
      Query("DROP TABLE gasoline"),
      Query("DROP TABLE coal"),
      Query("DROP TABLE nuclear") });

static QueryList Basic = QueryList("MultiBasic",
    { Query("CREATE TABLE t1 (id integer, post text, age bigint)"),
      Query("CREATE TABLE u_basic (id integer, username text)"),
      Query("CREATE TABLE "+PWD_TABLE_PREFIX+"u_basic (username text, psswd text)"),
      Query("INSERT INTO u_basic VALUES (1, 'alice')"),
      Query("SELECT * FROM u_basic"),
      Query("INSERT INTO t1 VALUES (1, 'text which is inserted', 23)"),
      Query("SELECT * FROM t1"),
      Query("SELECT post from t1 WHERE id = 1 AND age = 23"),
      Query("UPDATE t1 SET post='hello!' WHERE age > 22 AND id =1"),
      Query("SELECT * FROM t1"),
      Query("INSERT INTO u_basic VALUES (2, 'raluca')"),
      Query("SELECT * FROM u_basic"),
      Query("INSERT INTO t1 VALUES (2, 'raluca has text here', 5)"),
      Query("SELECT * FROM t1"),
      Query("DROP TABLE u_basic"),
      Query("DROP TABLE t1"),
      Query("DROP TABLE "+PWD_TABLE_PREFIX+"u_basic") });

static QueryList PrivMessages = QueryList("MultiPrivMessages",
    { Query("CREATE TABLE msgs (msgid integer, msgtext text)"),
      Query("CREATE TABLE privmsg (msgid integer, recid integer, senderid integer)"),
      Query("CREATE TABLE u_mess (userid integer, username text)"),
      Query("CREATE TABLE "+PWD_TABLE_PREFIX+"u_mess (username text, psswd text)"),
      Query("INSERT INTO u_mess VALUES (1, 'alice')"),
      Query("INSERT INTO u_mess VALUES (2, 'bob')"),
      Query("INSERT INTO privmsg (msgid, recid, senderid) VALUES (9, 1, 2)"),
      Query("INSERT INTO msgs VALUES (1, 'hello world')"),
      Query("SELECT msgtext FROM msgs WHERE msgid=1"),
      Query("INSERT INTO msgs VALUES (9, 'message for alice from bob')"),
      Query("DROP TABLE msgs"),
      Query("DROP TABLE privmsg"),
      Query("DROP TABLE u_mess"),
      Query("DROP TABLE "+PWD_TABLE_PREFIX+"u_mess") });

static QueryList UserGroupForum = QueryList("UserGroupForum",
    { Query("CREATE TABLE u (userid integer, username text)"),
      Query("CREATE TABLE usergroup (userid integer, groupid integer)"),
      Query("CREATE TABLE groupforum (forumid integer, groupid integer, optionid integer)"),
      Query("CREATE TABLE forum (forumid integer, forumtext text)"),
      Query("CREATE TABLE "+PWD_TABLE_PREFIX+"u (username text, psswd text)"),
     
      // Alice, Bob, Chris all logged on
      Query("INSERT INTO u VALUES (1, 'alice')"),
      Query("INSERT INTO u VALUES (2, 'bob')"),
      Query("INSERT INTO u VALUES (3, 'chris')"),

      Query("INSERT INTO usergroup VALUES (1,1)"),
      Query("INSERT INTO usergroup VALUES (2,2)"),
      Query("INSERT INTO usergroup VALUES (3,1)"),
      Query("INSERT INTO usergroup VALUES (3,2)"),

      //Alice is in group 1, Bob in group 2, Chris in group 1 & group 2
      Query("SELECT * FROM usergroup"),
      Query("INSERT INTO groupforum VALUES (1,1,14)"),
      Query("INSERT INTO groupforum VALUES (1,1,20)"),

      //Group 1 has access to forum 1
      Query("SELECT * FROM groupforum"),
      Query("INSERT INTO forum VALUES (1, 'success-- you can see forum text')"),
      
      // only Alice logged in and she should see forum 1
      Query("SELECT forumtext FROM forum WHERE forumid=1"),
      
      // only Bob logged in and he should not see forum 1
      Query("SELECT forumtext FROM forum WHERE forumid=1"),

      // only Chris logged in and he should see forum 1
      Query("SELECT forumtext FROM forum WHERE forumid=1"),

      // change forum text while Chris logged in
      Query("UPDATE forum SET forumtext='you win!' WHERE forumid=1"),
      Query("SELECT forumtext FROM forum WHERE forumid=1"),

      // only Alice logged in and she should see new text in forum 1
      Query("SELECT forumtext FROM forum WHERE forumid=1"),

      // create an orphaned forum
      Query("INSERT INTO forum VALUES (2, 'orphaned text! everyone should be able to see me')"),

      // only Alice logged in and she should see text in orphaned forum 2
      Query("SELECT forumtext FROM forum WHERE forumid=2"),

      // only Bob logged in and he should see text in orphaned forum 2
      Query("SELECT forumtext FROM forum WHERE forumid=2"),
      
      // only Chris logged in and he should see text in orphaned forum 2
      Query("SELECT forumtext FROM forum WHERE forumid=2"),

      // de-orphanize forum 2 -- now only accessible by group 2
      Query("INSERT INTO groupforum VALUES (2,2,20)"),

      // give group 2 access to forum 1 with the wrong access IDs -- since the forum will be inaccessible to group 2, doesn't matter that no one is logged in
      Query("INSERT INTO groupforum VALUES (1,2,2)"),
      Query("INSERT INTO groupforum VALUES (1,2,0)"),

      // attempt to gice group 2 actual access to the forum -- should fail, because no one is logged in
      Query("INSERT INTO groupforum VALUES (1,2,20)"),

      // only Bob logged in and he should still not have access to forum 1
      Query("SELECT forumtext FROM forum WHERE forumid=1"),
      Query("DROP TABLE u"),
      Query("DROP TABLE usergroup"),
      Query("DROP TABLE groupforum"),
      Query("DROP TABLE forum"),
      Query("DROP TABLE "+PWD_TABLE_PREFIX+"u") });

static QueryList Auto = QueryList("AutoInc",
    { Query("CREATE TABLE msgs (msgid integer PRIMARY KEY AUTO_INCREMENT, zooanimals integer, msgtext text)"),
      Query("CREATE TABLE moremore (x INTEGER,"
            "                       y integer PRIMARY KEY AUTO_INCREMENT)"
            "AUTO_INCREMENT=456"),
      Query("INSERT INTO msgs (msgtext, zooanimals) VALUES ('hello world', 100)"),
      Query("INSERT INTO msgs (msgtext, zooanimals) VALUES ('hello world2', 21)"),
      Query("INSERT INTO msgs (msgtext, zooanimals) VALUES ('hello world3', 10909)"),
      Query("INSERT INTO moremore (x) VALUES (1), (2), (3), (4)"),
      Query("SELECT msgtext FROM msgs WHERE msgid=1"),
      Query("SELECT msgtext FROM msgs WHERE msgid=2"),
      Query("SELECT msgtext FROM msgs WHERE msgid=3"),
      Query("INSERT INTO msgs VALUES (3, 2012, 'sandman') ON DUPLICATE KEY UPDATE zooanimals = VALUES(zooanimals), zooanimals = 22"),
      Query("SELECT * FROM msgs"),
      Query("SELECT SUM(zooanimals) FROM msgs"),
      Query("INSERT INTO msgs VALUES (3, 777, 'golfpants') ON DUPLICATE KEY UPDATE zooanimals = 16, zooanimals = VALUES(zooanimals)"),
      Query("SELECT * FROM msgs"),
      Query("SELECT SUM(zooanimals) FROM msgs"),
      Query("INSERT INTO msgs VALUES (9, 105, 'message for alice from bob')"),
      Query("INSERT INTO msgs VALUES (9, 201, 'whatever') ON DUPLICATE KEY UPDATE msgid = msgid + 10"),
      Query("SELECT * FROM msgs"),
      // Query("INSERT INTO msgs VALUES (1, 9001, 'lights are on') ON DUPLICATE KEY UPDATE msgid = zooanimals + 99, zooanimals=VALUES(zooanimals)"),
      Query("SELECT * FROM msgs"),
      Query("INSERT INTO msgs VALUES (2, 1998, 'stacksondeck') ON DUPLICATE KEY UPDATE zooanimals = VALUES(zooanimals), msgtext = VALUES(msgtext)"),
      Query("SELECT * FROM msgs"),
      Query("SELECT SUM(zooanimals) FROM msgs"),
      Query("ALTER TABLE msgs AUTO_INCREMENT = 555"),
      Query("INSERT INTO msgs (msgtext, zooanimals) VALUES"
            "   ('dolphins', 12)"),
      Query("SELECT * FROM msgs"),
      Query("INSERT INTO msgs (msgtext, zooanimals) VALUES"
            "   ('sharks', 12)"),
      Query("SELECT * FROM msgs"),
      Query("SELECT * FROM moremore"),
      Query("DROP TABLE msgs"),
      Query("DROP TABLE moremore")
    });

static QueryList Negative = QueryList("Negative",
    { Query("CREATE TABLE negs (a integer, b integer, c integer)"),
      Query("INSERT INTO negs (a, b, c) VALUES (10, -20, -100)"),
      Query("INSERT INTO negs (a, b, c) VALUES (-100, 50, -12)"),
      Query("INSERT INTO negs (a, b, c) VALUES (-8, -50, -18)"),
      Query("SELECT a FROM negs WHERE b = -50 OR b = 50"),
      Query("SELECT a FROM negs WHERE c = -100 OR b = -20"),
      Query("INSERT INTO negs (c) VALUES (-1009)"),
      Query("INSERT INTO negs (c) VALUES (1009)"),
      Query("SELECT * FROM negs WHERE c = -1009"),
      Query("DROP TABLE negs") });

static QueryList Null = QueryList("Null",
    { Query("CREATE TABLE test_null (uid integer, age integer, address text)"),
      Query("CREATE TABLE u_null (uid integer, username text)"),
      Query("CREATE TABLE "+PWD_TABLE_PREFIX+"u_null (username text, password text)"),
      Query("INSERT INTO u_null VALUES (1, 'alice')"),
      Query("INSERT INTO u_null VALUES (), ()"),
      Query("INSERT INTO u_null VALUES (2, 'somewhere'), (3, 'cookies')"),
      Query("INSERT INTO test_null (uid, age) VALUES (1, 20)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null (uid, address) VALUES (1, 'somewhere over the rainbow')"),
      Query("INSERT INTO test_null () VALUES (), ()"),
      Query("INSERT INTO test_null VALUES (), ()"),
      Query("INSERT INTO test_null (address, age) VALUES ('cookies', 1)"),
      Query("INSERT INTO test_null (age, address) VALUES (1, 'two')"),
      Query("INSERT INTO test_null (address, uid) VALUES ('three', 4)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null (uid, age) VALUES (1, NULL)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null (uid, address) VALUES (1, NULL)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null VALUES (1, 25, 'Australia')"),
      Query("SELECT * FROM test_null"),
      Query("SELECT * FROM test_null WHERE uid = 1"),
      Query("SELECT * FROM test_null WHERE age < 50"),
      Query("SELECT SUM(uid) FROM test_null"),
      Query("SELECT MAX(uid) FROM test_null"),
      Query("SELECT * FROM test_null"),
      Query("SELECT * FROM test_null WHERE address = 'cookies'"),
      Query("SELECT * FROM test_null WHERE address < 'amber'"),
      Query("SELECT * FROM test_null WHERE address LIKE 'aaron'"),
      Query("SELECT * FROM test_null LEFT JOIN u_null"
            "    ON test_null.uid = u_null.uid"),
      Query("SELECT * FROM test_null, u_null"),
      Query("SELECT * FROM test_null RIGHT JOIN u_null"
            "    ON test_null.uid = u_null.uid"),
      Query("SELECT * FROM test_null, u_null"),
      Query("SELECT * FROM test_null RIGHT JOIN u_null"
            "    ON test_null.address = u_null.username"),
      Query("SELECT * FROM test_null, u_null"),
      Query("SELECT * FROM test_null LEFT JOIN u_null"
            "    ON u_null.username = test_null.address"),
      Query("SELECT * FROM test_null, u_null"),
  /*{ Query("INSERT INTO u_null VALUES (1, 'alice')"),
      Query("INSERT INTO test_null (uid, age) VALUES (1, 20)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null (uid, address) VALUES (1, 'somewhere over the rainbow')"),
      Query("INSERT INTO test_null () VALUES (), ()"),
      Query("INSERT INTO test_null VALUES (), ()"),
      Query("INSERT INTO test_null (address, age) VALUES ('cookies', 1)"),
      Query("INSERT INTO test_null (age, address) VALUES (1, 'two')"),
      Query("INSERT INTO test_null (address, uid) VALUES ('three', 4)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null (uid, age) VALUES (1, NULL)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null (uid, address) VALUES (1, NULL)"),
      Query("SELECT * FROM test_null"),
      Query("INSERT INTO test_null VALUES (1, 25, 'Australia')"),
      Query("SELECT * FROM test_null"),
      Query("SELECT * FROM test_null WHERE uid = 1"),
      Query("SELECT * FROM test_null WHERE age < 50"),
      Query("SELECT SUM(uid) FROM test_null"),
      Query("SELECT MAX(uid) FROM test_null"),
      Query("SELECT * FROM test_null"),
      Query("SELECT * FROM test_null WHERE address = 'cookies'"),
      Query("SELECT * FROM test_null WHERE address < 'amber'"),
      Query("SELECT * FROM test_null WHERE address LIKE 'aaron'") },*/
      Query("DROP TABLE test_null"),
      Query("DROP TABLE u_null"),
      Query("DROP TABLE "+PWD_TABLE_PREFIX+"u_null") });

static QueryList BestEffort = QueryList("BestEffort",
    { Query("CREATE TABLE t (x integer, y integer)"),
      Query("INSERT INTO t VALUES (1, 100)"),
      Query("INSERT INTO t VALUES (22, 413)"),
      Query("INSERT INTO t VALUES (1001, 15)"),
      Query("INSERT INTO t VALUES (19, 18)"),
      Query("SELECT * FROM t"),
      Query("SELECT x*x FROM t"),
      Query("SELECT x*y FROM t"),
      Query("SELECT 2+2 FROM t"),
      Query("SELECT x+2+x FROM t"),
      Query("SELECT 2+x+2 FROM t"),
      Query("SELECT x+y+3+4 FROM t"),
      Query("SELECT 2*x*2*y FROM t"),
      Query("SELECT x, y FROM t WHERE x AND y"),
      Query("SELECT x, y FROM t WHERE x = 1 AND y < 200"),
      Query("SELECT x, y FROM t WHERE x AND y = 15"),
      Query("SELECT 10, x+y FROM t WHERE x"),
      Query("DROP TABLE t") });

static QueryList DefaultValue = QueryList("DefaultValue",
    { Query("CREATE TABLE def_0 (x INTEGER NOT NULL DEFAULT 10,"
      "                    y VARCHAR(100) NOT NULL DEFAULT 'purpflowers',"
      "                    z INTEGER)"),
      Query("CREATE TABLE def_1 (a INTEGER NOT NULL DEFAULT '100',"
      "                    b INTEGER,"
      "                    c VARCHAR(100))"),
      Query("INSERT INTO def_0 VALUES (100, 'singsongs', 500),"
            "                         (220, 'heyfriend', 15)"),
      Query("INSERT INTO def_0 (z) VALUES (500), (220), (32)"),
      Query("INSERT INTO def_0 (z, x) VALUES (500, '500')"),
      Query("INSERT INTO def_0 (z) VALUES (100)"),
      Query("INSERT INTO def_1 VALUES (250, 10000, 'smile!')"),
      Query("INSERT INTO def_1 (a, b, c) VALUES (250, 100, '!')"),
      Query("INSERT INTO def_1 (b, c) VALUES (250, 'smile!'),"
            "                                (99, 'happybday!')"),
      Query("SELECT * FROM def_0, def_1"),
      Query("SELECT * FROM def_0 WHERE x = 10"),
      Query("INSERT INTO def_0 (z) VALUES (500), (12), (19)"),
      Query("SELECT * FROM def_0 WHERE x = 10"),
      Query("SELECT * FROM def_0 WHERE y = 'purpflowers'"),
      Query("INSERT INTO def_0 (z) VALUES (450), (200), (300)"),
      Query("SELECT * FROM def_0 WHERE y = 'purpflowers'"),
      Query("SELECT * FROM def_0, def_1"),
      Query("DROP TABLE def_0"),
      Query("DROP TABLE def_1") });

static QueryList Decimal = QueryList("Decimal",
    { Query("CREATE TABLE dec_0 (x DECIMAL(10, 5),"
      "                    y DECIMAL(10, 5) NOT NULL DEFAULT 12.125)"),
      Query("CREATE TABLE dec_1 (a INTEGER, b DECIMAL(4, 2))"),
      Query("INSERT INTO dec_0 VALUES (50, 100.5)"),
      Query("INSERT INTO dec_0 VALUES (20.50, 1000.5)"),
      Query("INSERT INTO dec_0 VALUES (50, 100.59)"),
      Query("INSERT INTO dec_0 (x) VALUES (1.1)"),
      Query("INSERT INTO dec_1 VALUES (8, 1000.5)"),
      Query("SELECT * FROM dec_0 WHERE x = 50"),
      Query("SELECT * FROM dec_0 WHERE x < 50"),
      Query("SELECT * FROM dec_0 WHERE y = 100.5"),
      Query("SELECT * FROM dec_0"),
      Query("INSERT INTO dec_0 VALUES (19, 100.5)"),
      Query("SELECT * FROM dec_0 WHERE y = 100.5"),
      Query("SELECT * FROM dec_1"),
      Query("SELECT * FROM dec_0, dec_1 WHERE dec_0.y = dec_1.b"),
      Query("DROP TABLE dec_0"),
      Query("DROP TABLE dec_1") });

static QueryList NonStrictMode = QueryList("NonStrictMode",
    { Query("CREATE TABLE not_strict (x INTEGER NOT NULL,"
      "                         y INTEGER NOT NULL DEFAULT 100,"
      "                         z VARCHAR(100) NOT NULL)"),
      Query("INSERT INTO not_strict VALUES (150, 230, 'flowers')"),
      Query("INSERT INTO not_strict VALUES (850, 930, 'rainbow')"),
      Query("INSERT INTO not_strict (y) VALUES (11930)"),
      Query("SELECT * FROM not_strict WHERE x = 0"),
      Query("SELECT * FROM not_strict WHERE z = ''"),
      Query("INSERT INTO not_strict (y) VALUES (1212)"),
      Query("SELECT * FROM not_strict WHERE x = 0"),
      Query("SELECT * FROM not_strict WHERE z = ''"),
      Query("INSERT INTO not_strict (x) VALUES (0)"),
      Query("INSERT INTO not_strict (x, z) VALUES (12001, 'sun')"),
      Query("INSERT INTO not_strict (z) VALUES ('curtlanguage')"),
      Query("SELECT * FROM not_strict WHERE x = 0"),
      Query("SELECT * FROM not_strict WHERE z = ''"),
      Query("SELECT * FROM not_strict WHERE x < 110"),
      Query("SELECT * FROM not_strict"),
      Query("DROP TABLE not_strict") });

static QueryList Transactions = QueryList("Transactions",
    { Query("CREATE TABLE trans (a integer, b integer, c integer)"),
      Query("INSERT INTO trans VALUES (1, 2, 3)"),
      Query("INSERT INTO trans VALUES (33, 22, 11)"),
      Query("SELECT * FROM trans"),
      Query("START TRANSACTION"),
      Query("INSERT INTO trans VALUES (333, 222, 111)"),
      Query("ROLLBACK"),
      Query("SELECT * FROM trans"),
      Query("INSERT INTO trans VALUES (45, 22, 15)"),
      Query("UPDATE trans SET a = a + 1, b = c + 1"),
      Query("START TRANSACTION"),
      Query("UPDATE trans SET a = a + a, b = b + 12"),
      Query("SELECT * FROM trans"),
      Query("ROLLBACK"),
      Query("SELECT * FROM trans"),
      Query("START TRANSACTION"),
      Query("UPDATE trans SET a = c + 1, b = a + 1"),
      Query("COMMIT"),
      Query("ROLLBACK"),
      Query("SELECT * FROM trans"),
      Query("START TRANSACTION"),
      Query("UPDATE trans SET a = a + 1, c = 50 WHERE a < 50000"),
      Query("ROLLBACK", Query::WHERE_EXEC::CONTROL),
      Query("SELECT * FROM trans"),
      Query("START TRANSACTION"),
      Query("INSERT INTO trans VALUES (1, 50, 150)"),
      Query("UPDATE trans SET b = b + 10 WHERE c = 50"),
      Query("ROLLBACK", Query::WHERE_EXEC::CONTROL),
      Query("SELECT * FROM trans"),
      Query("DROP TABLE trans") });

static QueryList TableAliases = QueryList("TableAliases",
    { Query("CREATE TABLE star (a integer, b integer, c integer)"),
      Query("CREATE TABLE mercury (a integer, b integer, c integer)"),
      Query("CREATE TABLE moon (x integer, y integer, z integer)"),
      Query("INSERT INTO star VALUES (55, 66, 77), (99, 22, 109)"),
      Query("INSERT INTO mercury VALUES (55, 18, 17), (16, 15, 14)"),
      Query("INSERT INTO moon VALUES (55, 18, 1), (22, 22, 444)"),
      Query("SELECT s.a, e.b FROM star AS s INNER JOIN mercury AS e"
            "                  ON s.a = e.a"
            "               WHERE s.c < 100"),
      Query("SELECT * FROM mercury INNER JOIN mercury AS e"
            "           ON mercury.a = e.a"),
      Query("SELECT o.x, o.y FROM moon AS o INNER JOIN moon AS o2"
            "                  ON o.x = o2.y"),
      Query("SELECT mercury.a, mercury.b, e.a FROM star AS mercury"
            " INNER JOIN mercury AS e ON (mercury.a = e.a)"
            " WHERE mercury.b <> 18 AND mercury.b <> 15"),
      // throws an exception
      // Query("SELECT * FROM star AS q WHERE a IN"
            // "   (SELECT a FROM mercury)"),
      Query("DROP TABLE star"),
      Query("DROP TABLE mercury"),
      Query("DROP TABLE moon") });

static QueryList DDL = QueryList("DDL",
    { Query("CREATE TABLE ddl_test (a integer, b integer, c integer)"),
      Query("INSERT INTO ddl_test VALUES (1, 2, 3)"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test DROP COLUMN a"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test ADD COLUMN a integer"),
      Query("SELECT * FROM ddl_test"),
      Query("INSERT INTO ddl_test VALUES (3, 4, 5), (5, 6, 7),(7, 8, 9)"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test DROP COLUMN b, DROP COLUMN c"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test ADD COLUMN b integer, DROP COLUMN a,"
            "                     ADD COLUMN c integer"),
      Query("INSERT INTO ddl_test VALUES (15, '1212'), (20, '7676')"),
      Query("SELECT * FROM ddl_test"),
      Query("DELETE FROM ddl_test"),
      Query("INSERT INTO ddl_test VALUES (12, 15), (44, 14), (19, 5)"),
      Query("ALTER TABLE ddl_test ADD PRIMARY KEY(b)"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test DROP PRIMARY KEY,"
            "                     ADD PRIMARY KEY(b)"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test ADD INDEX j(b), ADD INDEX k(b, c)"),
      Query("SELECT * FROM ddl_test"),
      Query("ALTER TABLE ddl_test DROP INDEX j, DROP INDEX k,"
            "                     ADD INDEX j(b), ADD INDEX k(b, c)"),
      Query("SELECT * FROM ddl_test"),
      Query("CREATE INDEX i ON ddl_test (b, c)"),
      Query("ALTER TABLE ddl_test DROP INDEX i, ADD INDEX i(b)"),
      Query("ALTER TABLE ddl_test DROP INDEX i"),
      Query("DROP TABLE ddl_test")});

// the oEq column will encrypt three times and all three of these will pad;
// the mysql's maximum field size is 2**32 - 1; so the maximum field size
// we support is 2**32 - 1 - (3 * AES_BLOCK_BYTES)
static QueryList MiscBugs = QueryList("MiscBugs",
    { Query("CREATE TABLE crawlies (purple VARCHAR(4294967247),"
            "                       pink VARCHAR(0))"),
      // Query("CREATE TABLE enums (x enum('this', 'that'))"),
      Query("CREATE TABLE bugs (spider TEXT)"),
      Query("CREATE TABLE more_bugs (ant INTEGER)"),
      Query("CREATE TABLE real_type_bug (a real, b real unsigned,"
            "                            c real unsigned not null)"),
      Query("INSERT INTO bugs VALUES ('8legs'), ('crawly'), ('manyiz')"),
      Query("INSERT INTO more_bugs VALUES (9012), (2913), (19114)"),
      // Query("INSERT INTO enums VALUES ('this'), ('that')"),
      // Query("SELECT * FROM enums"),                 // proxy test
      // Query("SELECT spider + spider FROM bugs"),    // proxy test
      // Query("SELECT spider + spider FROM bugs"),    // proxy test
      // Query("SELECT SUM(spider) FROM bugs"),
      // Query("SELECT GREATEST(ant, 5000) FROM more_bugs"),
      // Query("SELECT GREATEST(12, ant, 5000) FROM more_bugs"),
      Query("CREATE TABLE crawlers (pink DATE)"),
      Query("INSERT INTO crawlers VALUES"
            "   ('0'), (0), ('2015-05-04 4:5:6')"),
      Query("SELECT * FROM crawlers"),
      Query("INSERT INTO crawlers VALUES"
            "   ('2415-05-04 4:5:6'), ('1998-500-04 4:5:6')"),
      Query("SELECT * FROM crawlers"),
      Query("INSERT INTO crawlers VALUES"
            "   ('100000-05-04 4:5:6'), ('1998-5-04 a:b:c'),"
            "   ('1000-05-04 house'),   ('1998-4-04 2015')"),
      Query("SELECT * FROM crawlers"),
      Query("SELECT * FROM bugs, more_bugs, crawlers, crawlies"),
      Query("INSERT INTO real_type_bug VALUES (1, 2, 3)"),
      Query("SELECT * FROM real_type_bug"),
      Query("UPDATE real_type_bug SET a = a + 1, b = b + 1"),
      // we don't currently support; but it shouldn't crash the system
      Query("CREATE TABLE jjj SELECT * FROM enums"),
      Query("SHOW ENGINES"),

      Query("CREATE TABLE floating (x float)"),
      Query("INSERT INTO floating VALUES (23e+12), (12e+14), ('a'), (12),"
            "                            ('12e+5')"),
      Query("SELECT * FROM floating WHERE x < 11e+14"),
      Query("SELECT * FROM floating"),
      Query("DROP TABLE floating"),

      // SpecialUpdate must correctly handle escaped chars
      Query("CREATE TABLE su (x integer, y text)"),
      Query("INSERT INTO su VALUES (1, 'some\\'text')"),
      Query("UPDATE su SET x = x + 1"),
      Query("SELECT * FROM su"),
      Query("DROP TABLE su"),

      Query("DROP TABLE crawlies"),
      Query("DROP TABLE enums"),
      Query("DROP TABLE bugs"),
      Query("DROP TABLE more_bugs"),
      Query("DROP TABLE real_type_bug"),
      Query("DROP TABLE crawlers")
    });

static QueryList QuotedSchemaObjects = QueryList("QuotedSchemaObjects",
    { Query("CREATE DATABASE IF NOT EXISTS `over+there`"),
      Query("USE `over+there`"),
      Query("CREATE TABLE `over+there`.`more+quotes`"
            "   (`quoted-+as*well` integer)"),
      Query("CREATE TABLE `hard/\\quotes`"
            "   (`!@abc` text, `$$eda` integer)"),
      Query("INSERT INTO `hard/\\quotes` VALUES "
            "   (1, 'quoting'), (12, 'shouldn;tbe'), (100, 'hard')"),
      Query("SELECT * FROM `hard/\\quotes`"),
      Query("SELECT * FROM `hard/\\quotes` WHERE `!@abc` < 1200"),
      Query("SELECT * FROM `hard/\\quotes`"),
      Query("SELECT * FROM `more+quotes`"),
      Query("SELECT * FROM `over+there`.`more+quotes`"),
      Query("DROP DATABASE IF EXISTS `over+there`"),
      Query("USE cryptdbtest"),
    });

// NOTE: These tests are not very effective at determining if things are
//       functional; need to compare levels and exceptions
static QueryList Directives = QueryList("Directives",
    { Query("CREATE TABLE directives (x integer, y text, z integer)"),
      Query("INSERT INTO directives VALUES (1, 'school learnin`', 7),"
            "                              (2, 'book learnin`', 8),"
            "                              (3, 'skool', 9)"),
      Query("SELECT * FROM directives"),
      // try to do non-existent directive
      Query("SET @cryptdb='wontwork', @other='that'"),
      // try to do multiple directives
      Query("SET @cryptdb='adjust', @cryptdb='adjust', @and='that'"),
      // try to adjust on a non existent database
      Query("SET @cryptdb='adjust', @database='notreal',"
            "    @field='wontmatter', @table='irrelevant',"
            "    @oHOM='OPE'"),
      Query("SELECT * FROM directives"),
      // try to adjust a non existent table
      Query("SET @cryptdb='adjust', @table='notrealnotreal',"
            "    @database='cryptdbtest', @field='doesntmatter',"
            "    @oOrder='OPE'"),
      Query("SELECT * FROM directives"),
      // try to adjust a non existent field
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @oOrder='OPE', @table='directives',"
            "    @field='made-up'"),
      Query("SELECT * FROM directives"),
      // try to adjust a non existent onion
      Query("SET @field='x', @cryptdb='adjust', @database='cryptdbtest',"
            "    @table='directives',"
            "    @badbad='OPE'"),
      Query("SELECT * FROM directives"),
      // try to adjust to non existent level
      Query("SET @database='cryptdbtest',"
            "    @table='directives', @field='x',"
            "    @oOrder='unreality', @cryptdb='adjust'"),
      Query("SELECT * FROM directives"),
      // try to adjust onion to real level it doesnt support
      // > FIXME: handling of this is not nice
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @table='directives', @field='x',"
            "    @oOrder='HOM'"),
      Query("SELECT * FROM directives"),
      // do real adjustment
      Query("SET @table='directives', @database='cryptdbtest',"
            "    @cryptdb='adjust', @field='x',"
            "    @oOrder='OPE'"),
      Query("SELECT * FROM directives WHERE x < 100"),
      // do it again (adjust to current level)
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @table='directives', @field='x',"
            "    @oOrder='OPE'"),
      Query("SELECT * FROM directives WHERE x < 100"),
      Query("SET @cryptdb='show', @random='stuff', @nothing='nothing'"),
      // try to adjust upwards
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @table='directives', @field='x',"
            "    @oOrder='RND'"),
      Query("SELECT * FROM directives WHERE x < 100"),
      // do multiple adjustments to same onion
      // NOTE: unsupported
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @oEq='DETJOIN', @table='directives', @field='x',"
            "    @oEq='DET'"),
      Query("SELECT * FROM directives"),
      // adjust down multiple layers
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @oEq='DETJOIN', @table='directives', @field='x'"),
      Query("SELECT * FROM directives WHERE x = 1"),
      // multiple adjustments to different onions
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @table='directives', @field='y',"
            "    @oEq='DET', @oOrder='OPE'"),
      Query("SELECT * FROM directives WHERE y < 'somerandomtext'"),
      Query("SELECT * FROM directives WHERE y = 'moretext'"),
      // do a good adjustment and one that won't do anything
      Query("SET @cryptdb='adjust', @database='cryptdbtest',"
            "    @table='directives', @field='z',"
            "    @oEq ='DET', @oOrder='RND'"),
      Query("SELECT * FROM directives WHERE z = 8"),
      Query("SET @more='less', @cryptdb='show', @nothing='short'"),
      Query("DROP TABLE directives")
    });

static QueryList Range = QueryList("Range",
      // we must run the control database in strict mode in order to
      // match semantics
    { Query("SET SESSION sql_mode = 'ANSI,TRADITIONAL'",
            Query::WHERE_EXEC::CONTROL),
      Query("CREATE TABLE t (t TINYINT UNSIGNED DEFAULT 0,"
            "                s SMALLINT UNSIGNED DEFAULT 0,"
            "                m MEDIUMINT UNSIGNED DEFAULT 0,"
            "                i INT UNSIGNED DEFAULT 0,"
            "                b BIGINT UNSIGNED DEFAULT 0)"),
      // lets take a look at the largest value
      Query("INSERT INTO t (b) VALUES (18446744073709551615)"),
      // lets also force the UDF to handle some other values
      Query("INSERT INTO t (b) VALUES (18446), (1001), (2), (2000009)"),
      Query("SELECT * FROM t"),
      // Query("SELECT SUM(b) FROM t"),
      Query("SELECT * FROM t WHERE b > 18446744073709551614"
            "                  AND b <= 18446744073709551615"),
      Query("SELECT MAX(b) FROM t"),
      // look at the largest value without server side decryption
      Query("INSERT INTO t (b) VALUES (18446744073709551615)"),
      Query("SELECT * FROM t WHERE b > 18446744073709551614"
            "                  AND b <= 18446744073709551615"),
      Query("SELECT MAX(b) FROM t"),
      // unsigned minimum
      Query("INSERT INTO t VALUES (0, 0, 0, 0, 0)"),
      Query("SELECT * FROM t"),
      // largest single value that will fit in each row
      Query("INSERT INTO t VALUES (255, 255, 255, 255, 255)"),
      Query("SELECT * FROM t"),
      // largest value each field will support
      Query("INSERT INTO t VALUES (255, 65535, 16777215,"
            "                      4294967295, 4294967295)"),
      // are leading zeros playing nice?
      Query("SELECT * FROM t ORDER BY b"),
      Query("SELECT MIN(b) FROM t"),
      // should fail on both because 5000 can't go in tiny
      Query("INSERT INTO t VALUES (5000, 5000, 5000, 5000, 5000)"),
      Query("SELECT * FROM t"),
      Query("SELECT SUM(t), SUM(s), SUM(m), SUM(i) FROM t"),
      // one more than maximum, should fail on both
      Query("INSERT INTO t (t) VALUES (256)"),
      Query("INSERT INTO t (s) VALUES (65536)"),
      Query("INSERT INTO t (m) VALUES (16777217)"),
      Query("INSERT INTO t (i) VALUES (4294967296)"),
      Query("SELECT * FROM t"),
      Query("SELECT SUM(t), SUM(s), SUM(m), SUM(i) FROM t"),
      // will fail on cryptdb and succeed on the control database
      // Query("INSERT INTO t (b) VALUES (4294967296)"),
      // Query("SELECT * FROM t"),
      // should fail on both
      Query("UPDATE t SET t = t + 5"),
      Query("UPDATE t SET s = s + 5"),
      Query("UPDATE t SET m = m + 5"),
      Query("UPDATE t SET i = i + 5"),
      Query("SELECT MAX(t), MAX(s), MAX(m), MAX(i), MAX(b) FROM t"),
      Query("SELECT MIN(t), MIN(s), MIN(m), MIN(i), MIN(b) FROM t"),
      Query("SELECT SUM(t), SUM(s), SUM(m), SUM(i) FROM t"),
      // will on cryptdb and succeed on control database
      // Query("UPDATE t SET b = b + 5"),
      // Query("SELECT SUM(b) FROM t"),
      Query("SELECT * FROM t"),
      // conditional selections
      Query("SELECT t, s FROM t WHERE t >= 255 AND s < 65535"),
      Query("SELECT m, i FROM t WHERE m < 1677215"),
      Query("SELECT m, i FROM t WHERE m > 1677215"),
      Query("SELECT t, s, i FROM t WHERE i = 16777215"),
      Query("SELECT * from t"),
      Query("SELECT MAX(t), MAX(s), MAX(m), MAX(i), MAX(b) FROM t"),
      /*
      // the comparisons fail because the constants are out of range
      Query("SELECT m, t, s FROM t WHERE s = 65536"),
      Query("SELECT m, i FROM t WHERE m > 4294967290"),
      Query("SELECT i FROM t WHERE i > 99999999999999999999"),
      Query("SELECT t, i FROM t WHERE t < 99999999999999999999"),
      Query("SELECT t, s, m, i FROM t"),
      */
      Query("SELECT t, s, m FROM t WHERE s = m AND b = i"),
      // fails because we don't handle summations larger than 64 bits
      Query("SELECT SUM(b) FROM t"),
      Query("DROP TABLE t"),
      Query("SET SESSION sql_mode = ''", Query::WHERE_EXEC::CONTROL)});

//-----------------------------------------------------------------------

Connection::Connection(const TestConfig &input_tc, test_mode input_type) {
    tc = input_tc;
    type = input_type;
    proxy_pid = -1;

    try {
        start();
    } catch (...) {
        stop();
        throw;
    }
}


Connection::~Connection() {
    stop();
}

void
Connection::restart() {
    stop();
    start();
}


void
Connection::start() {
    std::cerr << "start " << tc.db << std::endl;
    uint64_t mkey = 1133421234;
    std::string masterKey = BytesFromInt(mkey, AES_KEY_BYTES);
    switch (type) {
        //plain -- new connection straight to the DB
        case UNENCRYPTED:
        {
            Connect *const c =
                new Connect(tc.host, tc.user, tc.pass, tc.port);
            conn_set.insert(c);
            this->conn = conn_set.begin();
            break;
        }
        //single -- new Rewriter
        case SINGLE:
            break;
        case PROXYPLAIN:
        case PROXYENC:
        // TODO: a separate process for proxy
        case ENC:
        {
            ConnectionInfo ci(tc.host, tc.user, tc.pass);
            const std::string master_key = "2392834";
            SharedProxyState *const shared_ps =
                new SharedProxyState(ci, tc.shadowdb_dir, master_key,
                                     determineSecurityRating());
            ProxyState *const ps = new ProxyState(*shared_ps);
            re_set.insert(ps);
            this->re_it = re_set.begin();
        }
        break;
        default:
            assert_s(false, "invalid type passed to Connection");
    }
}

void
Connection::stop() {
    switch (type) {
        case PROXYPLAIN:
        case ENC:
            for (auto r = re_set.begin(); r != re_set.end(); r++) {
                delete *r;
            }
            re_set.clear();
            break;
        case SINGLE:
            break;
        case UNENCRYPTED:
            for (auto c = conn_set.begin(); c != conn_set.end(); c++) {
                delete *c;
            }
            conn_set.clear();
            break;
        default:
            break;
    }
}

ResType
Connection::execute(const Query &query) {
    switch (type) {
        case ENC:
            return executeRewriter(query);
        case UNENCRYPTED:
        case PROXYPLAIN:
            return executeConn(query);
        case SINGLE:
            break;
        default:
            assert_s(false, "unrecognized type in Connection");
    }
    return ResType(false, 0, 0);
}

void
Connection::executeFail(const Query &query) {
    LOG(test) << "Query: " << query.query << " could not execute" << std::endl;
}

ResType
Connection::executeConn(const Query &query) {
    std::unique_ptr<DBResult> dbres(nullptr);

    conn++;
    if (conn == conn_set.end()) {
        conn = conn_set.begin();
    }

    if (!(*conn)->execute(query.query, &dbres)) {
        executeFail(query);
        return ResType(false, 0, 0);
    }
    return dbres->unpack();
}

ResType
Connection::executeRewriter(const Query &query) {
    /*
    //translate the query
    re_it++;
    if (re_it == re_set.end()) {
        re_it = re_set.begin();
    }

    ProxyState *const ps = *re_it;
    // If this assert fails, deteremine if one schema_cache makes sense
    // for multiple connections.
    assert(re_set.size() == 1);
    const std::string &default_db =
        getDefaultDatabaseForConnection(ps->getConn());

    return executeQuery(*ps, query.query, default_db,
                        &this->schema_cache).res_type;
    */
    return ResType(false, 0, 0);
}

my_ulonglong
Connection::executeLast() {
    switch(type) {
        case SINGLE:
        case UNENCRYPTED:
        case PROXYPLAIN:
        case PROXYSINGLE:
            // TODO(ccarvalho) check this 
            break;
        default:
            assert_s(false, "type does not exist");
    }
    return 0;
}

my_ulonglong
Connection::executeLastConn() {
    conn++;
    if (conn == conn_set.end()) {
        conn = conn_set.begin();
    }
    return (*conn)->last_insert_id();
}

my_ulonglong
Connection::executeLastEDB() {
    std::cerr << "No functionality for LAST_INSERT_ID() without proxy" << std::endl;
    return 0;
}

//----------------------------------------------------------------------

static bool
CheckQuery(const TestConfig &tc, const Query &query)
{
    /*
    LOG(test) << "query: " << query.query;

    // FIXME: this code must be reworked, don't duplicate query execution logic
    if (query.crash_point != NULL) {
        // FIXME: doesn't have support for differential execution
        global_crash_point = query.crash_point->name;
        LOG(test) << "crash point: " << global_crash_point;

        try {
            test->execute(query);
        } catch (const CrashTestException &e) {}

        if (query.crash_point->executed_query) {
            control->execute(query);
        }

        global_crash_point = "";
    } else {
        LOG(test) << "no crash point";

        // _always_ execute the query against the control database
        // > there may be side effects that we want even if the test
        //   database throws an exception
        // > ie, if an INSERT throws an exception we want the SELECTs
        //   coming afterwards to fail as well
        ResType control_res(false);
        if (Query::WHERE_EXEC::TEST != query.where_exec) {
            control_res = control->execute(query);
            if (Query::WHERE_EXEC::CONTROL == query.where_exec) {
                return control_res.ok;
            }
        }

        ResType test_res(false);
        if (Query::WHERE_EXEC::CONTROL != query.where_exec) {
            try {
              test_res = test->execute(query);
            } catch (...) {
                return false == control_res.ok;
            }
            if (Query::WHERE_EXEC::TEST == query.where_exec) {
                return test_res.ok;
            }
        }
        assert(Query::WHERE_EXEC::BOTH == query.where_exec);

        if (control_res.ok != test_res.ok) {
            LOG(warn) << "control " << control_res.ok
                << ", test " << test_res.ok
                << " for query: " << query.query;

            if (tc.stop_if_fail) {
                thrower() << "stop on failure";
            }

            return false;
        }

        if (!match(test_res, control_res)) {
            LOG(warn) << "result mismatch for query: " << query.query;
            LOG(warn) << "control is:";
            printRes(control_res);
            LOG(warn) << "test is:";
            printRes(test_res);

            if (tc.stop_if_fail) {
                thrower() << "stop on failure";
            }

            return false;
        }
    }

    if (query.onion_states != NULL) {
        DBOnionState dbos = *(query.onion_states);

        ProxyState *const ps = test->getProxyState();
        auto si = loadSchemaInfo(ps->getConn(), ps->getEConn());
        const auto &dbs = si->getChildren();
        for (auto db_it = dbs.begin(); db_it != dbs.end(); ++db_it) {
            const auto &ts = db_it->second->getChildren();
            for (auto t_it = ts.begin(); t_it != ts.end(); ++t_it) {
                const std::string &t_name = t_it->first.getValue();
                const auto &fs = t_it->second->getChildren();
                for (auto f_it = fs.begin(); f_it != fs.end(); ++f_it) {
                    const std::string &f_name = f_it->first.getValue();
                    const auto &os = f_it->second->getChildren();
                    for (auto o_it = os.begin(); o_it != os.end(); ++o_it) {
                        const std::string &o_name =
                            TypeText<onion>::toText(o_it->first.getValue());
                        const std::string &level =
        TypeText<SECLEVEL>::toText(o_it->second->getLayerBack()->level());
                        if (dbos[t_name][f_name][o_name] != level) {
                          return false;
                        }
                    }
                }
            }
        }
    }
    */

    return false;
}

struct Score {
    unsigned int success;
    unsigned int total;
    std::string name;

    Score(const std::string &name) : success(0), total(0), name(name) {}

    void mark(bool t) {
        total++;

        if (t) {
          success++;
        }
    }

    std::string stringify() {
        return name + ":\t" + std::to_string(success) + "/" +
               std::to_string(total);
    }
};

static Score
CheckQueryList(const TestConfig &tc, const QueryList &queries) {
    Score score(queries.name);
    for (auto q = queries.queries.begin(); q != queries.queries.end(); q++) {
        score.mark(CheckQuery(tc, *q));
    }

    return score;
}

static void
RunTest(const TestConfig &tc) {
    // ###############################
    //      TOTAL RESULT: 594/610
    // ###############################

    std::vector<Score> scores;

    assert(testSlowMatch());
    assert(test64bitZZConversions());

    // Pass 49/49
    scores.push_back(CheckQueryList(tc, Select));

    // Pass 18/18
    scores.push_back(CheckQueryList(tc, SubQuery));

    // Pass 28/28
    scores.push_back(CheckQueryList(tc, HOM));

    // Pass 17/17
    scores.push_back(CheckQueryList(tc, Insert));

    // Pass 22/22
    scores.push_back(CheckQueryList(tc, Join));

    // Pass 17/17
    scores.push_back(CheckQueryList(tc, Basic));

    // Pass 36/36
    scores.push_back(CheckQueryList(tc, Update));

    // Pass 24/24
    scores.push_back(CheckQueryList(tc, Delete));

    // Pass 21/21
    scores.push_back(CheckQueryList(tc, MultiDelete));

    // Pass 14/14
    scores.push_back(CheckQueryList(tc, PrivMessages));

    // Pass 37/37
    scores.push_back(CheckQueryList(tc, UserGroupForum));

    // Pass 40/40
    scores.push_back(CheckQueryList(tc, Null));

    // Pass 18/18
    ProxyState *const ps = test->getProxyState();
    if (ps->defaultSecurityRating() == SECURITY_RATING::BEST_EFFORT) {
        scores.push_back(CheckQueryList(tc, BestEffort));
    }

    // Pass 28/31
    scores.push_back(CheckQueryList(tc, Auto));

    // Pass ?/?
    // scores.push_back(CheckQueryList(tc, Negative));

    // Pass 19/19
    scores.push_back(CheckQueryList(tc, DefaultValue));

    // Pass 17/17
    scores.push_back(CheckQueryList(tc, NonStrictMode));

    // Pass 30/30
    scores.push_back(CheckQueryList(tc, Transactions));

    // Pass 13/13
    scores.push_back(CheckQueryList(tc, TableAliases));

    // Pass 28/28
    scores.push_back(CheckQueryList(tc, DDL));

    // Pass 39/40
    scores.push_back(CheckQueryList(tc, MiscBugs));

    // Pass 12/12
    scores.push_back(CheckQueryList(tc, QuotedSchemaObjects));

    // Pass 24/35
    scores.push_back(CheckQueryList(tc, Directives));

    // Pass 43/44
    scores.push_back(CheckQueryList(tc, Range));

    int npass = 0;
    int ntest = 0;
    for (auto it : scores) {
        std::cout << it.stringify() << std::endl;
        npass += it.success;
        ntest += it.total;
    }

    std::cerr << "RESULT: " << npass << "/" << ntest << std::endl;
}

//---------------------------------------------------------------------

TestQueries::TestQueries() {
}

TestQueries::~TestQueries() {
}

static test_mode
string_to_test_mode(const std::string &s)
{
    if (s == "plain")
        return UNENCRYPTED;
    else if (s == "single")
        return SINGLE;
    else if (s == "proxy-plain")
        return PROXYPLAIN;
    else if (s == "enc")
        return ENC;
    else
        thrower() << "unknown test mode " << s;
    return TESTINVALID;
}

void
TestQueries::run(const TestConfig &tc, int argc, char ** argv) {
    switch(argc) {
        case 4:
            // TODO check that argv[3] is a proper int-string
            no_conn = valFromStr(argv[3]);
        case 3:
            control_type = string_to_test_mode(argv[1]);
            test_type = string_to_test_mode(argv[2]);
            break;
        default:
            std::cerr << "Usage:" << std::endl
                << "    .../tests/test queries control-type test-type [num_conn]" << std::endl
                << "Possible control and test types:" << std::endl
                << "    plain" << std::endl
                << "    single" << std::endl
                << "    proxy-plain" << std::endl
                << "    enc" << std::endl
                << "single make connections through EDBProxy" << std::endl
                << "proxy-* makes connections *'s encryption type through the proxy" << std::endl
                << "num_conn is the number of conns made to a single db (default 1)" << std::endl
                << "    for num_conn > 1, control and test should both be proxy-* for valid results" << std::endl;
            return;
    }

    if (no_conn > 1) {
        switch(test_type) {
            case UNENCRYPTED:
            case SINGLE:
            case PROXYPLAIN:
            case ENC:
                // TODO(ccarvalho) check this
                break;
            default:
                std::cerr << "test_type does not exist" << std::endl;
        }
    }


    try {
        TestConfig control_tc = TestConfig();
        control_tc.db = control_tc.db+"_control";

        Connection test_(tc, test_type);
        test = &test_;
        test->execute("CREATE DATABASE IF NOT EXISTS " + tc.db + ";");
        test->execute("USE " + tc.db + ";");

        Connection control_(control_tc, control_type);
        control = &control_;
        control->execute("CREATE DATABASE IF NOT EXISTS " + control_tc.db + ";");
        control->execute("USE " + control_tc.db + ";");

        RunTest(tc);
    } catch (const AbstractException &e) {
        std::cout << e << std::endl;
        throw;
    }
}
