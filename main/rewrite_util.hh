#pragma once

#include <string>

#include <main/rewrite_main.hh>
#include <main/Analysis.hh>
#include <main/rewrite_ds.hh>
#include <main/schema.hh>

#include <sql_list.h>
#include <sql_table.h>

const bool PRETTY_DEMO = true;
const std::string BOLD_BEGIN = "\033[1m";
const std::string RED_BEGIN = "\033[1;31m";
const std::string GREEN_BEGIN = "\033[1;92m";
const std::string COLOR_END = "\033[0m";

Item *
rewrite(const Item &i, const EncSet &req_enc, Analysis &a);

TABLE_LIST *
rewrite_table_list(const TABLE_LIST * const t, const Analysis &a);

TABLE_LIST *
rewrite_table_list(const TABLE_LIST * const t,
                   const std::string &anon_name);

SQL_I_List<TABLE_LIST>
rewrite_table_list(const SQL_I_List<TABLE_LIST> &tlist, Analysis &a,
                   bool if_exists=false);

List<TABLE_LIST>
rewrite_table_list(List<TABLE_LIST> tll, Analysis &a);

RewritePlan *
gather(const Item &i, Analysis &a);

void
gatherAndAddAnalysisRewritePlan(const Item &i, Analysis &a);

void
optimize(Item ** const i, Analysis &a);

std::vector<std::tuple<std::vector<std::string>, Key::Keytype> >
collectKeyData(const LEX &lex);

std::vector<Create_field *>
rewrite_create_field(const FieldMeta * const fm, Create_field * const f,
                     const Analysis &a);

void
highLevelRewriteKey(const TableMeta &tm, const LEX &seed_lex,
                    LEX *const out_lex, const Analysis &a);

std::string
bool_to_string(bool b);

bool string_to_bool(const std::string &s);

List<Create_field>
createAndRewriteField(Analysis &a, Create_field * const cf,
                      TableMeta *const tm, bool new_table,
                      const std::vector<std::tuple<std::vector<std::string>,
                                        Key::Keytype> >
                          &key_data,
                      List<Create_field> &rewritten_cfield_list);

Item *
encrypt_item_layers(const Item &i, onion o, const OnionMeta &om,
                    const Analysis &a, uint64_t IV = 0);

// FIXME(burrows): Generalize to support any container with next AND end
// semantics.
template <typename T>
std::string vector_join(std::vector<T> v, const std::string &delim,
                        const std::function<std::string(T)> &finalize)
{
    std::string accum;
    for (const auto &it : v) {
        const std::string &element = finalize(static_cast<T>(it));
        accum.append(element);
        accum.append(delim);
    }

    AssignOnce<std::string> output;
    if (accum.length() > 0) {
        output = accum.substr(0, accum.length() - delim.length());
    } else {
        output = accum;
    }

    return output.get();
}

static std::string identity(std::string s) {return s;}

inline std::string
vector_join(std::vector<std::string> v, const std::string &delim)
{
    return vector_join<std::string>(v, delim, identity);
}

std::string
escapeString(const std::unique_ptr<Connect> &c,
             const std::string &escape_me);

void
encrypt_item_all_onions(const Item &i, const FieldMeta &fm,
                        uint64_t IV, Analysis &a, std::vector<Item *> *l);

std::vector<onion>
getOnionIndexTypes();

void
typical_rewrite_insert_type(const Item &i, const FieldMeta &fm,
                            Analysis &a, std::vector<Item *> *l);

void
process_select_lex(const st_select_lex &select_lex, Analysis &a);

void
process_table_list(const List<TABLE_LIST> &tll, Analysis &a);

st_select_lex *
rewrite_select_lex(const st_select_lex &select_lex, Analysis &a);

std::string
getDefaultDatabaseForConnection(const std::unique_ptr<Connect> &c);

bool
retrieveDefaultDatabase(unsigned long long thread_id,
                        const std::unique_ptr<Connect> &c,
                        std::string *const out_name);

std::string terminalEscape(const std::string &s);

void
prettyPrintQuery(const std::string &query);

SECURITY_RATING
determineSecurityRating();

bool
handleActiveTransactionPResults(const ResType &res);

template <typename InType, typename InterimType, typename OutType>
std::function<OutType(InType in)>
fnCompose(std::function<OutType(InterimType)> outer,
          std::function<InterimType(InType)> inner) {

    return [&outer, &inner] (InType in)
    {
        return outer(inner(in));
    };
}

