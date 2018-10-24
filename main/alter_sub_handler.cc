#include <main/alter_sub_handler.hh>
#include <main/rewrite_util.hh>
#include <main/dispatcher.hh>
#include <parser/lex_util.hh>
#include <util/enum_text.hh>

// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//     These handlers expect a LEX that they
//            can update in place.
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


class AddColumnSubHandler : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *lex, const Preamble &preamble)
            const
    {
        TableMeta &tm = a.getTableMeta(preamble.dbname, preamble.table);

        // collect the keys (and their types) as they may affect the onion
        // layout we use
        const auto &key_data = collectKeyData(*lex);

        // Create *Meta objects.
        auto add_it =
            List_iterator<Create_field>(lex->alter_info.create_list);
        lex->alter_info.create_list =
            accumList<Create_field>(add_it,
                [&a, &tm, &key_data] (List<Create_field> out_list,
                                      Create_field *cf)
            {
                    return createAndRewriteField(a, cf, &tm, false, key_data,
                                                 out_list);
            });

        return lex;
    }
};

class DropColumnSubHandler : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *lex, const Preamble &preamble)
            const
    {
        // Get the column drops.
        auto drop_it =
            List_iterator<Alter_drop>(lex->alter_info.drop_list);
        List<Alter_drop> col_drop_list =
            filterList<Alter_drop>(drop_it,
                [](Alter_drop *const adrop)
        {
            return Alter_drop::COLUMN == adrop->type;
        });

        // Rewrite and Remove.
        drop_it = List_iterator<Alter_drop>(col_drop_list);
        lex->alter_info.drop_list =
            accumList<Alter_drop>(drop_it,
                [preamble, &a, this] (List<Alter_drop> out_list,
                                            Alter_drop *adrop)
        {
            TableMeta const &tm =
                a.getTableMeta(preamble.dbname, preamble.table);
            FieldMeta const &fm = a.getFieldMeta(tm, adrop->name);
            List<Alter_drop> lst = this->rewrite(fm, adrop);
            out_list.concat(&lst);
            a.deltas.push_back(std::unique_ptr<Delta>(
                                            new DeleteDelta(fm, tm)));
            return out_list; /* lambda */
        });

        return lex;
    }

    List<Alter_drop> rewrite(FieldMeta const &fm, Alter_drop *adrop) const
    {
        List<Alter_drop> out_list;
        THD *const thd = current_thd;

        // Rewrite each onion column.
        for (const auto &om_it : fm.getChildren()) {
            Alter_drop * const new_adrop = adrop->clone(thd->mem_root);
            OnionMeta *const om = om_it.second.get();
            new_adrop->name =
                thd->strdup(om->getAnonOnionName().c_str());
            out_list.push_back(new_adrop);
        }

        // Rewrite the salt column.
        if (fm.getHasSalt()) {
            Alter_drop * const new_adrop = adrop->clone(thd->mem_root);
            new_adrop->name =
                thd->strdup(fm.getSaltName().c_str());
            out_list.push_back(new_adrop);
        }

        return out_list;
    }
};

class ChangeColumnSubHandler : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *lex, const Preamble &preamble)
            const
    {
        FAIL_TextMessageError("implement ChangeColumnSubHandler");
    }
};

class ForeignKeySubHandler : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *lex, const Preamble &preamble)
            const
    {
        FAIL_TextMessageError("implement ForeignKeySubHandler");
    }
};

class AddIndexSubHandler : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *lex, const Preamble &preamble)
            const
    {
        TableMeta const &tm =
            a.getTableMeta(preamble.dbname, preamble.table);

        highLevelRewriteKey(tm, *lex, lex, a);

        return lex;
    }
};

class DropIndexSubHandler : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *lex, const Preamble &preamble)
            const
    {
        TableMeta const &tm =
            a.getTableMeta(preamble.dbname, preamble.table);

        // Get the key drops.
        auto drop_it =
            List_iterator<Alter_drop>(lex->alter_info.drop_list);
        List<Alter_drop> key_drop_list =
            filterList<Alter_drop>(drop_it,
                [](Alter_drop *const adrop)
                {
                    return Alter_drop::KEY == adrop->type;
                });

        // Rewrite.
        drop_it =
            List_iterator<Alter_drop>(key_drop_list);
        lex->alter_info.drop_list =
            accumList<Alter_drop>(drop_it,
                [preamble, &tm, &a, this]
                    (List<Alter_drop> out_list, Alter_drop *adrop)
                {
                        List<Alter_drop> lst =
                            this->rewrite(a, adrop, preamble.table);
                        out_list.concat(&lst); 
                        return out_list;
                });

        return lex;
    }

    List<Alter_drop> rewrite(const Analysis &a, Alter_drop *adrop,
                             const std::string &table) const
    {
        // Rewrite the Alter_drop data structure.
        List<Alter_drop> out_list;
        const std::vector<onion> key_onions = getOnionIndexTypes();
        for (auto onion_it : key_onions) {
            const onion o = onion_it;
            // HACK.
            if (oPLAIN == o) {
                continue;
            }
            Alter_drop *const new_adrop =
                adrop->clone(current_thd->mem_root);
            new_adrop->name =
                make_thd_string(a.getAnonIndexName(a.getDatabaseName(),
                                                   table, adrop->name, o));

            out_list.push_back(new_adrop);
        }

        if (equalsIgnoreCase(adrop->name, "PRIMARY")) {
            if (out_list.elements > 0) {
                List<Alter_drop> abridged_out_list;
                Alter_drop *const new_adrop =
                    adrop->clone(current_thd->mem_root);
                new_adrop->name = make_thd_string("PRIMARY");

                abridged_out_list.push_back(new_adrop);
                return abridged_out_list;
            }
        }
        return out_list;
    }
};

class DisableOrEnableKeys : public AlterSubHandler {
    virtual LEX *
        rewriteAndUpdate(Analysis &a, LEX *const lex,
                         const Preamble &preamble) const
    {
        return lex;
    }
};

LEX *AlterSubHandler::
transformLex(Analysis &a, LEX *const lex) const
{
    const std::string &db = lex->select_lex.table_list.first->db;
    TEST_DatabaseDiscrepancy(db, a.getDatabaseName());
    const Preamble preamble(db,
                            lex->select_lex.table_list.first->table_name);
    return this->rewriteAndUpdate(a, lex, preamble);
}

AlterDispatcher *buildAlterSubDispatcher() {
    AlterDispatcher *dispatcher = new AlterDispatcher();
    AlterSubHandler *h;

    h = new AddColumnSubHandler();
    dispatcher->addHandler(ALTER_ADD_COLUMN, h);

    h = new DropColumnSubHandler();
    dispatcher->addHandler(ALTER_DROP_COLUMN, h);

    h = new ChangeColumnSubHandler();
    dispatcher->addHandler(ALTER_CHANGE_COLUMN, h);

    h = new ForeignKeySubHandler();
    dispatcher->addHandler(ALTER_FOREIGN_KEY, h);

    h = new AddIndexSubHandler();
    dispatcher->addHandler(ALTER_ADD_INDEX, h);

    h = new DropIndexSubHandler();
    dispatcher->addHandler(ALTER_DROP_INDEX, h);

    h = new DisableOrEnableKeys();
    dispatcher->addHandler(ALTER_KEYS_ONOFF, h);

    return dispatcher;
}

