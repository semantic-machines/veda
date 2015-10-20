/**
 * define
 */

module veda.core.define;

import util.container;
import std.concurrency, std.file, std.stdio;
import veda.core.know_predicates;

string proccess_name = "";

string[] access_list_predicates = [ veda_schema__canCreate, veda_schema__canRead, veda_schema__canUpdate, veda_schema__canDelete ];

enum CNAME : byte
{
    COUNT_PUT        = 0,
    COUNT_GET        = 1,
    WORKED_TIME      = 2,
    LAST_UPDATE_TIME = 3
}

alias immutable(int)[]   const_int_array;
alias immutable(long)[]  const_long_array;
alias                    Tid[ string ] Tid2Name;
alias immutable Tid2Name Tids;

const byte               asObject = 0;
const byte               asArray  = 1;
const byte               asString = 2;

interface Outer
{
    void put(string data);
}

enum EVENT : byte
{
    CREATE    = 1,
    UPDATE    = 2,
    REMOVE    = 3,
    NONE      = 4,
    ERROR     = 5,
    NOT_READY = 6
}

const string docs_onto_path      = "./public/docs/onto";
const string dbs_backup          = "./backup";
const string dbs_data            = "./data";
const string individuals_db_path = "./data/lmdb-individuals";
const string tickets_db_path     = "./data/lmdb-tickets";
const string acl_indexes_db_path = "./data/acl-indexes";

public const string[ string ] xapian_search_db_path;
static this()
{
    xapian_search_db_path =
    [ "base":"data/xapian-search-base", "system":"data/xapian-search-system", "deleted":"data/xapian-search-deleted" ];
}

public const string xapian_metadata_doc_id = "ItIsADocumentContainingTheNameOfTheFieldTtheNumberOfSlots";
public const int    xapian_db_type         = 1;

void create_folder_struct()
{
    try
    {
        mkdir(dbs_data);
        writeln("create folder: ", dbs_data);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(individuals_db_path);
        writeln("create folder: ", individuals_db_path);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(tickets_db_path);
        writeln("create folder: ", tickets_db_path);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(acl_indexes_db_path);
        writeln("create folder: ", acl_indexes_db_path);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(dbs_backup);
        writeln("create folder: ", dbs_backup);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(docs_onto_path);
        writeln("create folder: ", docs_onto_path);
    }
    catch (Exception ex)
    {
    }
}
