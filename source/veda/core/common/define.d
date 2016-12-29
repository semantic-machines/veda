/**
 * define
 */
module veda.core.common.define;

import std.concurrency, std.file, std.stdio, core.atomic;
import veda.core.common.know_predicates, veda.util.container;

// variable process_name static mirror of g_process_name
string process_name;
static this()
{
    get_g_process_name();
}
/////////////////////////////// g_process_name //////////////////////////
private shared string g_process_name;
public string get_g_process_name()
{
    process_name = atomicLoad(g_process_name);
    return process_name;
}

public void set_g_process_name(string new_data)
{
    atomicStore(g_process_name, new_data);
    process_name = new_data;
}

long     max_size_of_individual = 1024 * 512;

string[] access_list_predicates = [ "v-s:canCreate", "v-s:canRead", "v-s:canUpdate", "v-s:canDelete" ];

enum CNAME : byte
{
    COUNT_PUT        = 0,
    COUNT_GET        = 1,
    WORKED_TIME      = 2,
    LAST_UPDATE_TIME = 3
}

alias immutable(long)[] const_long_array;

const byte              asObject = 0;
const byte              asArray  = 1;
const byte              asString = 2;

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

const string   attachments_db_path = "./data/files";
const string   docs_onto_path      = "./public/docs/onto";
const string   dbs_backup          = "./backup";
const string   dbs_data            = "./data";
const string   individuals_db_path = "./data/lmdb-individuals";
const string   tickets_db_path     = "./data/lmdb-tickets";
const string   acl_indexes_db_path = "./data/acl-indexes";
const string   queue_db_path       = "./data/queue";
const string   onto_path           = "./ontology";
const string   xapian_info_path    = "./data/xapian-info";
const string   module_info_path    = "./data/module-info";
const string   trails_path         = "./data/trails";
const string   logs_path           = "./logs";

const string[] paths_list          =
[
    logs_path, attachments_db_path, docs_onto_path, dbs_backup, dbs_data, individuals_db_path, tickets_db_path, acl_indexes_db_path, queue_db_path,
    xapian_info_path, module_info_path, trails_path
];

private string[ string ] _xapian_search_db_path;
public string get_xapiab_db_path(string db_name)
{
    if (_xapian_search_db_path.length == 0)
        _xapian_search_db_path =
        [ "base":"data/xapian-search-base", "system":"data/xapian-search-system", "deleted":"data/xapian-search-deleted" ];
    return _xapian_search_db_path.get(db_name, null);
}

public const string xapian_metadata_doc_id = "ItIsADocumentContainingTheNameOfTheFieldTtheNumberOfSlots";
public const int    xapian_db_type         = 1;

void create_folder_struct()
{
    foreach (path; paths_list)
    {
        try
        {
            mkdir(path);
            writeln("create folder: ", path);
        }
        catch (Exception ex)
        {
        }
    }
}
