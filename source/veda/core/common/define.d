/**
 * define
 */
module veda.core.common.define;

import std.concurrency, std.file, std.stdio, core.atomic;
import veda.core.common.know_predicates, veda.common.type;

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

const string acl_indexes_db_path   = "./data/acl-indexes";
const string attachments_db_path   = "./data/files";
const string docs_onto_path        = "./public/docs/onto";
const string dbs_backup            = "./backup";
const string dbs_data              = "./data";
const string uris_db_path          = "./data/uris";
const string tmp_path              = "./data/tmp";
const string queue_db_path         = "./data/queue";
const string onto_path             = "./ontology";
const string xapian_info_path      = "./data/xapian-info";
const string module_info_path      = "./data/module-info";
const string trails_path           = "./data/trails";
const string logs_path             = "./logs";
const string individuals_db_path0 = "./data/lmdb-individuals";
const string tickets_db_path0     = "./data/lmdb-tickets";

const string main_queue_name       = "individuals-flow";
const string ft_indexer_queue_name = "fulltext_indexer0";

string[]     paths_list            =
[
    tmp_path, logs_path, attachments_db_path, docs_onto_path, dbs_backup, dbs_data, uris_db_path, queue_db_path,
    xapian_info_path, module_info_path, trails_path, acl_indexes_db_path, individuals_db_path0, tickets_db_path0
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

/// id подсистем
public enum SUBSYSTEM : ubyte
{
    NONE              = 0,
    STORAGE           = 1,
    ACL               = 2,
    FULL_TEXT_INDEXER = 4,
    FANOUT_EMAIL      = 8,
    SCRIPTS           = 16,
    FANOUT_SQL        = 32,
    USER_MODULES_TOOL = 64
}

/// id компонентов
public enum COMPONENT : ubyte
{
    /// сохранение индивидуалов
    subject_manager  = 1,

    /// Индексирование прав
    acl_preparer     = 2,

    /// Полнотекстовое индексирование
    fulltext_indexer = 4,

    /// Отправка email
    fanout_email     = 8,

    /// исполнение скриптов, normal priority
    scripts_main     = 16,

    /// Выдача и проверка тикетов
    ticket_manager   = 29,

    /// Выгрузка в sql, низкоприоритетное исполнение
    fanout_sql_lp    = 30,

    /// Выгрузка в sql, высокоприоритетное исполнение
    fanout_sql_np    = 32,

    /// исполнение скриптов, low priority
    scripts_lp       = 33,

    //// long time run scripts
    ltr_scripts      = 34,

    ///////////////////////////////////////

    /// Сбор статистики
    statistic_data_accumulator = 35,

    /// Сохранение накопленных в памяти данных
    commiter                   = 36,

    /// Вывод статистики
    print_statistic            = 37,

    n_channel                  = 38,

    /// Загрузка из файлов
    file_reader                = 40,

    input_queue                = 41,

    user_modules_tool          = 64
}


/// id процессов
public enum P_MODULE : ubyte
{
    ticket_manager             = COMPONENT.ticket_manager,
    subject_manager            = COMPONENT.subject_manager,
    acl_preparer               = COMPONENT.acl_preparer,
    statistic_data_accumulator = COMPONENT.statistic_data_accumulator,
    commiter                   = COMPONENT.commiter,
    print_statistic            = COMPONENT.print_statistic,
    file_reader                = COMPONENT.file_reader,
    n_channel                  = COMPONENT.n_channel,
}

/// id модулей обрабатывающих очередь
public enum MODULE : ubyte
{
    ticket_manager    = COMPONENT.ticket_manager,
    subject_manager   = COMPONENT.subject_manager,
    acl_preparer      = COMPONENT.acl_preparer,
    fulltext_indexer  = COMPONENT.fulltext_indexer,
    scripts_main      = COMPONENT.scripts_main,
    scripts_lp        = COMPONENT.scripts_lp,
    fanout_email      = COMPONENT.fanout_email,
    user_modules_tool = COMPONENT.user_modules_tool,
    ltr_scripts       = COMPONENT.ltr_scripts,
    fanout_sql_np     = COMPONENT.fanout_sql_np,
    fanout_sql_lp     = COMPONENT.fanout_sql_lp,
    input_queue       = COMPONENT.input_queue
}

/// Команды используемые процессами
/// Сохранить
byte CMD_PUT         = 1;

/// Найти
byte CMD_FIND        = 2;

/// Коммит
byte CMD_COMMIT      = 16;

byte CMD_MSG         = 17;

/// Включить/выключить отладочные сообщения
byte CMD_SET_TRACE   = 33;

/// Остановить прием команд на изменение
byte CMD_FREEZE      = 42;

/// Возобновить прием команд на изменение
byte CMD_UNFREEZE    = 43;

byte CMD_EXIT        = 49;

/// Установить
byte CMD_SET         = 50;

/// Убрать
byte CMD_START       = 52;

