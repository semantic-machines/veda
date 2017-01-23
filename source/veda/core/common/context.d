/**
 * core API

   Copyright: © 2014-2015 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev

 */
module veda.core.common.context;

private import std.concurrency, std.datetime;
private import veda.common.type, veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.core.common.define, veda.util.container,
               veda.common.logger;

/// Имена процессов
public enum P_MODULE : byte
{
    /// Выдача и проверка тикетов
    ticket_manager  = 0,

    /// Чтение и сохранение индивидуалов
    subject_manager = 1,

    /// Индексирование прав
    acl_preparer    = 2,

    /// Полнотекстовое индексирование
    //xapian_thread_context      = 3,

    /// Полнотекстовое индексирование
    fulltext_indexer           = 4,

    /// Сбор статистики
    statistic_data_accumulator = 5,

    /// исполнение скриптов
    scripts_main               = 6,

    /// Сохранение накопленных данных в полнотекстовом индексаторе
    commiter                   = 7,

    /// Вывод статистики
    print_statistic            = 8,

    /// Загрузка из файлов
    file_reader                = 10,

    zmq_listener               = 11,

    fanout_email               = 12,

    //// data change signal
    fanout_sql                 = 13,

    ltr_scripts                = 14,

    webserver                  = 15,

    n_channel                  = 16,

    ccus_channel               = 17,

    nop                        = 99
}

/**
 * Коды результата выполнения
 */
public enum ResultCode
{
    /// 0
    zero                  = 0,

    /// 200
    OK                    = 200,

    /// 201
    Created               = 201,

    /// 204
    No_Content            = 204,

    /// 400
    Bad_Request           = 400,

    /// 403
    Forbidden             = 403,

    /// 404
    Not_Found             = 404,

    /// 422
    Unprocessable_Entity  = 422,

    /// 429
    Too_Many_Requests     = 429,

    /// 470
    Ticket_not_found      = 470,

    /// 471
    Ticket_expired        = 471,

    /// 472
    Not_Authorized        = 472,

    /// 473
    Authentication_Failed = 473,

    /// 474
    Not_Ready             = 474,

    /// 475
    Fail_Open_Transaction = 475,

    /// 476
    Fail_Commit           = 476,

    /// 477
    Fail_Store            = 477,

    /// 500
    Internal_Server_Error = 500,

    /// 501
    Not_Implemented       = 501,

    /// 503
    Service_Unavailable   = 503,

    Invalid_Identifier    = 904,

    /// 1021
    Disk_Full             = 1021,

    /// 1022
    Duplicate_Key         = 1022,

    /// 1118
    Size_too_large        = 1118,

    /// 4000
    Connect_Error         = 4000
}

public struct OpResult
{
    ResultCode result;
    long       op_id;
}

/**
 * Обьект - сессионный тикет
 */
public struct Ticket
{
    /// ID
    string     id;

    /// Uri пользователя
    string     user_uri;

    /// Код результата, если тикет не валидный != ResultCode.Ok
    ResultCode result;
//    string[] parentUnitIds = new string[ 0 ];

    /// Время жизни тикета в миллисекундах
    long end_time;

    /// Конструктор
    this(Ticket tt)
    {
        id       = tt.id.dup;
        user_uri = tt.user_uri.dup;
        end_time = tt.end_time;
    }

    this(string _id, string _user_uri, long _end_time)
    {
        id       = _id;
        user_uri = _user_uri;
        end_time = _end_time;
    }
}

public struct SearchResult
{
    string[]   result;
    int        count;
    int        estimated;
    int        processed;
    long       cursor;
    ResultCode result_code = ResultCode.Not_Ready;
}

interface Storage
{
    public ResultCode put(string in_key, string in_value, long op_id);
    public string find(string uri, bool return_value = true);
    public int get_of_cursor(bool delegate(string key, string value) prepare);
    public long count_entries();
    public void reopen_db();
    public void close_db();
    public long dump_to_binlog();
}

interface ScriptVM
{
    Script compile(string code);
}

interface Script
{
    void run();
}

/**
 * Внешнее API - Интерфейс
 */
interface Context
{
    string get_name();

    int[ string ] get_key2slot();

//    public bool ft_check_for_reload(void delegate() load);
    public bool acl_check_for_reload(void delegate() load);

    bool authorize(string uri, Ticket *ticket, ubyte request_acess, bool is_check_for_reload);
    string get_from_individual_storage(string uri);
    Onto get_onto();

    public string get_ticket_from_storage(string ticket_id);

    public Ticket *get_systicket_from_storage();

    public Ticket create_new_ticket(string user_id, string duration = "40000", string ticket_id = null);

    public long get_operation_state(P_MODULE thread_id, long wait_op_id);

    @property
    public Ticket sys_ticket(bool is_new = false);

    // *************************************************** external API ? *********************************** //
    ref string[ string ] get_prefix_map();
    void add_prefix_map(ref string[ string ] arg);
    public void stat(byte command_type, ref StopWatch sw) nothrow;
    // *************************************************** external API *********************************** //

//    //////////////////////////////////////////////////// ONTO //////////////////////////////////////////////

    public Logger get_logger();

    version (isServer)
    {
        public string execute(string in_msg);
    }

    public Individual[] get_individuals_via_query(Ticket *ticket, string query_str, bool inner_get = false, int top = 10, int limit = 10000);


    public Individual[ string ] get_onto_as_map_individuals();

    /**
       Aутентификация
       Params:
                login = имя пользователя
                password = хэш пароля

       Returns:
                экземпляр структуры Ticket
     */
    public Ticket authenticate(string login, string password);

    /**
       Доверенная аутентификация
       Params:
                ticket = имя пользователя, входящего в группу [cfg:SuperUser]
                login = имя пользователя, кому будет выдан новый тикет

       Returns:
                экземпляр структуры Ticket
     */
    Ticket get_ticket_trusted(string ticket, string login);

    /**
       Вернуть обьект Ticket по Id
     */
    public Ticket *get_ticket(string ticket_id, bool is_systicket = false);

    /**
       Проверить сессионный билет
     */
    public bool is_ticket_valid(string ticket_id);

    // ////////////////////////////////////////////// INDIVIDUALS IO ////////////////////////////////////////////
    /**
       Вернуть индивидуалов согласно заданному запросу
       Params:
                ticket = указатель на экземпляр Ticket
                query_str = строка содержащая VQL запрос

       Returns:
                список авторизованных uri
     */
    public SearchResult get_individuals_ids_via_query(Ticket *ticket, string query_str, string sort_str, string db_str, int from, int top, int limit);

    public void reopen_ro_fulltext_indexer_db();
    public void reopen_ro_subject_storage_db();
    public void reopen_ro_acl_storage_db();
    public void reopen_ro_ticket_manager_db();

    public void subject_storage_commmit(bool isWait = true);
    public long unload_subject_storage(string queue_name);


    public Storage get_subject_storage_db();

    /**
       Вернуть индивидуала по его uri
       Params:
                 ticket = указатель на обьект Ticket
                 Uri

       Returns:
                авторизованный экземпляр onto.Individual
     */
    public Individual               get_individual(Ticket *ticket, Uri uri);

    /**
       Вернуть список индивидуалов по списку uri
       Params:
                ticket = указатель на обьект Ticket
                uris   = список содержащий заданные uri

       Returns:
                авторизованные экземпляры Individual
     */
    public Individual[] get_individuals(Ticket *ticket, string[] uris);

    /**
       Вернуть индивидуала(CBOR) по его uri
       Params:
                 ticket = указатель на обьект Ticket
                 uri

       Returns:
                авторизованный индивид в виде строки CBOR
     */
    public string get_individual_as_cbor(Ticket *ticket, string uri, out ResultCode rs);

    /**
       Сохранить индивидуал, по указанному uri
       Params:
                 ticket = указатель на обьект Ticket
                 indv   = указатель на экземпляр Individual, сохраняется если !is null
                 uri    = uri, по которому сохраняется индивидула
                 wait_for_indexing = ожидать окончания полнотекстовой индексации

       Returns:
                Код результата операции
     */
    public OpResult put_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, bool ignore_freeze = false,
                                   bool is_api_request = true);

    public OpResult remove_individual(Ticket *ticket, string uri, bool prepareEvents, string event_id, bool ignore_freeze = false,
                                      bool is_api_request = true);

    public OpResult add_to_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, bool ignore_freeze =
                                          false, bool is_api_request = true);

    public OpResult set_in_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, bool ignore_freeze =
                                          false, bool is_api_request = true);

    public OpResult remove_from_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id,
                                           bool ignore_freeze = false, bool is_api_request = true);

    string begin_transaction();
    void commit_transaction(string transaction_id);
    void abort_transaction(string transaction_id);

    // ////////////////////////////////////////////// AUTHORIZATION ////////////////////////////////////////////
    /**
       Вернуть список доступных прав для пользователя на указанномый uri
       Params:
                 ticket = указатель на обьект Ticket
                 uri    = uri субьекта

       Returns:
                байт содержащий установленные биты (type.Access)
     */
    public ubyte get_rights(Ticket *ticket, string uri);


    /**
       Вернуть детализированный список доступных прав для пользователя по указанному uri, список представляет собой массив индивидов
       Params:
                 ticket = указатель на обьект Ticket
                 uri    = uri субьекта
                 trace_acl  = функция делегат, собирающая результат выполнения функции
     */
    public void get_rights_origin_from_acl(Ticket *ticket, string uri,
                                           void delegate(string resource_group, string subject_group, string right) trace_acl);

    /**
       Вернуть список групп в которые входит индивид указанный по uri, список представляет собой индивид
       Params:
                 ticket = указатель на обьект Ticket
                 uri    = uri субьекта
                 trace_group  = функция делегат, собирающая результат выполнения функции
     */
    public void get_membership_from_acl(Ticket *ticket, string uri,
                                        void delegate(string resource_group) trace_group);
    // ////////////////////////////////////////////// TOOLS ////////////////////////////////////////////

    /**
       Ожидать, пока завершится выполнение операции
       Params:
                 module_id = id процесса из перечисления P_MODULE
                 op_id - id операции изменения данных, если 0, то ожидание организуется через внутреннюю очередь модуля
     */

    public bool wait_operation_complete(P_MODULE module_id, long op_id, long timeout = 10_000);

    /**
       Перезагрузить модуль
       Params:
                 thread_id = id процесса из перечисления P_MODULE
     */
    public long restart_module(P_MODULE module_id);

    /**
       Включить/выключить отладочные сообщения
       Params:
                 idx   = id отладочного сообщения (0 - все сообщения)
                 state = true/false
     */
    public void set_trace(int idx, bool state);

    /**
       Количество индивидуалов в базе данных
     */
    public long count_individuals();

    /**
       Выполнить бэкапирование базы данных
     */
    public bool backup(bool to_binlog, int level = 0);

    /**
       Остановить выполнение операций записи, новые команды на запись не принимаются
     */
    public void freeze();

    /**
       Возобновить прием операций записи на выполнение
     */
    public void unfreeze();

    public string get_config_uri();
    public Individual get_configuration();
}

//////////////////////////////////////////////////////////////////////////

import core.atomic;

private shared long count_onto_update = 0;

public void inc_count_onto_update(long delta = 1)
{
    atomicOp !"+=" (count_onto_update, delta);
}

public long get_count_onto_update()
{
    return atomicLoad(count_onto_update);
}

///

private shared long subject_manager_op_id = 0;

public void set_subject_manager_op_id(long data)
{
    atomicStore(subject_manager_op_id, data);
}

public long get_subject_manager_op_id()
{
    return atomicLoad(subject_manager_op_id);
}

///
/*
   private shared long indexer_op_id = 0;

   public void set_indexer_op_id(long data)
   {
    atomicStore(indexer_op_id, data);
   }

   public long get_indexer_op_id()
   {
    return atomicLoad(indexer_op_id);
   }
 */
////

private shared long acl_manager_op_id = 0;

public void set_acl_manager_op_id(long data)
{
    atomicStore(acl_manager_op_id, data);
}

public long get_acl_manager_op_id()
{
    return atomicLoad(acl_manager_op_id);
}

////

//private shared long count_indexed = 0;

//public void set_count_indexed(long data)
//{
//    atomicStore(count_indexed, data);
//}

//public long get_count_indexed()
//{
//    return atomicLoad(count_indexed);
//}

/////////////////////////////// global_systicket //////////////////////////

private shared string systicket_id;
private shared string systicket_user_uri;
private shared long   systicket_end_time;

public Ticket get_global_systicket()
{
    Ticket t;

    t.id       = atomicLoad(systicket_id);
    t.user_uri = atomicLoad(systicket_user_uri);
    t.end_time = atomicLoad(systicket_end_time);
    return t;
}

public void set_global_systicket(Ticket new_data)
{
    atomicStore(systicket_id, new_data.id);
    atomicStore(systicket_user_uri, new_data.user_uri);
    atomicStore(systicket_end_time, new_data.end_time);
}
