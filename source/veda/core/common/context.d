/**
 * core API

   Copyright: © 2014-2015 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev

 */
module veda.core.common.context;

private import std.concurrency, std.datetime;
private import veda.common.type, veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.core.common.define, veda.util.container,
               veda.common.logger, veda.core.common.transaction, veda.core.search.vql, veda.core.az.acl;

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

    /// Дата начала действия тикета
    long       start_time;

    /// Дата окончания действия тикета
    long       end_time;

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
    public ResultCode put(bool need_auth, string user_id, string in_key, string in_value, long op_id);
    public string find(bool need_auth, string user_id, string uri, bool return_value = true);
    public ResultCode remove(bool need_auth, string user_id, string in_key);
    public int get_of_cursor(bool delegate(string key, string value) prepare, bool only_ids);
    public void unload_to_queue(string path, string queue_id, bool only_ids);
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

    bool authorize(string uri, Ticket *ticket, ubyte request_acess, bool is_check_for_reload);
    string get_from_individual_storage(string user_uri, string uri);
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
    // *************************************************** external API *********************************** //

//    //////////////////////////////////////////////////// ONTO //////////////////////////////////////////////

    public Logger get_logger();

    public ResultCode commit(Transaction *in_tnx);

    public VQL get_vql();
    public Authorization acl_indexes();

    public OpResult update(long tnx_id, Ticket *ticket, INDV_OP cmd, Individual *indv, bool prepare_events, string event_id,
                                       bool ignore_freeze,
                                       bool is_api_request);

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
    //public Ticket authenticate(string login, string password);

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
                sort_str = порядок сортировки
                db_str = базы данных используемые в запросе
                from = начинать обработку с ..
                top = сколько вернуть положительно авторизованных элементов
                limit = максимальное количество найденных элементов
                prepare_element_event = делегат для дополнительных действий извне

       Returns:
                список авторизованных uri
     */
    public SearchResult get_individuals_ids_via_query(Ticket *ticket, string query_str, string sort_str, string db_str, int from, int top, int limit,
                                                      void delegate(string uri) prepare_element_event,
                                                      bool trace);

    public void reopen_ro_fulltext_indexer_db();
    public void reopen_ro_individuals_storage_db();
    public void reopen_ro_acl_storage_db();
    public void reopen_ro_ticket_manager_db();

    public Storage get_inividuals_storage_r();

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
       Вернуть индивидуала(BINARY OBJECT) по его uri
       Params:
                 ticket = указатель на обьект Ticket
                 uri

       Returns:
                авторизованный индивид в виде строки CBOR
     */
    public string get_individual_as_binobj(Ticket *ticket, string uri, out ResultCode rs);

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
    public OpResult put_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, long transaction_id,
                                   bool ignore_freeze = false,
                                   bool is_api_request = true);

    public OpResult remove_individual(Ticket *ticket, string uri, bool prepareEvents, string event_id, long transaction_id, bool ignore_freeze =
                                          false,
                                      bool is_api_request = true);

    public OpResult add_to_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, long transaction_id,
                                      bool ignore_freeze =
                                          false,
                                      bool is_api_request = true);

    public OpResult set_in_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, long transaction_id,
                                      bool ignore_freeze =
                                          false,
                                      bool is_api_request = true);

    public OpResult remove_from_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id,
                                           long transaction_id,
                                           bool ignore_freeze = false,
                                           bool is_api_request = true);

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
