/**
 * core API

   Copyright: © 2014-2017 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev

 */
module veda.core.common.context;

private import std.concurrency, std.datetime;
private import veda.common.type, veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.core.common.define, veda.util.container,
               veda.common.logger, veda.core.common.transaction, veda.core.search.vql, veda.core.az.acl, veda.util.module_info, veda.storage.common, veda.storage.storage;

alias MODULES_MASK = long;
const ALL_MODULES  = 0;

public struct SearchResult
{
    string[]   result;
    int        count;
    int        estimated;
    int        processed;
    long       cursor;
    ResultCode result_code = ResultCode.Not_Ready;
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
	Storage get_storage ();

    string get_name();

    Onto get_onto();

    public long get_operation_state(MODULE module_id, long wait_op_id);

    @property
    public Ticket sys_ticket(bool is_new = false);

    // *************************************************** external API ? *********************************** //
    ref string[ string ] get_prefix_map();
    void add_prefix_map(ref string[ string ] arg);
    // *************************************************** external API *********************************** //

//    //////////////////////////////////////////////////// ONTO //////////////////////////////////////////////

    public Logger get_logger();

    public ResultCode commit(Transaction *in_tnx, OptAuthorize opt_authorize = OptAuthorize.YES);

    public VQL get_vql();

    public OpResult update(long tnx_id, Ticket *ticket, INDV_OP cmd, Individual *indv, string event_id, MODULES_MASK assigned_subsystems,
                           OptFreeze opt_freeze, OptAuthorize opt_request);

    public Individual[] get_individuals_via_query(Ticket *ticket, string query_str, OptAuthorize op_auth, int top = 10, int limit = 10000);


    public Individual[ string ] get_onto_as_map_individuals();

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
                                                      void delegate(string uri) prepare_element_event, OptAuthorize op_auth,
                                                      bool trace);

    public void reopen_ro_fulltext_indexer_db();
    public void reopen_ro_individuals_storage_db();
    public void reopen_ro_acl_storage_db();

    /**
       Вернуть индивидуала по его uri
       Params:
                 ticket = указатель на обьект Ticket
                 Uri

       Returns:
                авторизованный экземпляр onto.Individual
     */
    public Individual               get_individual(Ticket *ticket, Uri uri, OptAuthorize opt_authorize = OptAuthorize.YES);

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
       Сохранить индивидуал, по указанному uri
       Params:
                 ticket = указатель на обьект Ticket
                 indv   = указатель на экземпляр Individual, сохраняется если !is null
                 uri    = uri, по которому сохраняется индивидула
                 wait_for_indexing = ожидать окончания полнотекстовой индексации

       Returns:
                Код результата операции
     */
    public OpResult put_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                   OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES);

    public OpResult remove_individual(Ticket *ticket, string uri, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                      OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES);

    public OpResult add_to_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                      OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES);

    public OpResult set_in_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                      OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES);

    public OpResult remove_from_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                           OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES);

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
                                           void delegate(string resource_group, string subject_group, string right) trace_acl, void delegate(string log) trace_info);

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
