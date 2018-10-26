/**
 * Межмодульное API

   Copyright: © 2014-2018 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev

 */
module veda.core.common.context;

private import std.concurrency, std.datetime, std.outbuffer;
private import veda.common.type, veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.core.common.define, veda.util.container;
private import veda.common.logger, veda.core.common.transaction;
private import veda.search.common.isearch, veda.util.module_info, veda.storage.common, veda.storage.storage, veda.authorization.authorization;

alias MODULES_MASK = long;
const ALL_MODULES  = 0;

interface Context
{
    Storage get_storage();

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

    public Search get_vql();
    public void set_vql(Search in_vql);

    public Authorization get_az();
    public void set_az(Authorization in_az);

    public OpResult update(string src, long tnx_id, Ticket *ticket, INDV_OP cmd, Individual *indv, string event_id, MODULES_MASK assigned_subsystems,
                           OptFreeze opt_freeze, OptAuthorize opt_request);

    public Individual[] get_individuals_via_query(string user_uri, string query_str, OptAuthorize op_auth, int top = 10, int limit = 10000);

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
    public SearchResult get_individuals_ids_via_query(string user_id, string query_str, string sort_str, string db_str, int from, int top, int limit,
                                                      OptAuthorize op_auth, bool trace);

    public void reopen_ro_fulltext_indexer_db();
    public void reopen_ro_individuals_storage_db();
    public void reopen_ro_acl_storage_db();

    /**
       Вернуть индивидуала по его uri
       Params:
                 Uri

       Returns:
                НЕ ПРОШЕДШИЙ ПРОЦЕДУРУ авторизации экземпляр onto.Individual
     */
    public Individual               get_individual(Uri uri);

    // ////////////////////////////////////////////// TOOLS ////////////////////////////////////////////

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
