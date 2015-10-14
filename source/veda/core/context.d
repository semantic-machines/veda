/**
 * Внешнее API

   Copyright: © 2014 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev

 */
module veda.core.context;

private import std.concurrency, std.datetime;

private import type;
private import util.container;
private import search.vel;
private import veda.onto.onto, veda.onto.individual, veda.onto.resource;
private import veda.core.define;

private import bind.v8d_header;

/// Имена процессов
public enum P_MODULE : byte
{
    /// Выдача и проверка тикетов
    ticket_manager             = 0,

    /// Чтение и сохранение индивидуалов
    subject_manager            = 1,

    /// Индексирование прав, проверка прав
    acl_manager                = 2,

    /// Полнотекстовое индексирование
    xapian_thread_context      = 3,

    /// Полнотекстовое индексирование
    fulltext_indexer           = 4,

    /// Сбор статистики
    statistic_data_accumulator = 5,

    /// Запуск внешних скриптов
    condition                  = 6,

    /// Сохранение накопленных данных в полнотекстовом индексаторе
    commiter                   = 7,

    /// Вывод статистики
    print_statistic            = 8,

    /// Межпроцессные сигналы
    interthread_signals        = 9,

    /// Загрузка из файлов
    file_reader                = 10,

    zmq_listener               = 11,
    
    fanout					   = 12,	
    
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

    /// 1021
    Disk_Full             = 1021,

    /// 1022
    Duplicate_Key         = 1022
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
    immutable this(string _id, string _user_uri, long _end_time)
    {
        id       = _id;
        user_uri = _user_uri;
        end_time = _end_time;
    }

    /// Создание $(D immutable) копии
    immutable(Ticket) idup()
    {
        immutable(Ticket) result = immutable Ticket(id, user_uri, end_time);
        return result;
    }
}

/**
 * Внешнее API - Интерфейс
 */
interface Context
{
    string get_name();

    ScriptVM get_ScriptVM();

    Tid getTid(P_MODULE tid_name);

    @property search.vql.VQL vql();

    int[ string ] get_key2slot();

//    void store_subject(Subject ss, bool prepareEvents = true);
    public bool check_for_reload(string interthread_signal_id, void delegate() load);
    public bool ft_check_for_reload(void delegate() load);

//    /////////////////////////////////////////// <- oykumena -> ///////////////////////////////////////////////

    void push_signal(string key, long value);
    void push_signal(string key, string value);
    long look_integer_signal(string key);
    string look_string_signal(string key);
    void set_reload_signal_to_local_thread(string interthread_signal_id);
    bool authorize(string uri, Ticket *ticket, ubyte request_acess);
    Individual[] get_individuals_via_query(Ticket *ticket, string query_str);
    string get_individual_from_storage(string uri);
    Onto get_onto();

    // *************************************************** external API ? *********************************** //
    ref string[ string ] get_prefix_map();
    void add_prefix_map(ref string[ string ] arg);
    long get_last_update_time();

    // *************************************************** external API *********************************** //
    /**
       Выполнить скрипт
       Params:
                str = строка содержащая скрипт

       Returns:
                string[ 2 ], [0] - результат выполнения, [1] - не используется
     */
    public string[ 2 ] execute_script(string str);

//    //////////////////////////////////////////////////// ONTO //////////////////////////////////////////////

    public Individual[ string ] get_onto_as_map_individuals();

// //////////////////////////////////////////////////// TICKET //////////////////////////////////////////////
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
       Вернуть обьект Ticket по Id
     */
    public Ticket *get_ticket(string ticket_id);

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
    public immutable(string)[] get_individuals_ids_via_query(Ticket * ticket, string query_str, string sort_str, string db_str = null);

    public void reopen_ro_fulltext_indexer_db();
    public void reopen_ro_subject_storage_db();
    public void reopen_ro_acl_storage_db();

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
     * Вернуть список индивидуалов(CBOR) по списку uri
       Params:
                ticket = указатель на обьект Ticket
                uris   = список содержащий заданные uri

       Returns:
                авторизованные индивиды в виде массива CBOR строк
     */
    public immutable(string)[] get_individuals_as_cbor(Ticket * ticket, string[] uris);

    /**
     * Сохранить индивидуал
       Params:
                 ticket = указатель на обьект Ticket
                 indv   = указатель на экземпляр Individual, сохраняется если !is null
                 ss_as_cbor = индивидуал в виде строки, сохраняется если $(D indv is null)
                 wait_for_indexing = ожидать окончания полнотекстовой индексации
                 event_id = указывается при сохранения индивида из скриптов (если идет обработка вызванная срабатыванием storage event)

       Returns:
                Код результата операции
     */
    public ResultCode store_individual(CMD cmd, Ticket *ticket, Individual *indv, string ss_as_cbor, bool wait_for_indexing,
                                       bool prepareEvents = true,
                                       string event_id = null);

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
    public ResultCode put_individual(Ticket *ticket, string uri, Individual individual, bool wait_for_indexing);

    public ResultCode add_to_individual(Ticket *ticket, string uri, Individual individual, bool wait_for_indexing);
    public ResultCode set_in_individual(Ticket *ticket, string uri, Individual individual, bool wait_for_indexing);
    public ResultCode remove_from_individual(Ticket *ticket, string uri, Individual individual, bool wait_for_indexing);

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
       Вернуть детализированный список доступных прав для пользователя на указанномый uri
       Params:
                 ticket = указатель на обьект Ticket
                 uri    = uri субьекта
                 trace  = функция делегат, собирающая результат выполнения функции
     */
    public void get_rights_origin(Ticket *ticket, string uri,
                                  void delegate(string resource_group, string subject_group, string right) trace);

    // ////////////////////////////////////////////// TOOLS ////////////////////////////////////////////

    /**
       Ожидать, пока освободится процесс
       Params:
                 thread_id = id процесса из перечисления P_MODULE
     */
    public void wait_thread(P_MODULE thread_id);

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
    public bool backup(int level = 0);

    /**
       Остановить выполнение операций записи, новые команды на запись не принимаются
     */
    public void freeze();

    /**
       Возобновить прием операций записи на выполнение
     */
    public void unfreeze();

    public Individual getConfiguration();
}

import core.atomic;

private shared int count_put = 0;

public void inc_count_put(int delta = 1)
{
    atomicOp !"+=" (count_put, delta);
}

public int get_count_put()
{
    return atomicOp !"+" (count_put, 0);
}

private shared int count_indexed = 0;

public void inc_count_indexed(int delta = 1)
{
    atomicOp !"+=" (count_indexed, delta);
}

public int get_count_indexed()
{
    return atomicOp !"+" (count_indexed, 0);
}
