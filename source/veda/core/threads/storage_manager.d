/**
 * процесс отвечающий за хранение
 */
module veda.core.threads.storage_manager;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
    import util.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
    import veda.core.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.log_msg, veda.onto.individual,
           veda.onto.resource;
    import veda.core.storage.lmdb_storage, veda.core.storage.binlog_tools;
    import search.vel, veda.type;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "API");
    return _log;
}

/// Команды используемые процессами
enum CMD : byte
{
    /// Найти
    FIND         = 2,
    /// Проверить
    EXAMINE      = 4,

    /// Авторизовать
    AUTHORIZE    = 8,

    /// Коммит
    COMMIT       = 16,

    /// Конец данных
    END_DATA     = 32,

    /// Включить/выключить отладочные сообщения
    SET_TRACE    = 33,

    /// Выгрузить
    UNLOAD       = 34,

    /// Перезагрузить
    RELOAD       = 40,

    /// Backup
    BACKUP       = 41,

    /// Остановить прием команд на изменение
    FREEZE       = 42,

    /// Возобновить прием команд на изменение
    UNFREEZE     = 43,

    /// Сохранить соответствие ключ - слот (xapian)
    PUT_KEY2SLOT = 44,

    /// Удалить
    DELETE       = 46,

    EXIT         = 49,

    /// Установить
    SET          = 50,

    START        = 52,

    STOP         = 53,

    RESUME       = 54,

    PAUSE        = 55,

    WAIT         = 56,

    /// Пустая комманда
    NOP          = 64
}
// ////// ////// ///////////////////////////////////////////

struct TransactionItem
{
    CMD        cmd;
    string     indv_serl;
    string     ticket_id;
    string     event_id;

    Individual indv;

    this(CMD _cmd, string _indv_serl, string _ticket_id, string _event_id)
    {
        cmd       = _cmd;
        indv_serl = _indv_serl;
        ticket_id = _ticket_id;
        event_id  = _event_id;

        int code = cbor2individual(&indv, indv_serl);
        if (code < 0)
        {
            log.trace("ERR:v8d:transaction:cbor2individual [%s]", indv_serl);
        }
    }
}
TransactionItem *[ string ] transaction_buff;
TransactionItem *[] transaction_queue;


string begin_transaction(P_MODULE storage_id)
{
    return "";
}

void commit_transaction(P_MODULE storage_id, string transaction_id)
{
}

void abort_transaction(P_MODULE storage_id, string transaction_id)
{
}


public void freeze(P_MODULE storage_id)
{
    writeln("FREEZE");
    Tid tid_subject_manager = getTid(storage_id);

    if (tid_subject_manager != Tid.init)
    {
        send(tid_subject_manager, CMD.FREEZE, thisTid);
        receive((bool _res) {});
    }
}

public void unfreeze(P_MODULE storage_id)
{
    writeln("UNFREEZE");
    Tid tid_subject_manager = getTid(storage_id);

    if (tid_subject_manager != Tid.init)
    {
        send(tid_subject_manager, CMD.UNFREEZE);
    }
}

public string find(P_MODULE storage_id, string uri)
{
    string res;
    Tid    tid_subject_manager = getTid(P_MODULE.subject_manager);

    if (tid_subject_manager !is Tid.init)
    {
        send(tid_subject_manager, CMD.FIND, uri, thisTid);
        receive((string key, string data, Tid tid)
                {
                    res = data;
                });
    }
    else
        throw new Exception("find [" ~ uri ~ "], !!! NOT FOUND TID=" ~ text(P_MODULE.subject_manager));

    return res;
}

public string backup(P_MODULE storage_id)
{
    string backup_id;

    Tid    tid_subject_manager = getTid(storage_id);

    send(tid_subject_manager, CMD.BACKUP, "", thisTid);
    receive((string res) { backup_id = res; });

    return backup_id;
}

public ResultCode flush(bool is_wait)
{
    ResultCode rc;
    Tid        tid = getTid(P_MODULE.subject_manager);

    if (tid != Tid.init)
    {
        if (is_wait == false)
        {
            send(tid, CMD.COMMIT);
        }
        else
        {
            send(tid, CMD.COMMIT, thisTid);
            receive((bool isReady) {});
        }
        rc = ResultCode.OK;
    }
    return rc;
}

public long unload(P_MODULE storage_id, string queue_name)
{
    Tid  tid   = getTid(storage_id);
    long count = -1;

    if (tid != Tid.init)
    {
        send(tid, CMD.UNLOAD, queue_name, thisTid);
        receive((long _count) { count = _count; });
    }
    return count;
}

public ResultCode put(P_MODULE storage_id, string uri, string cur_state, bool ignore_freeze, out long op_id)
{
    ResultCode rc;
    Tid        tid = getTid(storage_id);

    if (tid != Tid.init)
    {
        send(tid, INDV_OP.PUT, uri, cur_state, ignore_freeze, thisTid);

        receive((ResultCode _rc, Tid from)
                {
                    if (from == getTid(storage_id))
                        rc = _rc;
                    op_id = get_subject_manager_op_id();
                    return true;
                });
    }
    return rc;
}

public ResultCode remove(P_MODULE storage_id, string uri, bool ignore_freeze, out long op_id)
{
    ResultCode rc;
    Tid        tid = getTid(storage_id);

    if (tid != Tid.init)
    {
        send(tid, INDV_OP.REMOVE, uri, ignore_freeze, thisTid);

        receive((ResultCode _rc, Tid from)
                {
                    if (from == getTid(storage_id))
                        rc = _rc;
                    op_id = get_subject_manager_op_id();
                    return true;
                });
    }
    return rc;
}

Queue queue;

shared static ~this()
{
    if (queue !is null)
    {
        queue.close();
        queue = null;
    }
}

public void individuals_manager(string thread_name, string db_path, string node_id)
{
    core.thread.Thread.getThis().name             = thread_name;
    LmdbStorage                  storage          = new LmdbStorage(db_path, DBMode.RW, "individuals_manager");
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;
    string                       bin_log_name     = get_new_binlog_name(db_path);

    long        op_id            = storage.last_op_id;
    long                      commited_op_id = 0;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    string last_backup_id = "---";

    bool   is_freeze = false;

    while (true)
    {
        try
        {
            receive(
                    (CMD cmd)
                    {
                        if (cmd == CMD.COMMIT)
                        {
                            storage.flush(1);
                        }
                        else if (cmd == CMD.UNFREEZE)
                        {
                            is_freeze = false;
                        }
                    },
                    (CMD cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.COMMIT)
                        {
                            storage.flush(1);
                            send(tid_response_reciever, true);
                        }
                        else if (cmd == CMD.FREEZE)
                        {
                            is_freeze = true;
                            send(tid_response_reciever, true);
                        }
                        else if (cmd == CMD.NOP)
                            send(tid_response_reciever, true);
                        else
                            send(tid_response_reciever, false);
                    },
                    (CMD cmd, string key, string msg)
                    {
                        if (cmd == CMD.PUT_KEY2SLOT)
                        {
                            storage.put(key, msg, -1);
                        }
                    },
                    (CMD cmd, string arg, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.FIND)
                        {
                            string res = storage.find(arg);
                            //writeln("@FIND msg=", msg, ", $res = ", res);
                            send(tid_response_reciever, arg, res, thisTid);
                            return;
                        }
                        else if (cmd == CMD.UNLOAD)
                        {
                            long count;
                            Queue queue = new Queue(arg, Mode.RW);

                            if (queue.open(Mode.RW))
                            {
                                bool add_to_queue(string key, string value)
                                {
                                    queue.push(value);
                                    count++;
                                    return true;
                                }

                                storage.get_of_cursor(&add_to_queue);
                                queue.close();
                            }
                            else
                                writeln("store_thread:CMD.UNLOAD: not open queue");

                            send(tid_response_reciever, count);
                        }
                    },
                    (INDV_OP cmd, string uri, bool ignore_freeze, Tid tid_response_reciever)
                    {
                        ResultCode rc = ResultCode.Not_Ready;

                        if (!ignore_freeze && is_freeze && cmd == INDV_OP.REMOVE)
                            send(tid_response_reciever, rc, thisTid);

                        try
                        {
                            if (cmd == INDV_OP.REMOVE)
                            {
                                if (storage.remove(uri) == ResultCode.OK)
                                    rc = ResultCode.OK;
                                else
                                    rc = ResultCode.Fail_Store;

                                send(tid_response_reciever, rc, thisTid);

                                return;
                            }
                        }
                        catch (Exception ex)
                        {
                            send(tid_response_reciever, ResultCode.Fail_Commit, thisTid);
                            return;
                        }
                    },
                    (INDV_OP cmd, string uri, string msg, bool ignore_freeze, Tid tid_response_reciever)
                    {
                        ResultCode rc = ResultCode.Not_Ready;

                        if (!ignore_freeze && is_freeze && cmd == INDV_OP.PUT)
                            send(tid_response_reciever, rc, thisTid);

                        try
                        {
                            if (cmd == INDV_OP.PUT)
                            {
                                string new_hash;

                                if (storage.update_or_create(uri, msg, op_id, new_hash) == 0)
                                {
                                    rc = ResultCode.OK;
                               	    op_id++;								    
								    set_subject_manager_op_id(op_id);
                                }    
                                else
                                    rc = ResultCode.Fail_Store;

                                send(tid_response_reciever, rc, thisTid);

                                if (rc == ResultCode.OK)
                                    bin_log_name = write_in_binlog(msg, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_path);

                                return;
                            }
                        }
                        catch (Exception ex)
                        {
                            send(tid_response_reciever, ResultCode.Fail_Commit, thisTid);
                            return;
                        }

/*
                        if (cmd == CMD.BACKUP)
                        {
                            try
                            {
                                string backup_id;
                                if (msg.length > 0)
                                    backup_id = msg;
                                else
                                    backup_id = storage.find(storage.summ_hash_this_db_id);

                                if (backup_id is null)
                                    backup_id = "0";

                                if (last_backup_id != backup_id)
                                {
                                    Result res = storage.backup(backup_id);
                                    if (res == Result.Ok)
                                    {
                                        size_bin_log = 0;
                                        bin_log_name = get_new_binlog_name(db_path);
                                        last_backup_id = backup_id;
                                    }
                                    else if (res == Result.Err)
                                    {
                                        backup_id = "";
                                    }
                                }
                                send(tid_response_reciever, backup_id);
                            }
                            catch (Exception ex)
                            {
                                send(tid_response_reciever, "");
                            }
                        }

                        else
                        {
                            send(tid_response_reciever, msg, "err in individuals_manager", thisTid);
                        }
 */
                    },
                    (CMD cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD.SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (Variant v) { writeln(thread_name, "::storage_manager::Received some other type.", v); });
        }
        catch (Throwable ex)
        {
            log.trace("individuals_manager# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}
