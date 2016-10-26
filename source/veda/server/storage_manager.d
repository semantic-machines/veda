/**
 * процесс отвечающий за хранение
 */
module veda.server.storage_manager;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
    import veda.common.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
    import veda.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.common.log_msg, veda.onto.individual,
           veda.onto.resource;
    import veda.core.storage.lmdb_storage, veda.core.storage.binlog_tools, veda.util.module_info;
    import veda.core.search.vel, veda.common.type;
    import kaleidic.nanomsg.nano;
    import veda.bind.libwebsocketd;
    import veda.server.wslink;
}

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-server", "log", "STORAGE-MANAGER");
    return _log;
}

struct TransactionItem
{
    byte       cmd;
    string     indv_serl;
    string     ticket_id;
    string     event_id;

    Individual indv;

    this(byte _cmd, string _indv_serl, string _ticket_id, string _event_id)
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
        send(tid_subject_manager, CMD_FREEZE, thisTid);
        receive((bool _res) {});
    }
}

public void unfreeze(P_MODULE storage_id)
{
    writeln("UNFREEZE");
    Tid tid_subject_manager = getTid(storage_id);

    if (tid_subject_manager != Tid.init)
    {
        send(tid_subject_manager, CMD_UNFREEZE);
    }
}

public string find(P_MODULE storage_id, string uri)
{
    string res;
    Tid    tid_subject_manager = getTid(P_MODULE.subject_manager);

    if (tid_subject_manager !is Tid.init)
    {
        send(tid_subject_manager, CMD_FIND, uri, thisTid);
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

    send(tid_subject_manager, CMD_BACKUP, "", thisTid);
    receive((string res) { backup_id = res; });

    return backup_id;
}

public ResultCode flush_int_module(P_MODULE f_module, bool is_wait)
{
    ResultCode rc;
    Tid        tid = getTid(f_module);

    if (tid != Tid.init)
    {
        if (is_wait == false)
        {
            send(tid, CMD_COMMIT);
        }
        else
        {
            send(tid, CMD_COMMIT, thisTid);
            receive((bool isReady) {});
        }
        rc = ResultCode.OK;
    }
    return rc;
}

public void flush_ext_module(P_MODULE f_module, long wait_op_id)
{
    Tid tid = getTid(P_MODULE.subject_manager);

    if (tid != Tid.init)
    {
        send(tid, CMD_COMMIT, f_module, wait_op_id);
    }
}

public long unload(P_MODULE storage_id, string queue_name)
{
    Tid  tid   = getTid(storage_id);
    long count = -1;

    if (tid != Tid.init)
    {
        send(tid, CMD_UNLOAD, queue_name, thisTid);
        receive((long _count) { count = _count; });
    }
    return count;
}

public ResultCode put(P_MODULE storage_id, string user_uri, Resources type, string indv_uri, string prev_state, string new_state, string event_id,
                      bool ignore_freeze,
                      out long op_id)
{
    ResultCode rc;
    Tid        tid = getTid(storage_id);

    if (tid != Tid.init)
    {
        send(tid, INDV_OP.PUT, user_uri, indv_uri, prev_state, new_state, event_id, ignore_freeze, thisTid);

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


public void individuals_manager(P_MODULE _storage_id, string db_path, string node_id)
{
    Queue                        individual_queue;

    P_MODULE                     storage_id  = _storage_id;
    string                       thread_name = text(storage_id);

    core.thread.Thread.getThis().name = thread_name;

    LmdbStorage                  storage          = new LmdbStorage(db_path, DBMode.RW, "individuals_manager", log);
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;
    string                       bin_log_name     = get_new_binlog_name(db_path);

    long                         op_id           = storage.last_op_id;
    long                         committed_op_id = 0;

    string                       notify_chanel_url = "tcp://127.0.0.1:9111\0";
    int                          sock;
    bool                         already_notify_chanel = false;
    ModuleInfoFile               module_info;

    try
    {
        if (storage_id == P_MODULE.subject_manager)
        {
            individual_queue = new Queue("individuals-flow", Mode.RW, log);
//            individual_queue.remove_lock();
            individual_queue.open();

            sock = nn_socket(AF_SP, NN_PUB);
            if (sock >= 0)
            {
                if (nn_bind(sock, cast(char *)notify_chanel_url) >= 0)
                {
                    already_notify_chanel = true;
                }
            }
        }

        // SEND ready
        receive((Tid tid_response_reciever)
                {
                    send(tid_response_reciever, true);
                });

        string last_backup_id = "---";

        bool   is_freeze = false;
        bool   is_exit   = false;
        module_info = new ModuleInfoFile(text(storage_id), _log, OPEN_MODE.WRITER);

        if (!module_info.is_ready)
        {
            log.trace("thread [%s] terminated", process_name);
            return;
        }

        while (is_exit == false)
        {
            try
            {
                receive(
                        (byte cmd, P_MODULE f_module, long wait_op_id)
                        {
                            if (cmd == CMD_COMMIT)
                            {
                                string msg = "COMMIT:" ~ text(f_module);
                                int bytes = nn_send(sock, cast(char *)msg, msg.length + 1, 0);
                                log.trace("SEND %d bytes [%s] TO %s, wait_op_id=%d", bytes, msg, notify_chanel_url, wait_op_id);
                            }
                        },
                        (byte cmd)
                        {
                            if (cmd == CMD_COMMIT)
                            {
                                storage.flush(1);
                                committed_op_id = op_id;
                                module_info.put_info(op_id, committed_op_id);
                                //log.trace ("FLUSH op_id=%d committed_op_id=%d", op_id, committed_op_id);
                            }
                            else if (cmd == CMD_UNFREEZE)
                            {
                                is_freeze = false;
                            }
                        },
                        (byte cmd, Tid tid_response_reciever)
                        {
                            if (cmd == CMD_COMMIT)
                            {
                                committed_op_id = op_id;
                                storage.flush(1);
                                send(tid_response_reciever, true);
                                module_info.put_info(op_id, committed_op_id);
                                log.trace("FLUSH op_id=%d committed_op_id=%d", op_id, committed_op_id);
                            }
                            else if (cmd == CMD_FREEZE)
                            {
                                is_freeze = true;
                                send(tid_response_reciever, true);
                            }
                            else if (cmd == CMD_EXIT)
                            {
                                is_exit = true;
                                writefln("[%s] recieve signal EXIT", text(storage_id));
                                send(tid_response_reciever, true);
                            }

                            else if (cmd == CMD_NOP)
                                send(tid_response_reciever, true);
                            else
                                send(tid_response_reciever, false);
                        },
                        (byte cmd, string key, string msg)
                        {
                            if (cmd == CMD_PUT_KEY2SLOT)
                            {
                                storage.put(key, msg, -1);
                            }
                        },
                        (byte cmd, string arg, Tid tid_response_reciever)
                        {
                            if (cmd == CMD_FIND)
                            {
                                string res = storage.find(arg);
                                //writeln("@FIND msg=", msg, ", $res = ", res);
                                send(tid_response_reciever, arg, res, thisTid);
                                return;
                            }
                            else if (cmd == CMD_UNLOAD)
                            {
                                long count;
                                Queue queue = new Queue(arg, Mode.RW, log);

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
                                    writeln("store_thread:CMD_UNLOAD: not open queue");

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
                        (INDV_OP cmd, string user_uri, string indv_uri, string prev_state, string new_state, string event_id, bool ignore_freeze,
                         Tid tid_response_reciever)
                        {
                            ResultCode rc = ResultCode.Not_Ready;

                            if (!ignore_freeze && is_freeze && cmd == INDV_OP.PUT)
                                send(tid_response_reciever, rc, thisTid);

                            try
                            {
                                if (cmd == INDV_OP.PUT)
                                {
                                    string new_hash;
                                    log.trace ("storage_manager:PUT %s", indv_uri);
                                    writeln ("@1");
                                    if (storage.update_or_create(indv_uri, new_state, op_id, new_hash) == 0)
                                    {
                                    writeln ("@2");
                                        rc = ResultCode.OK;
                                        op_id++;
                                        set_subject_manager_op_id(op_id);
                                    }
                                    else
                                    {
                                    writeln ("@3");
                                        rc = ResultCode.Fail_Store;
                                    }

                                    writeln ("@4");
                                    send(tid_response_reciever, rc, thisTid);

                                    writeln ("@5");
                                    if (rc == ResultCode.OK)
                                    {
                                    writeln ("@6");
                                    	
                                        module_info.put_info(op_id, committed_op_id);
                                    writeln ("@6.1");

                                        bin_log_name = write_in_binlog(new_state, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_path);
                                    writeln ("@6.2 storage_id=", storage_id);

                                        if (storage_id == P_MODULE.subject_manager)
                                        {
                                    writeln ("@6.3");
                                            Individual imm;
                                            imm.uri = text(op_id);
                                            imm.addResource("cmd", Resource(cmd));

                                            if (user_uri !is null && user_uri.length > 0)
                                                imm.addResource("user_uri", Resource(DataType.String, user_uri));

                                            imm.addResource("new_state", Resource(DataType.String, new_state));

                                            if (prev_state !is null && prev_state.length > 0)
                                                imm.addResource("prev_state", Resource(DataType.String, prev_state));

                                            if (event_id !is null && event_id.length > 0)
                                                imm.addResource("event_id", Resource(DataType.String, event_id));

                                            imm.addResource("op_id", Resource(op_id));

                                            writeln ("*imm=[", imm, "]");

                                    writeln ("@6.4");
                                            string cbor = individual2cbor(&imm);
                                            //writeln("*cbor.length=", cbor.length);

                                    writeln ("@6.5");
                                            individual_queue.push(cbor);
                                            string msg_to_modules = indv_uri ~ ";" ~ text(op_id) ~ "\0";
                                            int bytes = nn_send(sock, cast(char *)msg_to_modules, msg_to_modules.length, 0);
//                                            log.trace("SEND %d bytes UPDATE SIGNAL TO %s", bytes, notify_chanel_url);

                                            Tid tid_ccus_chanel = getTid(P_MODULE.ccus_chanel);
                                            if (tid_ccus_chanel !is Tid.init)
                                                send(tid_ccus_chanel, msg_to_modules);
                                        }
                                    writeln ("@e");
                                    }

                                    return;
                                }
                            }
                            catch (Exception ex)
                            {
                                send(tid_response_reciever, ResultCode.Fail_Commit, thisTid);
                                return;
                            }

/*
                        if (cmd == CMD_BACKUP)
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
                        (byte cmd, int arg, bool arg2)
                        {
                            if (cmd == CMD_SET_TRACE)
                                set_trace(arg, arg2);
                        },
                        (Variant v) { writeln(thread_name, "::storage_manager::Received some other type.", v); });
            }
            catch (Throwable ex)
            {
                log.trace("individuals_manager# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
            }
        }
    } finally
    {
        if (module_info !is null)
        {
            module_info.close();
            module_info = null;
        }

        if (individual_queue !is null)
        {
            individual_queue.close();
            individual_queue = null;
        }
    }
}
