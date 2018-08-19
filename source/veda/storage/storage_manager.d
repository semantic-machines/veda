/**
 * хранение индивидов
 */
module veda.storage.storage_manager;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string, std.digest.ripemd, std.bigint;
    import veda.common.logger, veda.core.util.utils, veda.util.queue;
    import veda.core.common.context, veda.core.common.define, veda.core.common.log_msg, veda.onto.individual, veda.onto.resource;
    import veda.storage.binlog_tools, veda.util.module_info;
    import veda.core.search.vel, veda.common.type, veda.core.common.transaction, veda.storage.common;
    import kaleidic.nanomsg.nano, veda.util.properd;
    import veda.util.properd;
    import veda.storage.lmdb.lmdb_driver, veda.storage.lmdb.lmdb_storage;
    import veda.storage.tarantool.tarantool_driver;
}

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-mstorage", "log", "STORAGE-MANAGER");
    return _log;
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

public ResultCode save(P_MODULE storage_id, OptAuthorize opt_request, immutable (TransactionItem)[] _ti, long tnx_id, OptFreeze opt_freeze,
                       out long op_id)
{
    ResultCode rc;
    Tid        tid = getTid(storage_id);

    if (tid != Tid.init)
    {
        send(tid, opt_request, _ti, tnx_id, opt_freeze, thisTid);

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

public ResultCode save(P_MODULE storage_id, OptAuthorize opt_request, INDV_OP cmd, string user_uri, string indv_uri, string prev_binobj,
                       string new_binobj,
                       long update_counter,
                       string event_id, long tnx_id, long assigned_subsystems, OptFreeze opt_freeze,
                       out long op_id)
{
    ResultCode rc;
    Tid        tid = getTid(storage_id);

    if (tid != Tid.init)
    {
        immutable(TransactionItem) ti = immutable TransactionItem(cmd, user_uri, indv_uri, prev_binobj, new_binobj, update_counter, event_id, false,
                                                                  false, assigned_subsystems);

        send(tid, opt_request, [ ti ], tnx_id, opt_freeze, thisTid);

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

public void individuals_manager(P_MODULE _storage_id, string node_id)
{
    Queue                        individual_queue;
    Queue                        uris_queue;

    P_MODULE                     storage_id  = _storage_id;
    string                       thread_name = text(storage_id);

    core.thread.Thread.getThis().name = thread_name;

    KeyValueDB                   storage = null;
    string                       db_name;

    string[ string ] properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    if (tarantool_url !is null)
    {
        if (_storage_id == P_MODULE.subject_manager)
        {
            storage = new TarantoolDriver(log, "individuals", 512);
            db_name = individuals_db_path;
        }
        else if (_storage_id == P_MODULE.ticket_manager)
        {
            storage = new TarantoolDriver(log, "tickets", 513);
            db_name = tickets_db_path;
        }
    }
    else
    {
        if (_storage_id == P_MODULE.subject_manager)
        {
            storage = new LmdbDriver(individuals_db_path, DBMode.RW, "individuals_manager", log);
            db_name = individuals_db_path;
        }
        else if (_storage_id == P_MODULE.ticket_manager)
        {
            storage = new LmdbDriver(tickets_db_path, DBMode.RW, "ticket_manager", log);
            db_name = tickets_db_path;
        }
    }

    create_folder_struct();

    long count = storage.count_entries();

    log.trace("COUNT INDIVIDUALS=%d", count);

    int            size_bin_log         = 0;
    int            max_size_bin_log     = 10_000_000;
    string         bin_log_name         = get_new_binlog_name(db_name);
    long           last_reopen_rw_op_id = 0;
    int            max_count_updates    = 10_000;

    long           op_id           = storage.get_last_op_id;
    long           committed_op_id = 0;

    string         notify_channel_url = "tcp://127.0.0.1:9111\0";
    int            sock;
    bool           already_notify_channel = false;
    ModuleInfoFile module_info;

    try
    {
        if (storage_id == P_MODULE.subject_manager)
        {
            individual_queue = new Queue(queue_db_path, "individuals-flow", Mode.RW, log);
            individual_queue.open();

            uris_queue = new Queue(uris_db_path, "uris-db", Mode.RW, log);
            uris_queue.open();

            sock = nn_socket(AF_SP, NN_PUB);
            if (sock >= 0)
            {
                if (nn_bind(sock, cast(char *)notify_channel_url) >= 0)
                {
                    already_notify_channel = true;
                }
            }
        }

        bool is_freeze = false;
        bool is_exit   = false;
        module_info = new ModuleInfoFile(text(storage_id), _log, OPEN_MODE.WRITER);

        if (!module_info.is_ready)
        {
            writefln("thread [%s] terminated", process_name);
            // SEND ready
            receive((Tid tid_response_reciever)
                    {
                        send(tid_response_reciever, false);
                    });
            core.thread.Thread.sleep(dur!("seconds")(1));

            return;
        }

        // SEND ready
        receive((Tid tid_response_reciever)
                {
                    send(tid_response_reciever, true);
                });

        while (is_exit == false)
        {
            try
            {
                receive(
                        (byte cmd, P_MODULE f_module, string _msg)
                        {
                            if (cmd == CMD_MSG)
                            {
                                string msg = "MSG:" ~ text(f_module) ~ ":" ~ _msg;
                                int bytes = nn_send(sock, cast(char *)msg, msg.length + 1, 0);
                                log.trace("SEND %d bytes [%s] TO %s", bytes, msg, notify_channel_url);
                            }
                        },
                        (byte cmd, P_MODULE f_module, long wait_op_id)
                        {
                            if (cmd == CMD_COMMIT)
                            {
                                string msg = "MSG:" ~ text(f_module) ~ ":COMMIT";
                                int bytes = nn_send(sock, cast(char *)msg, msg.length + 1, 0);
                                log.trace("SEND %d bytes [%s] TO %s, wait_op_id=%d", bytes, msg, notify_channel_url, wait_op_id);
                            }
                        },
                        (byte cmd)
                        {
                            if (cmd == CMD_COMMIT)
                            {
                                if (committed_op_id != op_id)
                                {
                                    storage.flush(1);

                                    if (last_reopen_rw_op_id == 0)
                                        last_reopen_rw_op_id = op_id;

                                    if (op_id - last_reopen_rw_op_id > max_count_updates)
                                    {
                                        log.trace("REOPEN RW DATABASE, op_id=%d", op_id);
                                        storage.close();
                                        storage.open();
                                        last_reopen_rw_op_id = op_id;
                                    }

                                    committed_op_id = op_id;
                                    module_info.put_info(op_id, committed_op_id);
                                }
                                //log.trace ("FLUSH op_id=%d committed_op_id=%d", op_id, committed_op_id);
                            }
                            else if (cmd == CMD_UNFREEZE)
                            {
                                log.trace("UNFREEZE");
                                is_freeze = false;
                            }
                        },
                        (byte cmd, Tid tid_response_reciever)
                        {
                            if (cmd == CMD_COMMIT)
                            {
                                committed_op_id = op_id;
                                storage.flush(1);

                                if (last_reopen_rw_op_id == 0)
                                    last_reopen_rw_op_id = op_id;

                                if (op_id - last_reopen_rw_op_id > max_count_updates)
                                {
                                    log.trace("REOPEN RW DATABASE, op_id=%d", op_id);
                                    storage.close();
                                    storage.open();
                                    last_reopen_rw_op_id = op_id;
                                }

                                send(tid_response_reciever, true);
                                module_info.put_info(op_id, committed_op_id);
                                log.trace("FLUSH op_id=%d committed_op_id=%d", op_id, committed_op_id);
                            }
                            else if (cmd == CMD_FREEZE)
                            {
                                log.trace("FREEZE");
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
                        (byte cmd, string arg, Tid tid_response_reciever)
                        {
                            if (cmd == CMD_FIND)
                            {
                                string res = storage.find(arg);
                                //writeln("@FIND msg=", msg, ", $res = ", res);
                                send(tid_response_reciever, arg, res, thisTid);
                                return;
                            }
                        },
                        (OptAuthorize opt_request, immutable(TransactionItem)[] tiz, long tnx_id, OptFreeze opt_freeze, Tid tid_response_reciever)
                        {
                            ResultCode rc = ResultCode.Not_Ready;
                            if (tiz.length == 0)
                            {
                                rc = ResultCode.No_Content;
                                send(tid_response_reciever, rc, thisTid);
                                return;
                            }

                            immutable TransactionItem ti = tiz[ 0 ];

                            if (opt_freeze == OptFreeze.NONE && is_freeze && ti.cmd == INDV_OP.PUT)
                                send(tid_response_reciever, rc, thisTid);

                            try
                            {
                                if (ti.cmd == INDV_OP.REMOVE)
                                {
                                    if (storage.remove(ti.uri) == ResultCode.OK)
                                        rc = ResultCode.OK;
                                    else
                                        rc = ResultCode.Fail_Store;

                                    send(tid_response_reciever, rc, thisTid);

                                    return;
                                }
                                else if (ti.cmd == INDV_OP.PUT)
                                {
                                    string new_hash;
                                    //log.trace ("storage_manager:PUT %s", ti.uri);
                                    if (storage.store(ti.uri, ti.new_binobj, op_id) == ResultCode.OK)
                                    {
                                        rc = ResultCode.OK;
                                        op_id++;
                                        set_subject_manager_op_id(op_id);
                                    }
                                    else
                                    {
                                        rc = ResultCode.Fail_Store;
                                    }

                                    send(tid_response_reciever, rc, thisTid);

                                    if (rc == ResultCode.OK)
                                    {
                                        module_info.put_info(op_id, committed_op_id);

                                        ubyte[ 20 ] hash = ripemd160Of(ti.new_binobj);
                                        BigInt msg_hash = "0x" ~ toHexString(hash);
                                        new_hash = toHex(msg_hash);

                                        bin_log_name = write_in_binlog(ti.new_binobj, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_name);

                                        if (storage_id == P_MODULE.subject_manager)
                                        {
                                            Individual imm;
                                            imm.uri = text(op_id);
                                            imm.addResource("cmd", Resource(ti.cmd));

                                            imm.addResource("uri", Resource(DataType.Uri, ti.uri));

                                            if (ti.user_uri !is null && ti.user_uri.length > 0)
                                                imm.addResource("user_uri", Resource(DataType.Uri, ti.user_uri));

                                            imm.addResource("new_state", Resource(DataType.String, ti.new_binobj));

                                            if (ti.prev_binobj !is null && ti.prev_binobj.length > 0)
                                                imm.addResource("prev_state", Resource(DataType.String, ti.prev_binobj));
                                            else
                                                uris_queue.push(ti.uri);

                                            if (ti.event_id !is null && ti.event_id.length > 0)
                                                imm.addResource("event_id", Resource(DataType.String, ti.event_id));

                                            if (tnx_id <= 0)
                                                tnx_id = op_id;

                                            imm.addResource("tnx_id", Resource(tnx_id));

                                            imm.addResource("op_id", Resource(op_id));
                                            imm.addResource("u_count", Resource(ti.update_counter));
                                            imm.addResource("assigned_subsystems", Resource(ti.assigned_subsystems));

                                            //log.trace ("imm=[%s]", imm);

                                            string binobj = imm.serialize();
                                            //writeln("*binobj.length=", binobj.length);

                                            individual_queue.push(binobj);
//                                          string msg_to_modules = indv_uri ~ ";" ~ text(update_counter) ~ ";" ~ text (op_id) ~ "\0";
                                            string msg_to_modules = format("#%s;%d;%d", ti.uri, ti.update_counter, op_id);

                                            int bytes = nn_send(sock, cast(char *)msg_to_modules, msg_to_modules.length, 0);
//                                          log.trace("SEND %d bytes UPDATE SIGNAL TO %s", bytes, notify_channel_url);
                                        }
                                    }

                                    return;
                                }
                            }
                            catch (Exception ex)
                            {
                                send(tid_response_reciever, ResultCode.Fail_Commit, thisTid);
                                return;
                            }
                        },
                        (byte cmd, int arg, bool arg2)
                        {
                            if (cmd == CMD_SET_TRACE)
                                set_trace(arg, arg2);
                        },
                        (OwnerTerminated ot)
                        {
                            //log.trace("%s::storage_manager::OWNER TERMINATED", thread_name);
                            return;
                        },
                        (Variant v) { log.trace("%s::storage_manager::Received some other type. %s", thread_name, v); });
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
        if (uris_queue !is null)
        {
            uris_queue.close();
            uris_queue = null;
        }
    }
}
