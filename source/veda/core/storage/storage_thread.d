/**
 * процесс отвечающий за хранение
 */
module veda.core.storage.storage_thread;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
    import util.logger, util.utils, veda.util.cbor, veda.core.util.cbor8individual;
    import veda.type, veda.core.bind.lmdb_header, veda.core.context, veda.core.define, veda.core.log_msg, veda.onto.individual, veda.onto.resource;
    import veda.core.storage.lmdb_storage, veda.core.storage.binlog_tools;
    import search.vel;
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
// ////// ////// ///////////////////////////////////////////
public string backup(Context ctx)
{
    string backup_id;

    Tid    tid_subject_manager = ctx.getTid(P_MODULE.subject_manager);

    send(tid_subject_manager, CMD.BACKUP, "", thisTid);
    receive((string res) { backup_id = res; });

    return backup_id;
}


public ResultCode send_put(P_MODULE storage_id, Context ctx, string uri, string cur_state, bool ignore_freeze, out long op_id)
{
    ResultCode rc;
    Tid        tid = ctx.getTid(storage_id);

    if (tid != Tid.init)
    {
        send(tid, CMD.PUT, uri, cur_state, ignore_freeze, thisTid);

        receive((ResultCode _rc, Tid from)
                {
                    if (from == ctx.getTid(storage_id))
                        rc = _rc;
                    op_id = get_count_put();
                    return true;
                });
    }
    return rc;
}

public ResultCode send_remove(P_MODULE storage_id, Context ctx, string uri, bool ignore_freeze, out long op_id)
{
    ResultCode rc;
    Tid        tid = ctx.getTid(storage_id);

    if (tid != Tid.init)
    {
        send(tid, CMD.REMOVE, uri, ignore_freeze, thisTid);

        receive((ResultCode _rc, Tid from)
                {
                    if (from == ctx.getTid(storage_id))
                        rc = _rc;
                    op_id = get_count_put();
                    return true;
                });
    }
    return rc;
}

public void individuals_manager(string thread_name, string db_path, string node_id)
{
    core.thread.Thread.getThis().name             = thread_name;
    LmdbStorage                  storage          = new LmdbStorage(db_path, DBMode.RW, "individuals_manager");
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;
    string                       bin_log_name     = get_new_binlog_name(db_path);

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
                        if (cmd == CMD.FREEZE)
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
                            storage.put(key, msg);
                        }
                    },
                    (CMD cmd, string msg, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.FIND)
                        {
                            string res = storage.find(msg);
                            //writeln("@FIND msg=", msg, ", $res = ", res);
                            send(tid_response_reciever, msg, res, thisTid);
                            return;
                        }
                    },
                    (CMD cmd, string uri, bool ignore_freeze, Tid tid_response_reciever)
                    {
                        ResultCode rc = ResultCode.Not_Ready;

                        if (!ignore_freeze && is_freeze && cmd == CMD.REMOVE)
                            send(tid_response_reciever, rc, thisTid);
                    	
                        try
                        {
                            if (cmd == CMD.REMOVE)
                            {
                                if (storage.remove(uri) == 0)
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
                    (CMD cmd, string uri, string msg, bool ignore_freeze, Tid tid_response_reciever)
                    {
                        ResultCode rc = ResultCode.Not_Ready;

                        if (!ignore_freeze && is_freeze && cmd == CMD.PUT)
                            send(tid_response_reciever, rc, thisTid);

                        try
                        {
                            if (cmd == CMD.PUT)
                            {
                                string new_hash;

                                if (storage.update_or_create(uri, msg, new_hash) == 0)
                                    rc = ResultCode.OK;
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
                    },
                    (CMD cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD.SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (Variant v) { writeln(thread_name, "::individuals_manager::Received some other type.", v); });
        }
        catch (Throwable ex)
        {
            log.trace("individuals_manager# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}

/*
   public string transform_and_execute_vql_to_lmdb(TTA tta, string p_op, out string l_token, out string op, out double _rd, int level,
                                                ref Subjects res, Context context)
   {
    string dummy;
    double rd, ld;

    if (tta.op == "==")
    {
        string ls = transform_and_execute_vql_to_lmdb(tta.L, tta.op, dummy, dummy, ld, level + 1, res, context);
        string rs = transform_and_execute_vql_to_lmdb(tta.R, tta.op, dummy, dummy, rd, level + 1, res, context);
   //          writeln ("ls=", ls);
   //          writeln ("rs=", rs);
        if (ls == "@")
        {
            string  rr = context.get_subject_as_cbor(rs);
            Subject ss = cbor2subject(rr);
            res.addSubject(ss);
        }
    }
    else if (tta.op == "||")
    {
        if (tta.R !is null)
            transform_and_execute_vql_to_lmdb(tta.R, tta.op, dummy, dummy, rd, level + 1, res, context);

        if (tta.L !is null)
            transform_and_execute_vql_to_lmdb(tta.L, tta.op, dummy, dummy, ld, level + 1, res, context);
    }
    else
    {
   //		writeln ("#5 tta.op=", tta.op);
        return tta.op;
    }
    return null;
   }
 */
