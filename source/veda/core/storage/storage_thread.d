/**
 * процесс отвечающий за хранение
 */
module veda.core.storage.storage_thread;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
    import util.logger, util.utils, util.cbor, veda.core.util.cbor8individual;
    import veda.type, veda.core.bind.lmdb_header, veda.core.context, veda.core.define, veda.core.log_msg, veda.onto.individual, veda.onto.resource;
    import veda.core.storage.lmdb_storage;
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


public bool send_put(Context ctx, CMD cmd, string cur_state, out string new_state, out string prev_state, out long op_id, out EVENT ev)
{
    Tid tid_subject_manager = ctx.getTid(P_MODULE.subject_manager);

    if (tid_subject_manager != Tid.init)
    {
        send(tid_subject_manager, cmd, cur_state, thisTid);
        receive((EVENT _ev, string _prev_state, string _new_state, Tid from)
                {
                    if (from == ctx.getTid(P_MODULE.subject_manager))
                        ev = _ev;
                    prev_state = _prev_state;
                    new_state = _new_state;
                    op_id = get_count_put();
                    return true;
                });
    }
    return false;
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

                        if (is_freeze == true && (cmd == CMD.PUT || cmd == CMD.ADD || cmd == CMD.SET || cmd == CMD.REMOVE))
                            send(tid_response_reciever, EVENT.NOT_READY, null, null, thisTid);

                        try
                        {
                            string prev_state;

                            if (cmd == CMD.PUT)
                            {
                                Individual arg;
                                int code = cbor2individual(&arg, msg);
                                if (code < 0)
                                {
                                    log.trace("ERR:store_individual(PUT):cbor2individual [%s]", msg);
                                    send(tid_response_reciever, EVENT.ERROR, prev_state, msg, thisTid);
                                    return;
                                }


                                string new_hash;
                                EVENT ev;
                                prev_state = storage.find(arg.uri);
                                if (prev_state is null)
                                    ev = EVENT.CREATE;
                                else
                                {
                                    ev = EVENT.UPDATE;
                                }
                                storage.update_or_create(arg.uri, msg, new_hash);
                                send(tid_response_reciever, ev, prev_state, msg, thisTid);

                                bin_log_name = write_in_binlog(msg, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_path);

                                return;
                            }
                            else if (cmd == CMD.ADD || cmd == CMD.SET || cmd == CMD.REMOVE)
                            {
                                Individual arg;
                                int code = cbor2individual(&arg, msg);
                                if (code < 0)
                                {
                                    log.trace("ERR:store_individual(ADD|SET|REMOVE):cbor2individual [%s]", msg);
                                    send(tid_response_reciever, EVENT.ERROR, null, msg, thisTid);
                                    return;
                                }

                                Individual indv;
                                prev_state = storage.find(arg.uri);
                                if (prev_state is null && (cmd == CMD.ADD || cmd == CMD.SET))
                                {
                                    string new_hash;
                                    storage.update_or_create(arg.uri, msg, new_hash);
                                    send(tid_response_reciever, EVENT.CREATE, prev_state, msg, thisTid);

                                    bin_log_name = write_in_binlog(msg, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_path);

                                    return;
                                }
                                else
                                {
                                    code = cbor2individual(&indv, prev_state);
                                    if (code < 0)
                                    {
                                        log.trace("ERR:store_individual(ADD|SET|REMOVE):cbor2individual [%s]", prev_state);
                                        send(tid_response_reciever, EVENT.ERROR, null, null, thisTid);
                                        return;
                                    }
                                }

                                foreach (predicate; arg.resources.keys)
                                {
                                    if (cmd == CMD.ADD)
                                    {
                                        // add value to set or ignore if exists
                                        indv.add_unique_Resources(predicate, arg.getResources(predicate));
                                    }
                                    else if (cmd == CMD.SET)
                                    {
                                        // set value to predicate
                                        indv.set_Resources(predicate, arg.getResources(predicate));
                                    }
                                    else if (cmd == CMD.REMOVE)
                                    {
                                        // remove predicate or value in set
                                        indv.remove_Resources(predicate, arg.getResources(predicate));
                                    }
                                }

                                string new_state = individual2cbor(&indv);
                                string new_hash;
                                storage.update_or_create(arg.uri, new_state, new_hash);
                                send(tid_response_reciever, EVENT.UPDATE, prev_state, new_state, thisTid);

                                bin_log_name = write_in_binlog(new_state, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_path);
                                return;
                            }
                        }
                        catch (Exception ex)
                        {
                            send(tid_response_reciever, EVENT.ERROR, null, thisTid);
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

private string write_in_binlog(string msg, string new_hash, string bin_log_name, out int size_bin_log, int max_size_bin_log, string db_path)
{
    inc_count_put();

    long      now = Clock.currTime().stdTime();
    OutBuffer oub = new OutBuffer();
    oub.write('\n');
    oub.write(cast(ubyte)0xff);
    oub.write(cast(ubyte)0x12);
    oub.write(cast(ubyte)0xff);
    oub.write(cast(ubyte)0x21);
    oub.write(cast(ubyte)0);

    oub.write(now);
    oub.write(cast(int)new_hash.length);
    oub.write(cast(int)msg.length);
    oub.write(new_hash);
    oub.write(msg);
    append(bin_log_name, oub.toString);
    size_bin_log += msg.length + 30;

    if (size_bin_log > max_size_bin_log)
    {
        size_bin_log = 0;
        bin_log_name = get_new_binlog_name(db_path);
    }
    return bin_log_name;
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
