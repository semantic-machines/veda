/**
 * процесс отвечающий за хранение
 */
module storage.storage_thread;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;

    import bind.lmdb_header;
    import type;
    import util.logger, util.utils, util.cbor, util.cbor8individual;
    import veda.core.context, veda.core.define, veda.core.log_msg;
    import onto.individual, onto.resource;
    import search.vel;
    import storage.lmdb_storage;
}

logger log;

static this()
{
    log = new logger("pacahon", "log", "server");
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
                        send(tid_response_reciever, EVENT.NOT_READY, null, thisTid);

                    try
                    {
                        if (cmd == CMD.PUT)
                        {
                            string new_hash;
                            EVENT ev = storage.update_or_create(msg, new_hash, EVENT.NONE);

                            send(tid_response_reciever, ev, msg, thisTid);

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
                                send(tid_response_reciever, EVENT.ERROR, msg, thisTid);
                                return;
                            }

                            Individual indv;
                            string ss_as_cbor = storage.find(arg.uri);
                            code = cbor2individual(&indv, ss_as_cbor);
                            if (code < 0)
                            {
                                log.trace("ERR:store_individual(ADD|SET|REMOVE):cbor2individual [%s]", ss_as_cbor);
                                send(tid_response_reciever, EVENT.ERROR, msg, thisTid);
                                return;
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
                                    // !!! not implemented
                                }
                            }

                            ss_as_cbor = individual2cbor(&indv);
                            string new_hash;
                            storage.update_or_create(ss_as_cbor, new_hash, EVENT.UPDATE);
                            send(tid_response_reciever, EVENT.UPDATE, ss_as_cbor, thisTid);

                            bin_log_name = write_in_binlog(ss_as_cbor, new_hash, bin_log_name, size_bin_log, max_size_bin_log, db_path);
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
