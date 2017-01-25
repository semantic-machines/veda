/**
   авторизация
 */

module veda.server.acl_manager;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.common.type, veda.onto.individual, veda.onto.resource, veda.bind.lmdb_header, veda.core.common.context, veda.core.common.define,
           veda.core.common.know_predicates, veda.core.common.log_msg;
    import veda.core.util.utils, veda.common.logger, veda.util.module_info;
    import veda.core.storage.lmdb_storage, veda.core.impl.thread_context, veda.core.az.acl, veda.core.az.right_set;
}

// ////////////// ACLManager
protected byte err;

// ////// Logger ///////////////////////////////////////////
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-server", "log", "ACL-MANAGER");
    return _log;
}
// ////// ////// ///////////////////////////////////////////
enum CMD : byte
{
    /// Сохранить
    PUT       = 1,

    /// Коммит
    COMMIT    = 16,

    /// Включить/выключить отладочные сообщения
    SET_TRACE = 33,

    /// Backup
    BACKUP    = 41,

    /// Пустая комманда
    NOP       = 64,

    EXIT      = 49
}

public string backup(string backup_id)
{
    string res;

    Tid    tid_acl_manager = getTid(P_MODULE.acl_preparer);

    send(tid_acl_manager, CMD_BACKUP, backup_id, thisTid);
    receive((string _res) { res = _res; });

    return res;
}

public ResultCode flush(bool is_wait)
{
    ResultCode rc;
    Tid        tid = getTid(P_MODULE.acl_preparer);

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


void acl_manager(string thread_name, string db_path)
{
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;

    core.thread.Thread.getThis().name         = thread_name;
    Authorization                storage      = new Authorization(acl_indexes_db_path, DBMode.RW, "acl_manager", log);
    string                       bin_log_name = get_new_binlog_name(db_path);

    long                         l_op_id;
    long                         committed_op_id;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    ModuleInfoFile module_info = new ModuleInfoFile(thread_name, _log, OPEN_MODE.WRITER);
    if (!module_info.is_ready)
    {
        log.trace("thread [%s] terminated", process_name);
        return;
    }

    bool is_exit = false;

    while (is_exit == false)
    {
        try
        {
            receive(
                    (byte cmd)
                    {
                        if (cmd == CMD_COMMIT)
                        {
                            if (committed_op_id != l_op_id)
                            {
                                storage.flush(1);
                                //log.trace("acl commit op_id=%d", l_op_id);
                                committed_op_id = l_op_id;
                                module_info.put_info(l_op_id, committed_op_id);
                            }
                        }
                    },
                    (byte cmd, EVENT type, string prev_state, string new_state, long op_id)
                    {
                        if (cmd == CMD_PUT)
                        {
                            try
                            {
                                Individual new_ind;
                                if (new_ind.deserialize(new_state) < 0)
                                {
                                    log.trace("ERR! invalid individual: [%s] op_id=%d", new_state, op_id);
                                    return;
                                }

                                Individual prev_ind;
                                if (prev_state !is null && prev_ind.deserialize(prev_state) < 0)
                                {
                                    log.trace("ERR! invalid individual: [%s] op_id=%d", prev_state, op_id);
                                    return;
                                }

                                Resources rdfType = new_ind.resources[ rdf__type ];

                                if (rdfType.anyExists(veda_schema__PermissionStatement) == true)
                                {
                                    prepare_permission_statement(prev_ind, new_ind, op_id, storage);
                                }
                                else if (rdfType.anyExists(veda_schema__Membership) == true)
                                {
                                    prepare_membership(prev_ind, new_ind, op_id, storage);
                                }
                                else if (rdfType.anyExists(veda_schema__PermissionFilter) == true)
                                {
                                    prepare_permission_filter(prev_ind, new_ind, op_id, storage);
                                }
                            }
                            finally
                            {
                                set_acl_manager_op_id(op_id);
                                l_op_id = op_id;
                                module_info.put_info(l_op_id, committed_op_id);
                            }
                        }
                    },
                    (byte cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD_EXIT)
                        {
                            is_exit = true;
                            writefln("[%s] recieve signal EXIT", "acl_manager");
                            send(tid_response_reciever, true);
                        }
                        else if (cmd == CMD_NOP)
                            send(tid_response_reciever, true);
                        else
                            send(tid_response_reciever, false);
                    },
                    (byte cmd, string msg, Tid tid_response_reciever)
                    {
                        if (cmd == CMD_BACKUP)
                        {
                            try
                            {
                                string backup_id;
                                if (msg.length > 0)
                                    backup_id = msg;

                                if (backup_id is null)
                                    backup_id = "0";

                                Result res = storage.backup(backup_id);
                                if (res == Result.Ok)
                                {
                                    size_bin_log = 0;
                                    bin_log_name = get_new_binlog_name(db_path);
                                }
                                else if (res == Result.Err)
                                {
                                    backup_id = "";
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
                            send(tid_response_reciever, "?");
                        }
                    },
                    (byte cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD_SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (OwnerTerminated ot)
                    {
                        //log.trace("%s::acl_manager::OWNER TERMINATED", thread_name);
                        return;
                    },
                    (Variant v) { writeln(thread_name, "::acl_manager::Received some other type: [", v, "]"); });
        }
        catch (Throwable ex)
        {
            log.trace("acl manager# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }

    module_info.close();
}
