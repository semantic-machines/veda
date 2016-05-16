/**
   авторизация
 */

module veda.core.threads.acl_manager;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.type, veda.onto.individual, veda.onto.resource, veda.core.bind.lmdb_header, veda.core.common.context, veda.core.common.define,
           veda.core.common.know_predicates, veda.core.log_msg, veda.util.cbor8individual;
    import veda.core.util.utils, veda.util.cbor, util.logger;
    import veda.core.storage.lmdb_storage, veda.core.impl.thread_context, veda.core.az.acl, veda.core.az.right_set;
}

// ////////////// ACLManager
protected byte err;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "ACL-MANAGER");
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
    NOP       = 64
}

public string backup(string backup_id)
{
    string res;

    Tid    tid_acl_manager = getTid(P_MODULE.acl_manager);

    send(tid_acl_manager, CMD.BACKUP, backup_id, thisTid);
    receive((string _res) { res = _res; });

    return res;
}

public ResultCode flush(bool is_wait)
{
    ResultCode rc;
    Tid        tid = getTid(P_MODULE.acl_manager);

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

void acl_manager(string thread_name, string db_path)
{
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;

    core.thread.Thread.getThis().name         = thread_name;
    Authorization                storage      = new Authorization(acl_indexes_db_path, DBMode.RW, "acl_manager");
    string                       bin_log_name = get_new_binlog_name(db_path);

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });
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
                    },
                    (CMD cmd, EVENT type, string msg, long op_id)
                    {
                        if (cmd == CMD.PUT)
                        {
                            try
                            {
                                Individual ind;
                                if (cbor2individual(&ind, msg) < 0)
                                {
                                    log.trace("ERR! invalid individual: [%s] op_id=%d", msg, op_id);
                                    return;
                                }

                                Resources rdfType = ind.resources[ rdf__type ];

                                if (rdfType.anyExists(veda_schema__PermissionStatement) == true)
                                {
                                    prepare_permission_statement(ind, op_id, storage);
                                }
                                else if (rdfType.anyExists(veda_schema__Membership) == true)
                                {
                                    prepare_membership(ind, op_id, storage);
                                }
                                else if (rdfType.anyExists(veda_schema__PermissionFilter) == true)
                                {
                                    prepare_permission_filter(ind, op_id, storage);
                                }
                            }
                            finally
                            {
                                set_acl_manager_op_id(op_id);
                            }
                        }
                    },
                    (CMD cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.NOP)
                            send(tid_response_reciever, true);
                        else
                            send(tid_response_reciever, false);
                    },
                    (CMD cmd, string msg, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.BACKUP)
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
                    (CMD cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD.SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (Variant v) { writeln(thread_name, "::acl_manager::Received some other type: [", v, "]"); });
        }
        catch (Throwable ex)
        {
            log.trace("acl manager# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}
