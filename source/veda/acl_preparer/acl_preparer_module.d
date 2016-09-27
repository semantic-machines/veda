/**
 * search module
 */
module veda.acl_preparer.acl_preparer_module;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, core.sys.posix.signal, core.sys.posix.unistd;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue,
               veda.core.common.know_predicates;
private import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.bind.xapian_d_header;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule;
private import veda.core.az.acl, veda.core.az.right_set;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-acl-preparer", "log", "ACLP");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

void main(char[][] args)
{
    core.thread.Thread.sleep(dur!("seconds")(1));
    process_name = "acl_preparer";

    auto p_module = new ACLPreparerProcess(P_MODULE.acl_preparer, "127.0.0.1", 8091);

    p_module.run();
}

class ACLPreparerProcess : VedaModule
{
    Authorization storage;
    long          last_update_time = 0;

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        super(_module_name, _host, _port);
    }

    override Context create_context()
    {
        storage = new Authorization(acl_indexes_db_path, DBMode.RW, "acl_manager");
        Context _context = new PThreadContext("cfg:standart_node", process_name, module_name, parent_url, storage);
        return _context;
    }

    override ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id)
    {
        //writefln ("#1 prepare, op_id=%d", op_id);

        Resources rdfType = new_indv.resources[ rdf__type ];

        if (rdfType.anyExists(veda_schema__PermissionStatement) == true)
        {
            prepare_permission_statement(new_indv, op_id, storage);
        }
        else if (rdfType.anyExists(veda_schema__Membership) == true)
        {
            prepare_membership(new_indv, op_id, storage);
        }
        else if (rdfType.anyExists(veda_schema__PermissionFilter) == true)
        {
            prepare_permission_filter(new_indv, op_id, storage);
        }

        //writefln ("#e prepare, op_id=%d", op_id);
        //storage.flush(1);

        return ResultCode.OK;
    }

    override void thread_id()
    {
        //writefln ("#1 commited_op_id=%d, op_id=%d", commited_op_id, op_id);
        if (committed_op_id == op_id)
            return;

        long now = Clock.currTime().stdTime();

        if (now - last_update_time > 1_000_000)
        {
            storage.flush(1);
            last_update_time = now;
            committed_op_id  = op_id;
            //log.trace("commit, commited_op_id=%d", commited_op_id);
        }
    }

    override bool configure()
    {
        return true;
    }
}
