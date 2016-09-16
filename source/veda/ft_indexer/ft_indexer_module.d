/**
 * search module
 */
module veda.ft_indexer.ft_indexer_module;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, core.sys.posix.signal, core.sys.posix.unistd;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.bind.xapian_d_header;
private import veda.core.common.context, veda.util.tools, veda.veda.ft_indexer.xapian_indexer;
private import veda.vmodule.vmodule;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-fulltext_indexer", "log", "FT-INDEXER");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

void main(char[][] args)
{
    core.thread.Thread.sleep(dur!("seconds")(1));
    process_name = "fulltext_indexer";

    auto p_module = new FTIndexerProcess(P_MODULE.fulltext_indexer, "127.0.0.1", 8091);

    p_module.run();
}

class FTIndexerProcess : VedaModule
{
    IndexerContext ictx = new IndexerContext;

    long           last_update_time = 0;

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        super(_module_name, _host, _port);
    }

    override Context create_context()
    {
        return null;
    }

    override bool prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id)
    {
        ictx.index_msg(new_indv, prev_indv, cmd, op_id, context);

        return true;
    }

    override void thread_id()
    {
        if (committed_op_id == op_id)
            return;

        long now = Clock.currTime().stdTime();

        if (now - last_update_time > 1_000_000)
        {
            ictx.commit_all_db();
            last_update_time = now;
            committed_op_id  = op_id;
            //writeln("@ commit, commited_op_id=", commited_op_id);
        }
    }

    override bool configure()
    {
        //writeln("@ configure B");
        ictx.thread_name = process_name;
        ictx.init();

        //writeln("@ configure E");
        return true;
    }
}
