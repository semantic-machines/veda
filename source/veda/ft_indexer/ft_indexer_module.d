/**
 * search module
 */
module veda.ft_indexer.ft_indexer_module;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, core.sys.posix.signal, core.sys.posix.unistd, core.thread;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.bind.xapian_d_header;
private import veda.core.common.context, veda.util.tools, veda.ft_indexer.xapian_indexer;
private import veda.vmodule.vmodule;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-fulltext_indexer", "log", "FT-INDEXER");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

void main(char[][] args)
{
    Thread.sleep(dur!("seconds")(1));
    process_name = "fulltext_indexer";

    auto p_module = new FTIndexerProcess(text(P_MODULE.fulltext_indexer), new Logger("veda-core-fulltext_indexer", "log", ""));

    p_module.run();
}

class FTIndexerProcess : VedaModule
{
    IndexerContext ictx = new IndexerContext;

    long           last_update_time = 0;

    this(string _module_name, Logger log)
    {
        super(_module_name, log);
    }

    override Context create_context()
    {
        return null;
    }

    override ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id,
                                long op_id)
    {
        ictx.index_msg(new_indv, prev_indv, cmd, op_id, context);

        return ResultCode.OK;
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
            //log.trace("commit, op_id=%d", committed_op_id);
        }
    }

    override void receive_msg(string msg)
    {
        //log.trace("@1 msg=[%s]", msg);
        if (msg == "COMMIT")
        {
            //log.trace("@2");
            long now = Clock.currTime().stdTime();
            ictx.commit_all_db();
            last_update_time = now;
            committed_op_id  = op_id;
            //log.trace("commit, op_id=%d", committed_op_id);
        }
        if (msg == "reindex_all")
        {
            prepare_all();
        }
    }

    override bool open()
    {
        ictx.thread_name = process_name;
        ictx.init(&sticket, context);
        return true;
    }

    override bool configure()
    {
        return true;
    }

    override bool close()
    {
        if (ictx !is null)
            ictx.close();
        return true;
    }

    override void event_of_change(string uri)
    {
        configure();
    }
}
