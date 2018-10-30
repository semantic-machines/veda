/**
 * search module
 */
module veda.ft_indexer.ft_indexer_module;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, core.sys.posix.signal, core.sys.posix.unistd, core.thread;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context, veda.search.xapian.xapian_search;
private import veda.bind.xapian_d_header;
private import veda.core.common.context, veda.ft_indexer.xapian_indexer;
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
    process_name = ft_indexer_queue_name;

    auto p_module = new FTIndexerProcess(SUBSYSTEM.FULL_TEXT_INDEXER, MODULE.fulltext_indexer, new Logger("veda-core-fulltext_indexer", "log", ""));

    p_module.run();
}

class FTIndexerProcess : VedaModule
{
    IndexerContext ictx = new IndexerContext;

    long           last_update_time  = 0;
    string         low_priority_user = "";

    int indexer_priority(string user_uri)
    {
        if (user_uri == low_priority_user)
            return 1;

        return 0;
    }

    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        super(_subsystem_id, _module_id, log);

        priority       = &indexer_priority;
        main_cs.length = 2;
    }



    /+  override int priority(string user_uri)
       {
          // if (user_uri == low_priority_user)
              // return 1;

          stderr.writefln("special user uri %s", user_uri);
          return 0;
       }+/

    override Context create_context()
    {
        return null;
    }

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id, long transaction_id, long op_id, long count_pushed, long count_popped)
    {
        ictx.index_msg(new_indv, prev_indv, cmd, op_id, context);

        return ResultCode.Ok;
    }

    override void thread_id()
    {
        if (committed_op_id == op_id)
            return;

        long now = Clock.currTime().stdTime();

        if (now - last_update_time > 600_000)
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
        else if (msg == "reindex_batch")
        {
            prepare_batch();
        }
    }

    override bool open()
    {
        context.set_vql (new XapianSearch(context));
        //context.set_vql(new FTQueryClient(context));

        ictx.thread_name = process_name;
        ictx.init(&sticket, context);

        low_priority_user = node.getFirstLiteral("cfg:low_priority_user");
        return true;
    }

    override bool configure()
    {
        log.trace("use configuration: %s", node);

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
