module veda.gluecode.scripts_lp;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.container.array, std.algorithm, std.range, core.thread;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.core.common.log_msg, veda.core.common.know_predicates, veda.onto.onto;
private import veda.vmodule.vmodule, veda.search.vel, veda.search.vql, veda.gluecode.script, veda.gluecode.v8d_header;
private import veda.gluecode.scripts;

class ScriptProcessLowPriority : ScriptProcess
{
    Consumer main_cs_r;
    string   main_queue_cs = "scripts_main0";   // TODO переделать

    this(string _vm_id, SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        super(_vm_id, _subsystem_id, _module_id, log);
    }

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin,
                                ref Individual new_indv,
                                string event_id, long transaction_id,
                                long op_id, long count_pushed,
                                long count_popped)
    {
        if (main_cs_r is null)
        {
            log.trace("INFO: open %s, %s, %s", main_queue, my_consumer_path, main_queue_cs);
            main_cs_r = new Consumer(main_queue, my_consumer_path, main_queue_cs, Mode.R, log);
            while (main_cs_r.open(true, Mode.R) == false)
            {
                log.trace("WARN: main queue consumer not open, sleep and repeate");
                core.thread.Thread.sleep(dur!("seconds")(1));
            }
        }

        main_cs_r.reopen();
        while (main_cs_r.count_popped < count_popped)
        {
            log.tracec("INFO: sleep, scripts_main=%d, my=%d", main_cs_r.count_popped, count_popped);
            core.thread.Thread.sleep(dur!("seconds")(1));
            main_cs_r.reopen();
        }

        return super.prepare(queue_name, src, cmd, user_uri, prev_bin, prev_indv, new_bin, new_indv, event_id, transaction_id, op_id, count_pushed, count_popped);
    }
}

int main(string[] args)
{
    string vm_id = "lp";

    process_name = "scripts-" ~ vm_id;
    Logger log = new Logger("veda-core-" ~ process_name, "log", "");
    log.tracec("use VM id=%s", vm_id);

    Thread.sleep(dur!("seconds")(1));

    Thread.getThis().priority(Thread.PRIORITY_MIN);

    ScriptProcess p_script = new ScriptProcessLowPriority("V8.LowPriority", SUBSYSTEM.SCRIPTS, MODULE.scripts_lp, log);
    p_script.run();

    return 0;
}
