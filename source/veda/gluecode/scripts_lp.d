module veda.gluecode.scripts_lp;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.container.array, std.algorithm, std.range, core.thread;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.core.common.log_msg, veda.core.common.know_predicates, veda.onto.onto;
private import veda.vmodule.vmodule, veda.core.search.vel, veda.core.search.vql, veda.gluecode.script, veda.gluecode.v8d_header;
private import veda.gluecode.scripts;

int main(string[] args)
{
    string vm_id = "lp";

    process_name = "scripts-" ~ vm_id;
    Logger log = new Logger("veda-core-" ~ process_name, "log", "");
    log.tracec("use VM id=%s", vm_id);

    Thread.sleep(dur!("seconds")(1));

    Thread.getThis().priority(Thread.PRIORITY_MIN);

    ScriptProcess p_script = new ScriptProcess("V8.LowPriority", process_name, log);
    p_script.run();

    return 0;
}
