module veda.gluecode.scripts_main;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.container.array, std.algorithm, std.range, core.thread;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.core.common.log_msg, veda.core.common.know_predicates, veda.onto.onto;
private import veda.vmodule.vmodule, veda.core.search.vel, veda.core.search.vql, veda.gluecode.script, veda.gluecode.v8d_header;
private import veda.gluecode.scripts;

int main(string[] args)
{
    string vm_id = "main";

    process_name = "scripts-" ~ vm_id;
    Logger log = new Logger("veda-core-" ~ process_name, "log", "");
    log.tracec("use VM id=%s", vm_id);

    Thread.sleep(dur!("seconds")(1));

    ScriptProcess p_script = new ScriptProcess(vm_id, process_name, log);
    p_script.run();

    return 0;
}
