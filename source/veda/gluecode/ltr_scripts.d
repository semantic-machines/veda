/**
 * ltr-scripts module
 */
module veda.gluecode.ltr_scripts;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.gluecode.v8d_header;
private import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.core.log_msg, veda.core.common.know_predicates, veda.onto.onto;
private import veda.process.child_process;
private import search.vel, search.vql, veda.gluecode.script;

void main(char[][] args)
{
    process_name = "ltr-scripts";

    core.thread.Thread.sleep(dur!("seconds")(1));

    ScriptProcess p_script = new ScriptProcess(P_MODULE.ltr_scripts, "127.0.0.1", 8091);

    p_script.run();
}

class ScriptProcess : ChildProcess
{
    private          ScriptInfo[ string ] codelet_scripts;

    private VQL      vql;
    private string   empty_uid;
    private string   vars_for_codelet_script;

    private ScriptVM script_vm;

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        super(_module_name, _host, _port);
    }

    long count_sckip = 0;

    override bool prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id)
    {
        if (script_vm is null)
            return false;

        return true;
    }

    override void configure()
    {
        vars_for_codelet_script =
            "var user_uri = get_env_str_var ('$user');"
            ~ "var execute_script = get_individual (ticket, '$execute_script');"
            ~ "var prev_state = get_individual (ticket, '$prev_state');"
            ~ "var super_classes = get_env_str_var ('$super_classes');"
            ~ "var _event_id = document['@'] + '+' + _script_id;";

        vql = new VQL(context);

        script_vm = get_ScriptVM(context);
    }

    public void load_event_scripts()
    {
        if (script_vm is null)
            return;
    }


    private void prepare_script(ref ScriptInfo[ string ] scripts, Individual ss, ScriptVM script_vm, string vars_env)
    {
    }
}


