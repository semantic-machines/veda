/**
 * scripts module
 */
module veda.gluecode.scripts;

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
    process_name = "scripts";

    core.thread.Thread.sleep(dur!("seconds")(1));

    ScriptProcess p_script = new ScriptProcess(P_MODULE.scripts, "127.0.0.1", 8091);

    p_script.run();
}

class ScriptProcess : ChildProcess
{
    private          ScriptInfo[ string ] event_scripts;

    private VQL      vql;
    private string   empty_uid;
    private string   vars_for_event_script;
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

        //writeln ("#prev_indv=", prev_indv);
        //writeln ("#new_indv=", new_indv);

        string    individual_id = new_indv.uri;

        bool      prepare_if_is_script = false;

        Resources types      = new_indv.resources.get(rdf__type, Resources.init);
        string[]  indv_types = types.getAsArrayStrings();

        foreach (itype; indv_types)
        {
            if (itype == veda_schema__PermissionStatement || itype == veda_schema__Membership)
                return true;

            if (itype == veda_schema__Event)
                prepare_if_is_script = true;
        }

        if (prepare_if_is_script == false)
        {
            if (event_scripts.get(individual_id, ScriptInfo.init) !is ScriptInfo.init)
                prepare_if_is_script = true;
        }

        if (prepare_if_is_script)
        {
            prepare_script(event_scripts, new_indv, script_vm, vars_for_event_script);
        }

        set_g_parent_script_id_etc(event_id);
        set_g_prev_state(prev_bin);

        g_document.data   = cast(char *)new_bin;
        g_document.length = cast(int)new_bin.length;

        if (user_uri !is null)
        {
            g_user.data   = cast(char *)user_uri;
            g_user.length = cast(int)user_uri.length;
        }
        else
        {
            g_user.data   = cast(char *)"cfg:VedaSystem";
            g_user.length = "cfg:VedaSystem".length;
        }

        string sticket = context.sys_ticket().id;
        g_ticket.data   = cast(char *)sticket;
        g_ticket.length = cast(int)sticket.length;

        //writeln ("@S1 sticket=", sticket);

        set_g_super_classes(indv_types, context.get_onto());

        foreach (script_id, script; event_scripts)
        {
            if (script.compiled_script !is null)
            {
                if (script.filters.length > 0 && isFiltred(&script, indv_types, context.get_onto()) == false)
                    continue;

                //log.trace("execute script:%s", script_id);

                if (event_id !is null && event_id.length > 1 && event_id == (individual_id ~ '+' ~ script_id))
                {
                    //writeln("skip script [", script_id, "], type:", type, ", indiv.:[", individual_id, "]");
                    continue;
                }

                try
                {
/*
    if (count_sckip == 0)
    {
   long now_sckip;
    writefln("... start exec event script : %s %s %d %s", script_id, individual_id, op_id, event_id);
    readf(" %d", &now_sckip);
    count_sckip = now_sckip;
    }

    if (count_sckip > 0)
        count_sckip--;
 */
                    if (trace_msg[ 300 ] == 1)
                        log.trace("start exec event script : %s %s %d %s", script_id, individual_id, op_id, event_id);

                    //count++;
                    script.compiled_script.run();

                    bool res = commit();

                    if (res == false)
                    {
                        log.trace("fail exec event script : %s", script_id);
                        return false;
                    }

                    if (trace_msg[ 300 ] == 1)
                        log.trace("end exec event script : %s", script_id);


                    //*(cast(char*)script_vm) = 0;
                }
                catch (Exception ex)
                {
                    log.trace_log_and_console("WARN! fail execute event script : %s %s", script_id, ex.msg);
                }
            }
        }
//                                writeln("count:", count);

        //clear_script_data_cache ();
        // writeln("scripts B #e *", process_name);
        return true;
    }

    override void configure()
    {
        vars_for_event_script =
            "var user_uri = get_env_str_var ('$user');"
            ~ "var parent_script_id = get_env_str_var ('$parent_script_id');"
            ~ "var parent_document_id = get_env_str_var ('$parent_document_id');"
            ~ "var prev_state = get_individual (ticket, '$prev_state');"
            ~ "var super_classes = get_env_str_var ('$super_classes');"
            ~ "var _event_id = document['@'] + '+' + _script_id;";

        vql = new VQL(context);

        script_vm = get_ScriptVM(context);
        load_event_scripts();
    }

    public void load_event_scripts()
    {
        if (script_vm is null)
            return;

        if (trace_msg[ 301 ] == 1)
            log.trace("start load db scripts");

        Ticket       sticket = context.sys_ticket();
        Individual[] res;
        vql.get(&sticket,
                "return { 'v-s:script'} filter { 'rdf:type' === 'v-s:Event'}",
                res);

        int count = 0;

        foreach (ss; res)
        {
            prepare_script(event_scripts, ss, script_vm, vars_for_event_script);
        }

        if (trace_msg[ 300 ] == 1)
            log.trace("end load db scripts, count=%d ", res.length);
    }


    private void prepare_script(ref ScriptInfo[ string ] scripts, Individual ss, ScriptVM script_vm, string vars_env)
    {
        if (trace_msg[ 310 ] == 1)
            log.trace("prepare_script uri=%s", ss.uri);

        try
        {
            if (ss.isExists(veda_schema__deleted, true) || ss.isExists("v-s:disabled", true))
            {
                //writeln ("@S0 SCRIPT OFF, uri=", ss.uri);
                ScriptInfo script = scripts.get(ss.uri, ScriptInfo.init);

                if (script !is ScriptInfo.init)
                {
                    script.compiled_script = null;
                    scripts[ ss.uri ]      = script;
                }
                return;
            }

            string scripts_text = ss.getFirstResource(veda_schema__script).literal;
            if (scripts_text.length <= 0)
                return;

            string str_script =
                "var ticket = get_env_str_var ('$ticket');"
                ~ "var document = get_individual (ticket, '$document');"
                ~ "if (document) {"
                ~ "var _script_id = '" ~ ss.uri ~ "';"
                ~ vars_env
                ~ "script();"
                ~ "};"
                ~ "function script() {" ~ scripts_text ~ "};"
            ;
            try
            {
                ScriptInfo script = ScriptInfo.init;
                script.id         = ss.uri;
                script.str_script = str_script;

                script.compiled_script = script_vm.compile(script.str_script);
                if (trace_msg[ 310 ] == 1)
                    log.trace("#compile event script.id=%s, text=%s", script.id, script.str_script);

                //writeln("scripts_text:", scripts_text);

                Resources filters = ss.getResources(veda_schema__filter);

                foreach (filter; filters)
                {
                    script.filters[ filter.uri ] = true;
                }

                scripts[ ss.uri ] = script;
            }
            catch (Exception ex)
            {
                log.trace_log_and_console("error:compile event script :%s", ex.msg);
            }
        }
        catch (Exception ex)
        {
            log.trace_log_and_console("error:load event script :%s", ex.msg);
        }
        finally
        {
        }
    }

    private void set_g_parent_script_id_etc(string event_id)
    {
        //writeln ("@d event_id=", event_id);
        string[] aa;

        if (event_id !is null)
        {
            aa = event_id.split("+");

            if (aa.length == 2)
            {
                g_parent_script_id.data     = cast(char *)aa[ 1 ];
                g_parent_script_id.length   = cast(int)aa[ 1 ].length;
                g_parent_document_id.data   = cast(char *)aa[ 0 ];
                g_parent_document_id.length = cast(int)aa[ 0 ].length;
            }
            else
            {
                g_parent_script_id.data     = cast(char *)empty_uid;
                g_parent_script_id.length   = cast(int)empty_uid.length;
                g_parent_document_id.data   = cast(char *)empty_uid;
                g_parent_document_id.length = cast(int)empty_uid.length;
            }
        }
        else
        {
            g_parent_script_id.data     = cast(char *)empty_uid;
            g_parent_script_id.length   = cast(int)empty_uid.length;
            g_parent_document_id.data   = cast(char *)empty_uid;
            g_parent_document_id.length = cast(int)empty_uid.length;
        }
    }

    private bool isFiltred(ScriptInfo *script, ref string[] indv_types, Onto onto)
    {
        bool any_exist = false;

        foreach (indv_type; indv_types)
        {
            if ((indv_type in script.filters) !is null)
            {
                any_exist = true;
                break;
            }

            if (onto.isSubClasses(cast(string)indv_type, script.filters.keys) == true)
            {
                any_exist = true;
                break;
            }
        }
        return any_exist;
    }
}


