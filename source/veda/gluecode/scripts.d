/**
 * scripts module
 */
module veda.gluecode.scripts;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.container.array, std.algorithm, std.range, core.thread;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.core.common.log_msg, veda.core.common.know_predicates, veda.onto.onto;
private import veda.vmodule.vmodule, veda.core.search.vel, veda.core.search.vql, veda.gluecode.script, veda.gluecode.v8d_header;

class ScriptProcess : VedaModule
{
    private          ScriptInfo[ string ] event_scripts;
    private          Array!string event_scripts_order;

    private VQL      vql;
    private string   empty_uid;
    private string   vars_for_event_script;
    private string   vars_for_codelet_script;

    private ScriptVM script_vm;
    private string   vm_id;

    this(string _vm_id, string _module_name, Logger log)
    {
        super(_module_name, log);

        vm_id           = _vm_id;
        g_vm_id         = vm_id;
        g_cache_of_indv = cache_of_indv;
    }

    long count_sckip = 0;

    override void thread_id()
    {
    }

    override void receive_msg(string msg)
    {
        log.trace("receive msg=%s", msg);
    }

    override Context create_context()
    {
        return null;
    }


    override ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id,
                                long op_id)
    {
        if (script_vm is null)
            return ResultCode.Not_Ready;

        //writeln ("#prev_indv=", prev_indv);
        //writeln ("#new_indv=", new_indv);

        string    individual_id = new_indv.uri;

        bool      prepare_if_is_script = false;

        Resources types      = new_indv.resources.get(rdf__type, Resources.init);
        string[]  indv_types = types.getAsArrayStrings();
        foreach (itype; indv_types)
        {
            if (itype == veda_schema__PermissionStatement || itype == veda_schema__Membership)
            {
                committed_op_id = op_id;
                return ResultCode.OK;
            }

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
            prepare_script(event_scripts, event_scripts_order, new_indv, script_vm, vars_for_event_script);
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

        //log.trace("-------------------");
        //log.trace ("indv=%s, indv_types=%s", individual_id, indv_types);
        //log.trace ("queue of scripts:%s", event_scripts_order.array());


        foreach (_script_id; event_scripts_order)
        {
            script_id = _script_id;

            ScriptInfo script = event_scripts[ script_id ];

            if (script.compiled_script !is null)
            {
                //log.trace("look script:%s", script_id);
                if (event_id !is null && event_id.length > 1 && (event_id == (individual_id ~ '+' ~ script_id) || event_id == "IGNORE"))
                {
                    //writeln("skip script [", script_id, "], type:", type, ", indiv.:[", individual_id, "]");
                    continue;
                }

                if (script.run_at != vm_id)
                    continue;

                //log.trace("first check pass script:%s, filters=%s", script_id, script.filters);

                if (is_filter_pass(&script, individual_id, indv_types, context.get_onto()) == false)
                {
                    //log.trace("skip (filter) script:%s", script_id);
                    continue;
                }


                //log.trace("filter pass script:%s", script_id);

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
                    //if (trace_msg[ 300 ] == 1)
                    log.trace("start exec event script : %s %s %d %s", script_id, individual_id, op_id, event_id);

                    //count++;
                    script.compiled_script.run();

                    ResultCode res = commit();
                    if (res != ResultCode.OK)
                    {
                        log.trace("fail exec event script : %s", script_id);
                        return res;
                    }

                    //if (trace_msg[ 300 ] == 1)
                    log.trace("end exec event script : %s", script_id);


                    //*(cast(char*)script_vm) = 0;
                }
                catch (Exception ex)
                {
                    log.trace("WARN! fail execute event script : %s %s", script_id, ex.msg);
                }
            }
        }

        //log.trace("count:", count);

        // clear_script_data_cache ();
        // log.trace("scripts B #e *", process_name);
        committed_op_id = op_id;

        return ResultCode.OK;
    }

    override bool open()
    {
        vql       = new VQL(context);
        script_vm = get_ScriptVM(context);

        if (script_vm !is null)
            return true;

        return false;
    }

    override bool configure()
    {
        log.trace("configure scripts");

        vars_for_event_script =
            "var user_uri = get_env_str_var ('$user');"
            ~ "var parent_script_id = get_env_str_var ('$parent_script_id');"
            ~ "var parent_document_id = get_env_str_var ('$parent_document_id');"
            ~ "var prev_state = get_individual (ticket, '$prev_state');"
            ~ "var super_classes = get_env_str_var ('$super_classes');"
            ~ "var _event_id = document['@'] + '+' + _script_id;";

        load_event_scripts();

        return true;
    }

    override bool close()
    {
        return vql.close_db();
    }

    override void event_of_change(string uri)
    {
        configure();
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

        foreach (ss; res)
            prepare_script(event_scripts, event_scripts_order, ss, script_vm, vars_for_event_script);

        string scripts_ordered_list;
        foreach (_script_id; event_scripts_order)
            scripts_ordered_list ~= "," ~ _script_id;

//        if (trace_msg[ 300 ] == 1)
        log.trace("load db scripts, count=%d, scripts_uris=[%s] ", event_scripts_order.length, scripts_ordered_list);
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
}


