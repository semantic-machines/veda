/**
 * выполнение JS скриптов
 */
module veda.core.glue_code.scripts;

private
{
    import std.json, std.stdio, std.string, std.array, std.datetime, std.concurrency, std.conv, std.file;
    import core.thread;
    import veda.util.container, util.utils, util.logger, veda.util.cbor, veda.core.util.cbor8individual;
    import veda.type, veda.onto.individual, veda.onto.resource, veda.onto.onto;
    import veda.core.know_predicates, veda.core.context, veda.core.define, veda.core.thread_context, veda.core.log_msg;
    import search.vel, search.vql;
    import bind.v8d_header;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "SCRIPTS");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

private struct ScriptInfo
{
    string id;
    string str_script;
    bool[ string ] filters;
    Script compiled_script;
}

private int     count;
private Context context;
private         ScriptInfo[ string ] scripts;
private VQL     vql;

public Tid start_module(string node_id)
{
    Tid tid = spawn(&scripts_thread, text(P_MODULE.scripts), node_id);

    if (wait_starting_module(P_MODULE.scripts, tid) == false)
        return Tid.init;

    register(text(P_MODULE.scripts), tid);
    return locate(text(P_MODULE.scripts));
}

public bool stop_module(Context ctx)
{
    Tid tid_scripts = ctx.getTid(P_MODULE.scripts);

    if (tid_scripts != Tid.init)
    {
        send(tid_scripts, CMD.EXIT);
    }

    return 0;
}

public void send_put(Context ctx, Ticket *ticket, EVENT ev_type, string new_state, string prev_state, ref MapResource rdfType,
                     Individual *individual,
                     string event_id,
                     long op_id)
{
    Tid tid_scripts = ctx.getTid(P_MODULE.scripts);

    if (tid_scripts != Tid.init)
    {
        try
        {
            immutable(string)[] types;

            foreach (key; rdfType.keys)
                types ~= key;

            string user_uri;

            if (ticket !is null)
                user_uri = ticket.user_uri;

            send(tid_scripts, user_uri, ev_type, new_state, prev_state, types, individual.uri, event_id, op_id);
        }
        catch (Exception ex)
        {
            writeln("EX!bus_event:", ex.msg);
        }
    }

    veda.core.glue_code.scripts.inc_count_recv_put();
}

string empty_uid = "";

public void scripts_thread(string thread_name, string node_id)
{
    core.thread.Thread.getThis().name = thread_name;

    context   = new PThreadContext(node_id, thread_name, P_MODULE.scripts);
    g_context = context;

    vql = new VQL(context);
    load_event_scripts();

    try
    {
        // SEND ready
        receive((Tid tid_response_receiver)
                {
                    send(tid_response_receiver, true);
                });

        Onto onto;

        while (true)
        {
            try
            {
                ScriptVM script_vm = context.get_ScriptVM();

                receive(
                        (CMD cmd)
                        {
                            if (cmd == CMD.EXIT)
                            {
                                thread_term();
                            }
                        },
                        (CMD cmd, string arg, Tid to)
                        {
                            if (cmd == CMD.RELOAD)
                            {
                                Individual ss;
                                if (cbor2individual(&ss, arg) > 0)
                                {
                                    prepare_event_scripts(ss, script_vm);
                                    send(to, true);
                                }
                            }
                            else
                            {
                                send(to, false);
                            }
                        },
                        (CMD cmd, Tid to)
                        {
                            if (cmd == CMD.RELOAD)
                            {
                                if (onto is null)
                                    onto = context.get_onto();
                                onto.load();
                                log.trace("onto reloaded");
                                send(to, true);
                            }
                            else if (cmd == CMD.NOP)
                                send(to, true);
                            else
                                send(to, false);
                        },
                        (string user_uri, EVENT type, string msg, string prev_state, immutable(string)[] indv_types, string individual_id,
                         string event_id, long op_id)
                        {
                            try
                            {
                                //writeln("scripts B #1 *", process_name);
                                if (msg !is null && msg.length > 3 && script_vm !is null)
                                {
                                    bool prepare_if_is_script = false;

                                    foreach (itype; indv_types)
                                    {
                                        if (itype == veda_schema__PermissionStatement || itype == veda_schema__Membership)
                                            return;

                                        if (itype == veda_schema__Event)
                                            prepare_if_is_script = true;
                                    }

                                    if (onto is null)
                                        onto = context.get_onto();

                                    Individual ss;
                                    if (cbor2individual(&ss, msg) > 0)
                                    {
                                        if (prepare_if_is_script == false)
                                        {
                                            if (scripts.get(ss.uri, ScriptInfo.init) !is ScriptInfo.init)
                                                prepare_if_is_script = true;
                                        }
                                    }

                                    if (prepare_if_is_script)
                                    {
                                        prepare_event_scripts(ss, script_vm);
                                    }
                                    string[] aa;

                                    //writeln ("@d event_id=", event_id);
                                    if (event_id !is null)
                                    {
                                        aa = event_id.split("+");

                                        if (aa.length == 2)
                                        {
                                            g_parent_script_id.data = cast(char *)aa[ 1 ];
                                            g_parent_script_id.length = cast(int)aa[ 1 ].length;
                                            g_parent_document_id.data = cast(char *)aa[ 0 ];
                                            g_parent_document_id.length = cast(int)aa[ 0 ].length;
                                        }
                                        else
                                        {
                                            g_parent_script_id.data = cast(char *)empty_uid;
                                            g_parent_script_id.length = cast(int)empty_uid.length;
                                            g_parent_document_id.data = cast(char *)empty_uid;
                                            g_parent_document_id.length = cast(int)empty_uid.length;
                                        }
                                    }
                                    else
                                    {
                                        g_parent_script_id.data = cast(char *)empty_uid;
                                        g_parent_script_id.length = cast(int)empty_uid.length;
                                        g_parent_document_id.data = cast(char *)empty_uid;
                                        g_parent_document_id.length = cast(int)empty_uid.length;
                                    }

                                    if (prev_state !is null)
                                    {
                                        g_prev_state.data = cast(char *)prev_state;
                                        g_prev_state.length = cast(int)prev_state.length;
                                    }
                                    else
                                    {
                                        g_prev_state.data = cast(char *)empty_uid;
                                        g_prev_state.length = cast(int)empty_uid.length;
                                    }

                                    g_document.data = cast(char *)msg;
                                    g_document.length = cast(int)msg.length;

                                    if (user_uri !is null)
                                    {
                                        g_user.data = cast(char *)user_uri;
                                        g_user.length = cast(int)user_uri.length;
                                    }
                                    else
                                    {
                                        g_user.data = cast(char *)"cfg:VedaSystem";
                                        g_user.length = "cfg:VedaSystem".length;
                                    }

                                    string sticket = context.sys_ticket().id;
                                    g_ticket.data = cast(char *)sticket;
                                    g_ticket.length = cast(int)sticket.length;


                                    Classes super_classes;

                                    foreach (indv_type; indv_types)
                                    {
                                        if (super_classes == Classes.init)
                                        {
                                            super_classes = onto.get_super_classes(indv_type);
                                        }
                                        else
                                        {
                                            Classes i_super_classes = onto.get_super_classes(indv_type);
                                            foreach (i_super_class; i_super_classes.keys)
                                            {
                                                if (super_classes.get(i_super_class, false) == false)
                                                {
                                                    super_classes[ i_super_class ] = true;
                                                }
                                            }
                                        }
                                    }

                                    string superclasses_str = text(super_classes.keys);
                                    g_super_classes.data = cast(char *)superclasses_str;
                                    g_super_classes.length = cast(int)superclasses_str.length;

                                    foreach (script_id, script; scripts)
                                    {
                                        if (script.compiled_script !is null)
                                        {
                                            //writeln("#1 exec script:", script_id, ", script.filters=", script.filters);
                                            if (script.filters.length > 0)
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

                                                if (any_exist == false)
                                                    continue;
                                            }
                                            //log.trace("execute script:%s", script_id);

                                            if (event_id !is null && event_id.length > 1 && event_id == (individual_id ~ '+' ~ script_id))
                                            {
                                                //writeln("skip script [", script_id, "], type:", type, ", indiv.:[", individual_id, "]");
                                                continue;
                                            }


                                            try
                                            {
                                                if (trace_msg[ 300 ] == 1)
                                                    log.trace("start exec script : %s %s %d %s", script_id, individual_id, op_id, event_id);

                                                count++;
                                                script_vm.run(script.compiled_script);

                                                if (trace_msg[ 300 ] == 1)
                                                    log.trace("end exec script : %s", script_id);


                                                //*(cast(char*)script_vm) = 0;
                                            }
                                            catch (Exception ex)
                                            {
                                                log.trace_log_and_console("WARN! fail execute script : %s %s", script_id, ex.msg);
                                            }
                                        }
                                    }


//                                writeln("count:", count);

                                    //clear_script_data_cache ();
                                    // writeln("scripts B #e *", process_name);
                                }
                            }
                            finally
                            {
                                set_scripts_op_id(op_id);
                                inc_count_prep_put();
                            }
                        },
                        (CMD cmd, int arg, bool arg2)
                        {
                            if (cmd == CMD.SET_TRACE)
                                set_trace(arg, arg2);
                        },
                        (Variant v) { log.trace_log_and_console(thread_name ~ "::scripts_thread::Received some other type." ~ text(v)); });
            }
            catch (Exception ex)
            {
                log.trace("#%s EX! LINE:[%s], FILE:[%s], MSG:[%s]", thread_name, ex.line, ex.file, ex.msg);
            }
        }
    }
    catch (Throwable ex)
    {
        log.trace("#%s ERR! LINE:[%s], FILE:[%s], MSG:[%s]", thread_name, ex.line, ex.file, ex.msg);
    }
    writeln("TERMINATED: ", thread_name);
}

public void load_event_scripts()
{
    ScriptVM script_vm = context.get_ScriptVM();

    if (script_vm is null)
        return;

    //if (trace_msg[ 301 ] == 1)
    log.trace("start load db scripts");

    Ticket       sticket = context.sys_ticket();
    Individual[] res;
    vql.get(&sticket,
            "return { 'v-s:script'}
            filter { 'rdf:type' === 'v-s:Event'}",
            res);

    int count = 0;

    foreach (ss; res)
    {
        prepare_event_scripts(ss, script_vm);
    }

    //writeln ("@2");
    //if (trace_msg[ 300 ] == 1)
    log.trace("end load db scripts, count=%d ", res.length);
}

private void prepare_event_scripts(Individual ss, ScriptVM script_vm)
{
    if (trace_msg[ 310 ] == 1)
        log.trace("prepare_event_scripts uri=%s", ss.uri);

    JSONValue nil;
    try
    {
        if (ss.isExists(veda_schema__deleted, true) || ss.isExists("v-s:disabled", true))
        {
//          writeln ("SCRIPT OFF, uri=", ss.uri);
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
            ~ "var user_uri = get_env_str_var ('$user');"
            ~ "var parent_script_id = get_env_str_var ('$parent_script_id');"
            ~ "var parent_document_id = get_env_str_var ('$parent_document_id');"
            ~ "var prev_state = get_individual (ticket, '$prev_state');"
            ~ "var super_classes = get_env_str_var ('$super_classes');"
            ~ "var _script_id = '" ~ ss.uri ~ "';"
            ~ "var _event_id = document['@'] + '+' + _script_id;"
            ~ "script();"
            ~ "};"
            ~ "function script() {" ~ scripts_text ~ "};"
        ;
        try
        {
            ScriptInfo script = ScriptInfo.init;
            script.id         = ss.uri;
            script.str_script = str_script;

            script.compiled_script = script_vm.compile(cast(char *)(script.str_script ~ "\0"));
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
        //writeln ("@4");
    }
}

///////////////////////////// STAT //////////////////////////////

import core.atomic;

private shared long _count_recv_put = 0;

private long inc_count_recv_put(long delta = 1)
{
    return atomicOp !"+=" (_count_recv_put, delta);
}

public long get_count_recv_put()
{
    return atomicLoad(_count_recv_put);
}

////////////////////////////

private shared long _count_prep_put = 0;

private long inc_count_prep_put(long delta = 1)
{
    return atomicOp !"+=" (_count_prep_put, delta);
}

public long get_count_prep_put()
{
    return atomicLoad(_count_prep_put);
}
/////////////////////////////////////////////////////////////

