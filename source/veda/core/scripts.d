/**
 * выполнение JS скриптов
 */
module veda.core.scripts;

private
{
    import std.json, std.stdio, std.string, std.array, std.datetime, std.concurrency, std.conv, std.file;
    import core.thread;
    import util.container, util.utils, util.logger, util.cbor, veda.core.util.cbor8individual;
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

struct ScriptInfo
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

public void send_put(Context ctx, Ticket *ticket, EVENT ev_type, string new_state, string prev_state, Resource[ string ] rdfType,
                     Individual *individual,
                     string event_id,
                     long op_id)
{
    Tid tid_scripts = ctx.getTid(P_MODULE.scripts);

    if (tid_scripts != Tid.init)
    {
        if (rdfType.anyExist(veda_schema__Event))
        {
            // изменения в v-s:Event, послать модуль Condition сигнал о перезагузке скрипта
            send(tid_scripts, CMD.RELOAD, new_state, thisTid);
            receive((bool){});
        }

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

    veda.core.scripts.inc_count_recv_put();
}


public void scripts_thread(string thread_name, string node_id)
{
    core.thread.Thread.getThis().name = thread_name;

    context   = new PThreadContext(node_id, thread_name, P_MODULE.scripts);
    g_context = context;

    vql = new VQL(context);
    load();

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
                        (CMD cmd, string arg, Tid to)
                        {
                            if (cmd == CMD.RELOAD)
                            {
                                Individual ss;
                                if (cbor2individual(&ss, arg) > 0)
                                {
                                    prepare_scripts(ss, script_vm);
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
                            //writeln("scripts B #1 *", process_name);
                            if (msg !is null && msg.length > 3 && script_vm !is null)
                            {
                                if (onto is null)
                                    onto = context.get_onto();

                                if (prev_state !is null)
                                {
                                    g_prev_state.data = cast(char *)prev_state;
                                    g_prev_state.length = cast(int)prev_state.length;
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

                                                foreach (filter; script.filters.keys)
                                                {
                                                    if (onto.isSubClass(cast(string)indv_type, filter) == true)
                                                    {
                                                        any_exist = true;
                                                        break;
                                                    }
                                                }
                                            }

                                            if (any_exist == false)
                                                continue;
                                        }
                                        //log.trace("execute script:%s", script_id);

                                        if (event_id !is null && event_id.length > 1 && event_id == (individual_id ~ script_id))
                                        {
                                            //writeln("skip script [", script_id, "], type:", type, ", indiv.:[", individual_id, "]");
                                            continue;
                                        }


                                        try
                                        {
                                            if (trace_msg[ 300 ] == 1)
                                                log.trace("start exec script : %s %s", script_id, individual_id);

                                            count++;
                                            script_vm.run(script.compiled_script);

                                            if (trace_msg[ 300 ] == 1)
                                                log.trace("end exec script : %s %s", script_id, individual_id);
                                        }
                                        catch (Exception ex)
                                        {
                                            log.trace_log_and_console("EX!scripts.receive : %s", ex.msg);
                                        }
                                    }
                                }


//                                writeln("count:", count);

                                //clear_script_data_cache ();
                                // writeln("scripts B #e *", process_name);
                            }
                            set_scripts_op_id(op_id);
                            inc_count_prep_put();
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
                writeln(thread_name, "EX!: receive");
            }
        }
    }
    catch (Exception ex)
    {
        writeln(thread_name, "EX!: main loop");
    }
    writeln("TERMINATED: ", thread_name);
}

public void load()
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
            filter { 'rdf:type' == 'v-s:Event'}",
            res);

    int count = 0;

    foreach (ss; res)
    {
        prepare_scripts(ss, script_vm);
    }

    //writeln ("@2");
    //if (trace_msg[ 300 ] == 1)
    log.trace("end load db scripts, count=%d ", res.length);
}

private void prepare_scripts(Individual ss, ScriptVM script_vm)
{
    if (trace_msg[ 310 ] == 1)
        log.trace("prepare_scripts uri=%s", ss.uri);

    JSONValue nil;
    try
    {
        string scripts_text = ss.getFirstResource(veda_schema__script).literal;
        if (scripts_text.length <= 0)
            return;

        string str_script =
            "var ticket = get_env_str_var ('$ticket');"
            ~ "var user_uri = get_env_str_var ('$user');"
            ~ "var prev_state = get_individual (ticket, '$prev_state');"
            ~ "var document = get_individual (ticket, '$document');"
            ~ "if (document) {"
            ~ "var _script_id = '" ~ ss.uri ~ "';"
            ~ "var _event_id = document['@'] + _script_id;"
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
                log.trace("#compile script.id=%s, text=%s", script.id, script.str_script);

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
            log.trace_log_and_console("error:compile script :%s", ex.msg);
        }
    }
    catch (Exception ex)
    {
        log.trace_log_and_console("error:load script :%s", ex.msg);
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

