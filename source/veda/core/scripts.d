/**
 * выполнение JS скриптов
 */
module veda.core.scripts;

private
{
    import std.json, std.stdio, std.string, std.array, std.datetime, std.concurrency, std.conv, std.file;
    import core.thread;
    import util.container, util.utils, util.logger, util.cbor, veda.core.util.cbor8individual;
    import type;
    import veda.onto.individual, veda.onto.resource, veda.onto.onto;
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
        _log = new logger("core-" ~ proccess_name, "log", "SCRIPTS");
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

public void condition_thread(string thread_name, string node_id)
{
    core.thread.Thread.getThis().name = thread_name;

    context   = new PThreadContext(node_id, thread_name, P_MODULE.condition);
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
                                    prepare_condition(ss, script_vm);
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
                         string event_id)
                        {
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
                                    g_user.data = cast(char *)"v-a:VedaSystem";
                                    g_user.length = "v-a:VedaSystem".length;
                                }

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
                                        //writeln("#2 exec script:", script_id);

                                        if (event_id !is null && event_id.length > 1 && event_id == (individual_id ~ script_id))
                                        {
                                            //writeln("skip script [", script_id, "], type:", type, ", indiv.:[", individual_id, "]");
                                            continue;
                                        }


                                        try
                                        {
                                            if (trace_msg[ 300 ] == 1)
                                                log.trace("exec script : %s ", script_id);

                                            count++;
                                            script_vm.run(script.compiled_script);

                                            if (trace_msg[ 300 ] == 1)
                                                log.trace("end exec script");
                                        }
                                        catch (Exception ex)
                                        {
                                            log.trace_log_and_console("EX!condition.receive : %s", ex.msg);
                                        }
                                    }
                                }

//                                writeln("count:", count);

                                //clear_script_data_cache ();
                            }
                        },
                        (CMD cmd, int arg, bool arg2)
                        {
                            if (cmd == CMD.SET_TRACE)
                                set_trace(arg, arg2);
                        },
                        (Variant v) { log.trace_log_and_console(thread_name ~ "::condition_thread::Received some other type." ~ text(v)); });
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
    //writeln ("@1");
    ScriptVM script_vm = context.get_ScriptVM();

    if (script_vm is null)
        return;

    //if (trace_msg[ 301 ] == 1)
    log.trace("start load scripts");

    Ticket       sticket = context.sys_ticket();
    Individual[] res;
    vql.get(&sticket,
            "return { 'v-s:script'}
            filter { 'rdf:type' == 'v-s:Event'}",
            res);

    int count = 0;

    foreach (ss; res)
    {
        prepare_condition(ss, script_vm);
    }

    //writeln ("@2");
    //if (trace_msg[ 300 ] == 1)
    log.trace("end load scripts, count=%d ", res.length);
}

private void prepare_condition(Individual ss, ScriptVM script_vm)
{
    if (trace_msg[ 310 ] == 1)
        log.trace("prepare_condition uri=%s", ss.uri);

    JSONValue nil;
    try
    {
        string condition_text = ss.getFirstResource(veda_schema__script).literal;
        if (condition_text.length <= 0)
            return;

        string str_script =
            "var ticket = ''; var user_uri = get_env_str_var ('$user'); var prev_state = get_individual (ticket, '$prev_state'); var document = get_individual (ticket, '$document'); if (document) { var _script_id = '"
            ~ ss.uri ~
            "'; var _event_id = document['@'] + _script_id; " ~ condition_text ~ "}";
        try
        {
            ScriptInfo script = ScriptInfo.init;
            script.id         = ss.uri;
            script.str_script = str_script;

            script.compiled_script = script_vm.compile(cast(char *)(script.str_script ~ "\0"));
            if (trace_msg[ 310 ] == 1)
                log.trace("#compile script.id=%s, text=%s", script.id, script.str_script);

            //writeln("condition_text:", condition_text);

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
