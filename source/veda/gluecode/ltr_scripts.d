/**
 * ltr_scripts module
 * https://github.com/semantic-machines/veda/blob/f9fd83a84aea0f9721299dff6d673dd967202ce2/source/veda/core/glue_code/scripts.d
 * https://github.com/semantic-machines/veda/blob/f9fd83a84aea0f9721299dff6d673dd967202ce2/source/veda/core/glue_code/ltrs.d
 */
module veda.gluecode.ltr_scripts;

private
{
    import core.thread, core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, std.container.array;
    import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.uuid, std.concurrency, std.algorithm;
    import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
    import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
    import veda.core.common.context, veda.util.tools, veda.core.log_msg, veda.core.common.know_predicates, veda.onto.onto;
    import veda.process.child_process;
    import search.vel, search.vql, veda.gluecode.script, veda.gluecode.v8d_header;
}

Tid tid_ltr_scripts;

extern (C) void handleTermination1(int _signal)
{
    log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));
    log.close();
    shutdown_ltr_scripts();
    writeln("!SYS: ", process_name, ": exit");
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination1);
    process_name = "ltr_scripts";
}

void main(char[][] args)
{
    core.thread.Thread.sleep(dur!("seconds")(2));

    ScriptProcess p_script = new ScriptProcess(P_MODULE.ltr_scripts, "127.0.0.1", 8091);
    //log = p_script.log();

    tid_ltr_scripts = spawn(&ltrs_thread, p_script.parent_url);

    p_script.run();

    shutdown_ltr_scripts();
}

private struct Task
{
    Consumer   consumer;
    Individual executed_script;
    string     executed_script_cbor;
    string     codelet_id;
}

private struct Tasks
{
    Task *[ string ] list;
}

/// Команды используемые процессами
enum CMD : byte
{
    EXIT  = 49,
    START = 52
}

Onto    onto;
Context context;
ScriptInfo[ string ] codelet_scripts;
Array!string codelet_scripts_order;

VQL      vql;
string   empty_uid;
string   vars_for_codelet_script;

ScriptVM script_vm;

Tasks *[ int ] tasks_2_priority;
Task *task;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-ltr_scripts", "log", "LTR-SCRIPTS");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

private void ltrs_thread(string parent_url)
{
    process_name = "ltr_scripts";

    scope (exit)
    {
        log.trace("ERR! ltrs_thread dead (exit)");
    }

//    core.thread.Thread.getThis().name = thread_name;

    context = new PThreadContext("cfg:standart_node", "ltr_scripts", P_MODULE.ltr_scripts, parent_url);


    vars_for_codelet_script =
        "var user_uri = get_env_str_var ('$user');"
        ~ "var execute_script = get_individual (ticket, '$execute_script');"
        ~ "var prev_state = get_individual (ticket, '$prev_state');"
        ~ "var super_classes = get_env_str_var ('$super_classes');"
        ~ "var _event_id = document['@'] + '+' + _script_id;";

    vql = new VQL(context);

    script_vm = get_ScriptVM(context);

    long recv_wait_dur = 100_000_000;

//    Thread.sleep(dur!("seconds")(30));
    while (true)
    {
        try
        {
            if (tasks_2_priority.length == 0)
            {
                //writeln("ltrs zzzzzzz...");
                //Thread.sleep(dur!("seconds")(1));
                recv_wait_dur = 100_000_000;
            }

            receiveTimeout(msecs(recv_wait_dur),
                           (CMD cmd)
                           {
                               if (cmd == CMD.EXIT)
                               {
                                   thread_term();
                               }
                           },
                           (CMD cmd, string inst_of_codelet, string queue_id)
                           {
                               //Thread.sleep(dur!("seconds")(15));
//                               check_context();
                               if (cmd == CMD.START)
                               {
                                   Individual indv;
                                   if (cbor2individual(&indv, inst_of_codelet) < 0)
                                       return;

                                   if (indv.getFirstBoolean("v-s:isSuccess") == true)
                                       return;

                                   Queue queue = new Queue(queue_id, Mode.R);
                                   if (queue.open())
                                   {
                                       Consumer cs = new Consumer(queue, "consumer1");

                                       if (cs.open())
                                       {
                                           int priority = cast(int)indv.getFirstInteger("v-s:priority", 16);
                                           string codelet_id = indv.getFirstLiteral("v-s:useScript");

                                           Tasks *tasks = tasks_2_priority.get(priority, null);

                                           if (tasks is null)
                                           {
                                               tasks = new Tasks();
                                               tasks_2_priority[ priority ] = tasks;
                                           }

                                           task = new Task(cs, indv, inst_of_codelet, codelet_id);
                                           tasks.list[ indv.uri ] = task;
                                       }
                                       else
                                           writeln("ltrs:Consumer not open :", cs);
                                   }
                                   else
                                       writeln("ltrs:Queue not open :", queue);
                               }
                           },
                           (Variant v) { writeln("ltrs_thread::Received some other type.", v); });
            // обработка элементов очередей согласно приоритетам
            yield();

            if (tasks_2_priority.length > 0)
            {
                recv_wait_dur = 0;

                //check_context();
                Ticket sticket = context.sys_ticket();

                foreach (priority; sort(tasks_2_priority.keys))
                {
                    Tasks *tasks = tasks_2_priority.get(priority, null);

                    if (tasks is null)
                        continue;

                    foreach (script_uri; tasks.list.keys.dup)
                    {
                        task = tasks.list.get(script_uri, null);

                        if (tasks is null)
                            continue;

                        string data = task.consumer.pop();
                        //writeln ("@ data from queue=", data);
                        if (data !is null)
                        {
                            execute_script(sticket.user_uri, data, task.codelet_id, task.executed_script_cbor);

                            bool res = task.consumer.commit();
                            if (res == false)
                            {
                                writeln("Queue commit fail !!!!");
                                break;
                            }
                        }
                        else
                        {
                            //remove complete task
                            tasks.list.remove(script_uri);

                            if (tasks.list.length == 0)
                                tasks_2_priority.remove(priority);

                            //task.consumer.remove();
                            //task.consumer.queue.remove();
                        }
                    }
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ltrs# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.info);
        }
    }
}

bool execute_script(string user_uri, string msg, string script_uri, string executed_script_cbor)
{
    if (msg is null || msg.length <= 3 || script_vm is null ||
        script_uri is null || script_uri.length <= 3 ||
        executed_script_cbor is null || executed_script_cbor.length <= 3)
        return true;

    Individual indv;
    if (cbor2individual(&indv, msg) < 0)
    {
        writeln("ERR msg=", msg);
        return true;
    }

    if (onto is null)
        onto = context.get_onto();

    Resources   types = indv.resources.get(rdf__type, Resources.init);
    MapResource rdfType;
    setMapResources(types, rdfType);

    g_document.data   = cast(char *)msg;
    g_document.length = cast(int)msg.length;

    g_execute_script.data   = cast(char *)executed_script_cbor;
    g_execute_script.length = cast(int)executed_script_cbor.length;

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


    Ticket sticket    = context.sys_ticket();
    string sticket_id = sticket.id;
    g_ticket.data   = cast(char *)sticket_id;
    g_ticket.length = cast(int)sticket_id.length;

    set_g_super_classes(rdfType.keys, onto);

    ScriptInfo script = codelet_scripts.get(script_uri, ScriptInfo.init);

    if (script is ScriptInfo.init)
    {
        Individual codelet = context.get_individual(&sticket, script_uri);
        prepare_script(codelet_scripts, codelet_scripts_order, codelet, script_vm, vars_for_codelet_script);
    }

    if (script.compiled_script !is null)
    {
        if (script.filters.length > 0 && isFiltred(&script, rdfType.keys, onto) == false)
            return true;

        try
        {
            //if (trace_msg[ 300 ] == 1)
            log.trace("start exec ltr-script : %s %s", script.id, indv.uri);

            script.compiled_script.run();
            bool res = commit();
            if (res == false)
            {
                log.trace("fail exec event script : %s", script.id);
                return false;
            }

            //if (trace_msg[ 300 ] == 1)
            log.trace("end exec ltr-script : %s", script.id);
        }
        catch (Exception ex)
        {
            log.trace_log_and_console("WARN! fail execute ltr-script : %s %s", script.id, ex.msg);
        }
    }

    return true;
}

class ScriptProcess : ChildProcess
{
    long count_sckip = 0;

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        super(_module_name, _host, _port);
    }

    override bool prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id)
    {
        if (new_indv.isExists("rdf:type", Resource(DataType.Uri, "v-s:ExecuteScript")) == false)
            return true;

        if (new_indv.getFirstBoolean("v-s:isSuccess") == true)
            return true;

        string queue_id = randomUUID().toString();

        context.unload_subject_storage(queue_id);

        start_script(new_bin, queue_id);

        return true;
    }

    override void configure()
    {
    }
}

private void start_script(string execute_script_srz, string queue_id)
{
    if (tid_ltr_scripts != Tid.init)
        send(tid_ltr_scripts, CMD.START, execute_script_srz, queue_id);
}

private void shutdown_ltr_scripts()
{
    if (tid_ltr_scripts != Tid.init)
        send(tid_ltr_scripts, CMD.EXIT);
}

