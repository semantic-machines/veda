/**
 * Long Time Run Scripts thread
 *
 *	START - подготавливает очередь и запускает исполнение скрипта над данными из очереди
 */

module veda.core.glue_code.ltrs;

private import std.concurrency, std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, core.thread, std.algorithm;
private import bind.v8d_header;
private import veda.core.util.utils, veda.util.cbor, veda.core.util.cbor8individual, veda.core.queue;
private import veda.core.storage.lmdb_storage, veda.core.thread_context, veda.core.glue_code.script;
private import veda.type, veda.core.context, veda.core.define, veda.onto.resource, onto.lang, veda.onto.individual;

// ////// logger ///////////////////////////////////////////
import util.logger;
private logger _log;
private logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "LTRS");
    return _log;
}

private Context context;
private string  node_id;

private struct Task
{
    Consumer   consumer;
    Individual execute_script;
    string     execute_script_id;
    string     arguments_cbor;
    string     results_uri;
}

private struct Tasks
{
    Task *[ string ] list;
}

private Tasks *[ int ] tasks_2_priority;
private Task *task;

private void ltrs_thread(string thread_name, string _node_id)
{
    scope (exit)
    {
        log.trace("ERR! ltrs_thread dead (exit)");
    }

    core.thread.Thread.getThis().name = thread_name;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    void check_context()
    {
        if (context is null)
            context = new PThreadContext(node_id, thread_name, P_MODULE.ltrs);
    }

    long recv_wait_dur = 100_000_000;

    Thread.sleep(dur!("seconds")(30));
    while (true)
    {
        try
        {
            if (tasks_2_priority.length == 0)
            {
                writeln("ltrs zzzzzzz...");
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
                           (CMD cmd, string inst_of_codelet)
                           {
                               check_context();
                               if (cmd == CMD.START)
                               {
                                   Individual indv;
                                   if (cbor2individual(&indv, inst_of_codelet) < 0)
                                       return;

                                   Queue queue = new veda.core.queue.Queue("queue-ltrs-" ~ indv.uri);

                                   bool add_to_queue(string key, string value)
                                   {
                                       queue.push(value);
                                       return true;
                                   }

                                   context.get_subject_storage_db.get_of_cursor(&add_to_queue);
                                   Consumer cs = new veda.core.queue.Consumer(queue, "consumer1");

                                   int priority = cast(int)indv.getFirstInteger("v-s:priority", 16);
                                   string execute_script_id = indv.getFirstLiteral("v-s:useScript");

                                   Tasks *tasks = tasks_2_priority.get(priority, null);

                                   if (tasks is null)
                                   {
                                       tasks = new Tasks();
                                       tasks_2_priority[ priority ] = tasks;
                                   }

                                   task = new Task(cs, indv, execute_script_id, null, null);
                                   tasks.list[ indv.uri ] = task;
                               }
                           },
                           (Variant v) { writeln(thread_name, "::ltrs_thread::Received some other type.", v); });

            // обработка элементов очередей согласно приоритетам
            yield();

            if (tasks_2_priority.length > 0)
            {
                recv_wait_dur = 0;

                check_context();
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
                        if (data !is null)
                        {
                            veda.core.glue_code.scripts.execute_script(context, &sticket, data, task.execute_script_id, task.arguments_cbor,
                                                                       task.results_uri,
                                                                       thisTid);

                            task.consumer.commit();
                        }
                        else
                        {
                            //remove complete task
                            tasks.list.remove(script_uri);

                            if (tasks.list.length == 0)
                                tasks_2_priority.remove(priority);
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

////////////////////////////////////////////////////////////////////////////////////////

public Tid start_module(string node_id)
{
    Tid tid = spawn(&ltrs_thread, text(P_MODULE.ltrs), node_id);

    if (wait_starting_module(P_MODULE.ltrs, tid) == false)
        return Tid.init;

    register(text(P_MODULE.ltrs), tid);
    return locate(text(P_MODULE.ltrs));
}

public bool stop_module(Context ctx)
{
    Tid tid_scripts = ctx.getTid(P_MODULE.ltrs);

    if (tid_scripts != Tid.init)
        send(tid_scripts, CMD.EXIT);
    else
        return false;

    return true;
}

public bool execute_script(Context ctx, string execute_script_srz)
{
    Tid tid_scripts = ctx.getTid(P_MODULE.ltrs);

    if (tid_scripts != Tid.init)
        send(tid_scripts, CMD.START, execute_script_srz);
    else
        return false;

    return true;
}
