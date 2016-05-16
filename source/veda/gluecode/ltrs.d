/**
 * Long Time Run Scripts thread
 *
 *	START - подготавливает очередь и запускает исполнение скрипта над данными из очереди
 */

module veda.gluecode.ltrs;

private import std.concurrency, std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, core.thread, std.algorithm, std.uuid;
private import veda.gluecode.v8d_header, veda.gluecode.script;
private import veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
private import veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.type, veda.core.common.context, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual;

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
    string     execute_script_cbor;
    string     codelet_id;
}

private struct Tasks
{
    Task *[ string ] list;
}

private Tasks *[ int ] tasks_2_priority;
private Task *task;

/// Команды используемые процессами
enum CMD : byte
{
    EXIT         = 49,
    START        = 52
}


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
                           (CMD cmd, string inst_of_codelet)
                           {
                               //Thread.sleep(dur!("seconds")(15));
                               check_context();
                               if (cmd == CMD.START)
                               {
                                   Individual indv;
                                   if (cbor2individual(&indv, inst_of_codelet) < 0)
                                       return;

                                   if (indv.getFirstBoolean("v-s:isSuccess") == true)
                                       return;

                                   string queue_name = randomUUID().toString();

                                   context.unload_subject_storage(queue_name);
                                   Queue queue = new Queue(queue_name, Mode.R);
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
                                           writeln("ltrs:Consumer not open");
                                   }
                                   else
                                       writeln("ltrs:Queue not open");
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
                            //veda.core.glue_code.scripts.execute_script(context, &sticket, data, task.codelet_id, task.execute_script_cbor, thisTid);

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

                            task.consumer.remove();
                            task.consumer.queue.remove();
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

public bool stop_module()
{
    Tid tid_scripts = getTid(P_MODULE.ltrs);

    if (tid_scripts != Tid.init)
        send(tid_scripts, CMD.EXIT);
    else
        return false;

    return true;
}

public bool execute_script(string execute_script_srz)
{
    Tid tid_scripts = getTid(P_MODULE.ltrs);

    if (tid_scripts != Tid.init)
        send(tid_scripts, CMD.START, execute_script_srz);
    else
        return false;

    return true;
}
