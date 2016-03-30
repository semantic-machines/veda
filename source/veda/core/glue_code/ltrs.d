/**
 * Long Time Run Scripts thread
 *
 *	START - подготавливает очередь и запускает исполнение скрипта над данными из очереди
 */

module veda.core.ltrs;

private import std.concurrency, std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, core.thread, std.algorithm;
private import bind.v8d_header;
private import veda.core.util.utils, util.logger, veda.util.cbor, veda.core.util.cbor8individual, veda.core.queue;
private import veda.core.storage.lmdb_storage, veda.core.thread_context, veda.core.glue_code.script;
private import veda.type, veda.core.context, veda.core.define, veda.onto.resource, onto.lang, veda.onto.individual;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "LTRS");
    return _log;
}

Context context;
string  node_id;

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
    {
        send(tid_scripts, CMD.EXIT);
    }

    return 0;
}

private struct Task
{
    ScriptInfo script_info;
    Consumer   consumer;
    Individual codelet;
}

alias Task *[ string ] Tasks;

Tasks[ int ] tasks_2_priority;
Task *task;

void ltrs_thread(string thread_name, string _node_id)
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

    while (true)
    {
        try
        {
            receiveTimeout(msecs(0),
                           (CMD cmd)
                           {
                               if (cmd == CMD.EXIT)
                               {
                                   thread_term();
                               }
                           },
                           (CMD cmd, string new_state)
                           {
                               check_context();
                               if (cmd == CMD.START)
                               {
                                   Individual indv;
                                   if (cbor2individual(&indv, new_state) < 0)
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

                                   Tasks tasks = tasks_2_priority.get(priority, Tasks.init);

                                   if (tasks is Tasks.init)
                                       tasks_2_priority[ priority ] = tasks;

                                   task = new Task(ScriptInfo(), cs, indv);
                                   tasks[ indv.uri ] = task;
                               }
                           },
                           (Variant v) { writeln(thread_name, "::ltrs_thread::Received some other type.", v); });

            // обработка элементов очередей согласно приоритетам
            yield();


            foreach (priority; sort(tasks_2_priority.keys))
            {
                Tasks tasks = tasks_2_priority.get(priority, Tasks.init);

                foreach (id, task; tasks)
                {
                    string data = task.consumer.pop();
                    if (data !is null)
                    {
                    }
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ltrs# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}