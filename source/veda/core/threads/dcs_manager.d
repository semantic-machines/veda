/**
 * Data Change Signal
 */

module veda.core.threads.dcs_manager;

import core.thread, std.stdio, std.conv, std.file, std.datetime, std.outbuffer, std.string;
import util.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
import veda.type, veda.core.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.log_msg, veda.onto.individual,
       veda.onto.resource, veda.util.tools;
import vibe.core.concurrency, vibe.core.task, vibe.http.router                 : URLRouter;
import vibe.inet.url, vibe.http.client, vibe.http.server, vibe.http.websockets : WebSocket, handleWebSockets;

private shared Task task_of_scripts;
private shared Task task_of_ltr_scripts;
private shared Task task_of_fanout;

void handleWebSocketConnection(scope WebSocket socket)
{
    string module_name;

    try
    {
        while (true)
        {
            if (!socket.connected)
                break;

            string   inital_message = socket.receiveText();
            string[] kv             = inital_message.split('=');

            if (kv.length == 2)
            {
                if (kv[ 0 ] == "module-name")
                {
                    module_name = kv[ 1 ];
                    if (module_name == "scripts")
                        task_of_scripts = Task.getThis();
                    else if (module_name == "ltr_scripts")
                        task_of_ltr_scripts = Task.getThis();
                    else if (module_name == "fanout")
                        task_of_fanout = Task.getThis();
                    else
                        module_name = null;
                }
            }

            if (module_name !is null)
            {
                log.trace("create chanel [%s]", module_name);

                while (true)
                {
                    string msg;
                    Task   task_to;
                    vibe.core.concurrency.receive(
                                                  (string _msg, Task _to)
                                                  {
                                                      msg = _msg;
                                                      task_to = _to;
                                                  }
                                                  );

                    //log.trace("Sending '%s'.", msg);
                    socket.send(msg);
                    //log.trace("Ok");
                    string resp = socket.receiveText();
                    //log.trace("recv '%s'", resp);

                    if (msg == "get_opid")
                    {
                        long l_res;

                        try
                        {
                            l_res = to!long (resp);
                        }
                        catch (Throwable tr)
                        {
                            l_res = -1;
                        }
                        vibe.core.concurrency.send(task_to, l_res);
                    }
                }
            }
        }
    }
    catch (Throwable tr)
    {
        log.trace("err on chanel [%s] ex=%s", module_name, tr.msg);
    }

    if (module_name == "scripts")
        task_of_scripts = shared(Task).init;
    else if (module_name == "ltr_scripts")
        task_of_ltr_scripts = shared(Task).init;
    else if (module_name == "fanout")
        task_of_fanout = shared(Task).init;

    log.trace("chanel [%s] is closed", module_name);
}


shared static this()
{
    auto router = new URLRouter;
    router.get("/ws", handleWebSockets(&handleWebSocketConnection));

    auto settings = new HTTPServerSettings;
    settings.port          = 8091;
    settings.bindAddresses = [ "127.0.0.1" ];
    listenHTTP(settings, router);
    log.trace("listen %s:%s", text(settings.bindAddresses), text(settings.port));
}


string     node_id;
Context    context;
Individual node;

enum CMD : byte { EXAMINE, GET, CLOSE }

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "DCS");
    return _log;
}

alias log mlog;
// ////// ////// ///////////////////////////////////////////
public void set_module_info(string module_name, string host, ushort port)
{
    std.concurrency.Tid tid_dcs = getTid(P_MODULE.dcs);

    if (tid_dcs != std.concurrency.Tid.init)
    {
        std.concurrency.send(tid_dcs, module_name, host, port);
    }
}

public void ev_update_individual(byte cmd, string user_uri, string cur_state, string prev_state, string event_id, long op_id)
{
    std.concurrency.Tid tid_dcs = getTid(P_MODULE.dcs);

    if (tid_dcs != std.concurrency.Tid.init)
    {
        std.concurrency.send(tid_dcs, cast(CMD)cmd, user_uri, prev_state, cur_state, event_id, op_id);
    }
}

public bool examine_modules()
{
    bool                all_ready = false;

    std.concurrency.Tid tid_dcs = getTid(P_MODULE.dcs);

    if (tid_dcs != std.concurrency.Tid.init)
    {
        std.concurrency.send(tid_dcs, CMD.EXAMINE, std.concurrency.thisTid);
        std.concurrency.receive((bool _all_ready)
                                {
                                    all_ready = _all_ready;
                                });
    }
    return all_ready;
}

long timeout = 1_000;

public shared(Task) getTaskOfPMODULE(P_MODULE pm)
{
    if (pm == P_MODULE.scripts)
        return task_of_scripts;
    else if (pm == P_MODULE.ltr_scripts)
        return task_of_ltr_scripts;
    else if (pm == P_MODULE.fanout)
        return task_of_fanout;

    return shared(Task).init;
}

public void wait_module(P_MODULE pm, long op_id)
{
    long op_id_from_module;

    Task task = getTaskOfPMODULE(pm);

    if (task_of_scripts is shared(Task).init)
        return;

    long wait_time = 0;

    while (op_id > op_id_from_module)
    {
        vibe.core.concurrency.send(task, "get_opid", Task.getThis());
        vibe.core.concurrency.receive((long _op_id)
                                      {
                                          op_id_from_module = _op_id;
                                      });

        if (op_id_from_module >= op_id)
            break;

        core.thread.Thread.sleep(dur!("msecs")(100));
        wait_time += 100;

        if (wait_time > timeout)
        {
            writeln("WARN! timeout (wait opid=", op_id, ", opid from module = ", op_id_from_module, ") wait_module:", pm);
            break;
        }
    }
}

public long get_opid(P_MODULE pm)
{
    long op_id_from_module;

    Task task = getTaskOfPMODULE(pm);

    if (task_of_scripts is shared(Task).init)
        return -1;

    vibe.core.concurrency.send(task, "get_opid", Task.getThis());
    vibe.core.concurrency.receive((long _op_id)
                                  {
                                      op_id_from_module = _op_id;
                                  });
    return op_id_from_module;
}

public void close()
{
    std.concurrency.Tid tid_dcs = getTid(P_MODULE.dcs);

    std.concurrency.send(tid_dcs, CMD.CLOSE, std.concurrency.thisTid);
    std.concurrency.receive((long _op_id)
                            {
                            });
}


Queue queue;

shared static ~this()
{
    log.trace("dsc_manager: shared static ~this");
    if (queue !is null)
    {
        queue.close();
        queue = null;
    }
}

void dcs_thread(string thread_name, string _node_id)
{
    P_MODULE[] modules = [ P_MODULE.fanout, P_MODULE.scripts, P_MODULE.ltr_scripts ];

    node_id = _node_id;
    scope (exit)
    {
        log.trace("ERR! DCS dead (exit)");
    }

    // SEND ready
    std.concurrency.receive((std.concurrency.Tid tid_response_reciever)
                            {
                                std.concurrency.send(tid_response_reciever, true);
                            });

    core.thread.Thread.getThis().name = thread_name;

    queue = new Queue("individuals-flow", Mode.RW);
    queue.remove_lock();
    queue.open();

    core.thread.Thread.sleep(dur!("msecs")(3000));

    Task main_loop_task = Task.getThis();

    while (true)
    {
        try
        {
            std.concurrency.receive(
                                    (CMD cmd, std.concurrency.Tid tid_response_reciever)
                                    {
                                        if (cmd == CMD.EXAMINE)
                                        {
                                            log.trace("examine modules");
                                            byte count_ready_module = 0;

                                            if (task_of_scripts !is shared(Task).init)
                                                count_ready_module++;

                                            count_ready_module++;

                                            log.trace("examine modules, count_ready_module=%d, all=%d", count_ready_module, 2);
                                            std.concurrency.send(tid_response_reciever, 2 == count_ready_module);
                                        }
                                        else if (cmd == CMD.CLOSE)
                                        {
                                            //TODO: send close to Task

                                            std.concurrency.send(tid_response_reciever, 0);
                                        }
                                    },
                                    (CMD cmd, string user_uri, string prev_state, string new_state, string event_id, long op_id)
                                    {
                                        Individual imm;
                                        imm.uri = text(op_id);
                                        imm.addResource("cmd", Resource(cmd));

                                        if (user_uri !is null && user_uri.length > 0)
                                            imm.addResource("user_uri", Resource(DataType.String, user_uri));

                                        imm.addResource("new_state", Resource(DataType.String, new_state));

                                        if (prev_state !is null && prev_state.length > 0)
                                            imm.addResource("prev_state", Resource(DataType.String, prev_state));

                                        if (event_id !is null && event_id.length > 0)
                                            imm.addResource("event_id", Resource(DataType.String, event_id));

                                        imm.addResource("op_id", Resource(op_id));

                                        //writeln ("*imm=[", imm, "]");

                                        string cbor = individual2cbor(&imm);
                                        //writeln("*cbor.length=", cbor.length);

                                        queue.push(cbor);

                                        if (task_of_scripts !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_scripts, text(op_id), main_loop_task);
                                            
                                        if (task_of_ltr_scripts !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_ltr_scripts, text(op_id), main_loop_task);

                                        if (task_of_fanout !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_fanout, text(op_id), main_loop_task);
                                    },
                                    (std.concurrency.Variant v) { log.trace("::dcs_thread::Received some other type. %s", text(v)); });
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }
    }
}
