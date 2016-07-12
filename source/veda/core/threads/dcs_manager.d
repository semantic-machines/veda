/**
 * Data Change Signal
 */

module veda.core.threads.dcs_manager;

import core.thread, std.stdio, std.conv, std.file, std.datetime, std.outbuffer, std.string, core.atomic;
import util.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
import veda.type, veda.core.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.log_msg, veda.onto.individual,
       veda.onto.resource, veda.util.tools;
import vibe.core.concurrency, vibe.core.task, vibe.http.router                 : URLRouter;
import vibe.inet.url, vibe.http.client, vibe.http.server, vibe.http.websockets : WebSocket, handleWebSockets;

private shared Task task_of_ft_indexer;
private shared Task task_of_scripts;
private shared Task task_of_ltr_scripts;
private shared Task task_of_fanout;

struct UidInfo
{
    long update_counter;
    long opid;
}

private shared        UidInfo[ string ] _info_2_uid;
private shared long   _last_opid;
//private shared long   _count_opid;
private shared Object _mutex = new Object();

public void set_updated_uid(string uid, long opid, long update_counter)
{
    synchronized (_mutex)
    {
        //writeln ("@ uid=", uid, ", opid=", opid, ", update_counter=", update_counter);
        _info_2_uid[ uid ] = UidInfo(update_counter, opid);
        _last_opid         = opid;
//        core.atomic.atomicOp !"+=" (_count_opid, 1);
    }
}

public long get_counter_4_uid(string uid)
{
    long res;

    synchronized (_mutex)
    {
        if ((uid in _info_2_uid) !is null)
            res = _info_2_uid[ uid ].update_counter;
    }
    return res;
}

public long get_last_opid()
{
    synchronized (_mutex)
    {
        return _last_opid;
    }
}
/*
   public long get_count_opid()
   {
    synchronized (_mutex)
    {
        return _count_opid;
    }
   }
 */
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

                    //writeln ("@module_name=", module_name);

                    if (module_name == "scripts")
                        task_of_scripts = Task.getThis();
                    else if (module_name == "ltr_scripts")
                        task_of_ltr_scripts = Task.getThis();
                    else if (module_name == "fanout")
                        task_of_fanout = Task.getThis();
                    else if (module_name == "fulltext_indexer")
                        task_of_ft_indexer = Task.getThis();
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
    else if (module_name == "fulltext_indexer")
        task_of_ft_indexer = shared(Task).init;

    log.trace("chanel [%s] is closed", module_name);
}

void handleWebSocketConnection_CCUS(scope WebSocket socket)
{
    const(HTTPServerRequest)hsr = socket.request();

    // Client Cache Update Subscription
    string chid;
    long[ string ] count_2_uid;

    string get_list_of_changes()
    {
        string   res;

        string[] keys = count_2_uid.keys;
        foreach (i_uid; keys)
        {
            long i_count = count_2_uid[ i_uid ];
            long g_count = get_counter_4_uid(i_uid);
            if (g_count > i_count)
            {
                i_count = g_count;

                if (res is null)
                    res ~= i_uid ~ "=" ~ text(i_count);
                else
                    res ~= "," ~ i_uid ~ "=" ~ text(i_count);

                count_2_uid[ i_uid ] = i_count;
            }
        }
        return res;
    }

    string get_list_of_subscribe()
    {
        string   res;

        string[] keys = count_2_uid.keys;
        foreach (i_uid; keys)
        {
            long i_count = count_2_uid[ i_uid ];
            long g_count = get_counter_4_uid(i_uid);
            if (g_count > i_count)
            {
                i_count              = g_count;
                count_2_uid[ i_uid ] = i_count;
            }
            if (res is null)
                res ~= i_uid ~ "=" ~ text(i_count);
            else
                res ~= "," ~ i_uid ~ "=" ~ text(i_count);
        }
        return res;
    }

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
                if (kv[ 0 ] == "ccus")
                {
                    chid = kv[ 1 ];

                    log.trace("init chanel [%s]", chid);
                    //task_2_client[hsr.clientAddress] = Task.getThis();
                    //task_2_client ~= Task.getThis();
                }
            }

            if (chid !is null)
            {
                long last_check_opid = 0;

                long timeout = 1;

                while (true)
                {
                    string msg_from_sock;

                    if (socket.waitForData(dur!("msecs")(1000)) == true)
                        msg_from_sock = socket.receiveText();

                    if (msg_from_sock !is null && msg_from_sock.length > 0)
                    {
                        if (msg_from_sock[ 0 ] == '=')
                        {
                            string res = get_list_of_subscribe();

                            if (res !is null)
                            {
                                socket.send(res);
                            }
                        }
                        else if (msg_from_sock.length == 2 && msg_from_sock[ 0 ] == '*' && msg_from_sock[ 1 ] == '-')
                        {
                            count_2_uid = count_2_uid.init;
                        }
                        else if (msg_from_sock.length > 3)
                        {
                            foreach (data; msg_from_sock.split(','))
                            {
                                try
                                {
                                    string[] expr = data.split('=');

                                    string   uid_info;
                                    if (expr.length > 0)
                                        uid_info = expr[ 0 ];

                                    if (expr.length == 2)
                                    {
                                        if (uid_info.length > 2)
                                        {
                                            string uid = uid_info[ 1..$ ];

                                            if (uid_info[ 0 ] == '+')
                                            {
                                                uid = uid_info[ 1..$ ];
                                                long uid_counter = to!long (expr[ 1 ]);
                                                count_2_uid[ uid ] = uid_counter;
                                            }
                                        }
                                    }
                                    else if (expr.length == 1)
                                    {
                                        if (uid_info.length > 2)
                                        {
                                            string uid = uid_info[ 1..$ ];
                                            if (uid_info[ 0 ] == '-')
                                            {
                                                uid = uid_info[ 1..$ ];
                                                count_2_uid.remove(uid);
                                            }
                                        }
                                    }
                                }
                                catch (Throwable tr)
                                {
                                    log.trace("recv msg:[%s], %s", data, tr);
                                }
                            }
                        }
                    }


                    string msg;
                    vibe.core.concurrency.receiveTimeout(dur!("msecs")(1), (string _msg)
                                                         {
                                                             writeln("@@recv ch data from thread");
                                                             msg = _msg;
                                                         }
                                                         );

                    long last_opid = get_last_opid();
                    if (last_check_opid < last_opid)
                    {
                        string res = get_list_of_changes();
                        if (res !is null)
                        {
                            socket.send(res);
                        }
                        last_check_opid = last_opid;
                    }
                }
            }
        }
    }
    catch (Throwable tr)
    {
        log.trace("err on chanel [%s] ex=%s", chid, tr.msg);
    }

    scope (exit)
    {
        log.trace("chanel [%s] closed", chid);
    }
}

shared static this()
{
    auto router = new URLRouter;
    router.get("/ws", handleWebSockets(&handleWebSocketConnection));

    auto settings = new HTTPServerSettings;
    settings.port          = 8091;
    settings.bindAddresses = [ "127.0.0.1" ];
    listenHTTP(settings, router);
    log.trace("listen /ws %s:%s", text(settings.bindAddresses), text(settings.port));

    router.get("/ccus", handleWebSockets(&handleWebSocketConnection_CCUS));
    settings      = new HTTPServerSettings;
    settings.port = 8088;
    //settings.bindAddresses = [ "127.0.0.1" ];
    listenHTTP(settings, router);
    log.trace("listen /ccus %s:%s", text(settings.bindAddresses), text(settings.port));
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

public void ev_update_individual(byte cmd, string user_uri, string indv_uri, string cur_state, string prev_state, string event_id, long op_id,
                                 long update_counter)
{
    std.concurrency.Tid tid_dcs = getTid(P_MODULE.dcs);

    if (tid_dcs != std.concurrency.Tid.init)
    {
        std.concurrency.send(tid_dcs, cast(CMD)cmd, user_uri, indv_uri, prev_state, cur_state, event_id, op_id, update_counter);
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
    else if (pm == P_MODULE.fulltext_indexer)
        return task_of_ft_indexer;

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
                                    (CMD cmd, string user_uri, string indv_uri, string prev_state, string new_state, string event_id, long opid,
                                     long update_counter)
                                    {
                                        Individual imm;
                                        imm.uri = text(opid);
                                        imm.addResource("cmd", Resource(cmd));

                                        if (user_uri !is null && user_uri.length > 0)
                                            imm.addResource("user_uri", Resource(DataType.String, user_uri));

                                        imm.addResource("new_state", Resource(DataType.String, new_state));

                                        if (prev_state !is null && prev_state.length > 0)
                                            imm.addResource("prev_state", Resource(DataType.String, prev_state));

                                        if (event_id !is null && event_id.length > 0)
                                            imm.addResource("event_id", Resource(DataType.String, event_id));

                                        imm.addResource("op_id", Resource(opid));

                                        //writeln ("*imm=[", imm, "]");

                                        string cbor = individual2cbor(&imm);
                                        //writeln("*cbor.length=", cbor.length);

                                        queue.push(cbor);

                                        if (task_of_scripts !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_scripts, text(opid), main_loop_task);

                                        if (task_of_ltr_scripts !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_ltr_scripts, text(opid), main_loop_task);

                                        if (task_of_fanout !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_fanout, text(opid), main_loop_task);

                                        if (task_of_ft_indexer !is shared(Task).init)
                                            vibe.core.concurrency.send(task_of_ft_indexer, text(opid), main_loop_task);

                                        set_updated_uid(indv_uri, opid, update_counter);
                                    },
                                    (std.concurrency.Variant v) { log.trace("::dcs_thread::Received some other type. %s", text(v)); });
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }
    }
}
