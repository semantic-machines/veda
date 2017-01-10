/**
 * core main thread
 */
module veda.core.srv.server;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
    import core.thread, std.stdio, std.string, core.stdc.string, std.outbuffer, std.datetime, std.conv, std.concurrency, std.process, std.json;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.bind.libwebsocketd, veda.server.wslink;
    import veda.core.common.context, veda.core.common.know_predicates, veda.core.common.log_msg, veda.core.impl.thread_context;
    import veda.core.common.define, veda.common.type, veda.onto.individual, veda.onto.resource, veda.util.individual8json, veda.common.logger,
           veda.core.util.utils;
    import veda.server.load_info, veda.server.acl_manager, veda.server.storage_manager, veda.server.nanomsg_channel;
}

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-server", "log", "server");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

Logger io_msg;

enum CMD : byte
{
    /// Установить
    SET = 50,
}

static this()
{
    io_msg = new Logger("pacahon", "io", "server");
    bsd_signal(SIGINT, &handleTermination2);
}

extern (C) void handleTermination2(int _signal)
{
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));

    if (_log !is null)
        _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    //_log.close();

    writeln("!SYS: ", process_name, ": preparation for the exit.");

    f_listen_exit = true;

    thread_term();
    Runtime.terminate();
}

Context g_context;

void main(char[][] args)
{
    process_name = "server";

    VedaServer veda_server = new VedaServer("127.0.0.1", 8091, log);
    veda_server.init_core(null);
    g_context = veda_server.core_context;

    log.trace("start ws listener");
    veda_server.listen(&ev_LWS_CALLBACK_GET_THREAD_ID, &ev_LWS_CALLBACK_CLIENT_WRITEABLE, &ev_LWS_CALLBACK_CLIENT_RECEIVE);
    writefln("stop ws listener");

    while (f_listen_exit == false)
        core.thread.Thread.sleep(dur!("seconds")(1000));

    writefln("send signals EXIT to threads");

    exit(P_MODULE.commiter);
    exit(P_MODULE.acl_preparer);
    exit(P_MODULE.subject_manager);
    exit(P_MODULE.ticket_manager);

    thread_term();

//    exit(P_MODULE.statistic_data_accumulator);
}

void ev_LWS_CALLBACK_GET_THREAD_ID(lws *wsi)
{
    //writeln ("server: ev_LWS_CALLBACK_GET_THREAD_ID");
}

void ev_LWS_CALLBACK_CLIENT_WRITEABLE(lws *wsi)
{
}

void ev_LWS_CALLBACK_CLIENT_RECEIVE(lws *wsi, char[] msg, ResultCode rc)
{
    //writeln("server: ev_LWS_CALLBACK_CLIENT_RECEIVE msg=", msg);
    string res;

    if (rc == ResultCode.OK)
    {
        res = g_context.execute(cast(string)msg);
    }
    else
    {
        JSONValue jres;
        jres[ "type" ]   = "OpResult";
        jres[ "result" ] = rc;
        jres[ "op_id" ]  = -1;

        res = jres.toString();
    }

    websocket_write(wsi, res);
}

class VedaServer : WSClient
{
    ushort  port;
    string  host;
    Context core_context;

    this(string _host, ushort _port, Logger log)
    {
        host = _host;
        port = _port;
        super(host, port, "/ws", "module-name=server", log);
    }

    Context init_core(string node_id)
    {
        if (node_id is null || node_id.length < 2)
            node_id = "cfg:standart_node";

        log.trace("init_core: node_id=[%s]", node_id);

        Backtrace.install(stderr);

        io_msg = new Logger("pacahon", "io", "server");
        Tid[ P_MODULE ] tids;

        try
        {
            Individual node;

            Ticket     sticket;

            core_context = new PThreadContext(node_id, "core_context", log);
            sticket      = core_context.sys_ticket();
            node         = core_context.get_configuration();
            if (node.getStatus() == ResultCode.OK)
                log.trace_log_and_console("VEDA NODE CONFIGURATION: [%s]", node);

            log.trace("init core");

            tids[ P_MODULE.subject_manager ] = spawn(&individuals_manager, P_MODULE.subject_manager, individuals_db_path, node_id);
            wait_starting_thread(P_MODULE.subject_manager, tids);

            tids[ P_MODULE.ticket_manager ] = spawn(&individuals_manager, P_MODULE.ticket_manager, tickets_db_path, node_id);
            wait_starting_thread(P_MODULE.ticket_manager, tids);

            tids[ P_MODULE.acl_preparer ] = spawn(&acl_manager, text(P_MODULE.acl_preparer), acl_indexes_db_path);
            wait_starting_thread(P_MODULE.acl_preparer, tids);

            tids[ P_MODULE.commiter ] =
                spawn(&commiter, text(P_MODULE.commiter));
            wait_starting_thread(P_MODULE.commiter, tids);

            tids[ P_MODULE.statistic_data_accumulator ] = spawn(&statistic_data_accumulator, text(P_MODULE.statistic_data_accumulator));
            wait_starting_thread(P_MODULE.statistic_data_accumulator, tids);

            tids[ P_MODULE.n_channel ] = spawn(&nanomsg_channel, text(P_MODULE.n_channel));
            wait_starting_thread(P_MODULE.n_channel, tids);

            tids[ P_MODULE.print_statistic ] = spawn(&print_statistic, text(P_MODULE.print_statistic),
                                                     tids[ P_MODULE.statistic_data_accumulator ]);
            wait_starting_thread(P_MODULE.print_statistic, tids);

            foreach (key, value; tids)
                register(text(key), value);

            sticket = core_context.sys_ticket(true);
            string guest_ticket = core_context.get_ticket_from_storage("guest");

            if (guest_ticket is null)
                core_context.create_new_ticket("cfg:Guest", "4000000", "guest");

            /////////////////////////////////////////////////////////////////////////////////////////////////////////
            if (node.getStatus() != ResultCode.OK)
            {
                core_context.reopen_ro_subject_storage_db();
                core_context.reopen_ro_acl_storage_db();
                node = core_context.get_individual(&sticket, node_id);

                log.trace_log_and_console("VEDA NODE CONFIGURATION:[%s]", node);
            }

            return core_context;
        } catch (Throwable ex)
        {
            writeln("Exception: ", ex.msg);
            return null;
        }
    }

    bool wait_starting_thread(P_MODULE tid_idx, ref Tid[ P_MODULE ] tids)
    {
        bool res;
        Tid  tid = tids[ tid_idx ];

        if (tid == Tid.init)
            throw new Exception("wait_starting_thread: Tid=" ~ text(tid_idx) ~ " not found", __FILE__, __LINE__);

        log.trace("START THREAD... : %s", text(tid_idx));
        send(tid, thisTid);
        receive((bool isReady)
                {
                    res = isReady;
                    //if (trace_msg[ 50 ] == 1)
                    log.trace("START THREAD IS SUCCESS: %s", text(tid_idx));
                    if (res == false)
                        log.trace("FAIL START THREAD: %s", text(tid_idx));
                });
        return res;
    }

    void shutdown_core()
    {
    }
}

public void exit(P_MODULE module_id)
{
    Tid tid_module = getTid(module_id);

    if (tid_module != Tid.init)
    {
        writefln("send command EXIT to thread_%s", text(module_id));
        send(tid_module, CMD_EXIT, thisTid);
        receive((bool _res) {});
    }
}

void commiter(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;
    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    bool is_exit = false;

    while (is_exit == false)
    {
        receiveTimeout(dur!("seconds")(1),
                       (byte cmd, Tid tid_response_reciever)
                       {
                           if (cmd == CMD_EXIT)
                           {
                               is_exit = true;
                               writefln("[%s] recieve signal EXIT", "commiter");
                               send(tid_response_reciever, true);
                           }
                       },
                       (OwnerTerminated ot)
                       {
                           return;
                       },
                       (Variant v) { writeln(thread_name, "::commiter::Received some other type.", v); });

        veda.server.storage_manager.flush_int_module(P_MODULE.subject_manager, false);
        veda.server.acl_manager.flush(false);
        veda.server.storage_manager.flush_int_module(P_MODULE.ticket_manager, false);
    }
}
