/**
 * core main thread
 */
module veda.core.srv.server;

private
{
    import core.thread, std.stdio, std.string, core.stdc.string, std.outbuffer, std.datetime, std.conv, std.concurrency, std.process, std.json;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.bind.libwebsocketd, veda.server.wslink;
    import veda.core.common.context, veda.core.common.know_predicates, veda.core.common.log_msg, veda.core.impl.thread_context;
    import veda.core.common.define, veda.common.type, veda.onto.individual, veda.onto.resource, veda.util.individual8json, util.logger,
           veda.core.util.utils;
    import veda.server.load_info, veda.server.acl_manager, veda.server.storage_manager, veda.server.nanomsg_chanel;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "server");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

logger io_msg;

enum CMD : byte
{
    /// Установить
    SET = 50,
}


static this()
{
    io_msg = new logger("pacahon", "io", "server");
}

Context g_context;

void main(char[][] args)
{
    process_name = "server";

    VedaServer veda_server = new VedaServer("127.0.0.1", 8091);
    veda_server.init_core(null);
    g_context = veda_server.core_context;

    veda_server.listen(&ev_LWS_CALLBACK_GET_THREAD_ID, &ev_LWS_CALLBACK_CLIENT_RECEIVE);

    while (true)
        core.thread.Thread.sleep(dur!("seconds")(1000));
}

void ev_LWS_CALLBACK_GET_THREAD_ID()
{
    //writeln ("server: ev_LWS_CALLBACK_GET_THREAD_ID");
}

void ev_LWS_CALLBACK_CLIENT_RECEIVE(lws *wsi, char[] msg)
{
    //writeln("server: ev_LWS_CALLBACK_CLIENT_RECEIVE msg=", msg);
    string res = g_context.execute(cast(string)msg);

    websocket_write_back(wsi, res);
}

class VedaServer : WSLink
{
    ushort  port;
    string  host;
    Context core_context;

    this(string _host, ushort _port)
    {
        host = _host;
        port = _port;
        super(host, port);
    }

    Context init_core(string node_id)
    {
        if (node_id is null || node_id.length < 2)
            node_id = "cfg:standart_node";

        log.trace("init_core: node_id=[%s]", node_id);

        Backtrace.install(stderr);

        io_msg = new logger("pacahon", "io", "server");
        Tid[ P_MODULE ] tids;

        try
        {
            Individual node;

            Ticket     sticket;

            core_context = new PThreadContext(node_id, "core_context", P_MODULE.nop, log);
            sticket      = core_context.sys_ticket();
            node         = core_context.getConfiguration();
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

            tids[ P_MODULE.n_chanel ] = spawn(&nanomsg_chanel, text(P_MODULE.n_chanel));
            wait_starting_thread(P_MODULE.n_chanel, tids);

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

void commiter(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;
    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    while (true)
    {
        core.thread.Thread.sleep(dur!("seconds")(1));
        veda.server.storage_manager.flush_int_module(P_MODULE.subject_manager, false);
        core.thread.Thread.sleep(dur!("seconds")(1));
        veda.server.acl_manager.flush(false);
        veda.server.storage_manager.flush_int_module(P_MODULE.ticket_manager, false);
    }
}
