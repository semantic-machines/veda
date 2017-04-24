/**
 * master storage
 */
module veda.mstorage.server;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
    import core.thread, std.stdio, std.string, core.stdc.string, std.outbuffer, std.datetime, std.conv, std.concurrency, std.process, std.json;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.bind.libwebsocketd, veda.mstorage.wslink;
    import veda.core.common.context;
    import veda.core.common.know_predicates, veda.core.common.log_msg, veda.core.impl.thread_context;
    import veda.core.common.define, veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.bj8individual.individual8json;
    import veda.common.logger, veda.core.util.utils;
    import veda.mstorage.load_info, veda.mstorage.acl_manager, veda.mstorage.storage_manager, veda.mstorage.nanomsg_channel;
}

alias veda.mstorage.storage_manager ticket_storage_module;
alias veda.mstorage.storage_manager subject_storage_module;
alias veda.mstorage.acl_manager     acl_module;
alias veda.mstorage.load_info       load_info;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-mstorage", "log", "mstorage");
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
    io_msg = new Logger("pacahon", "io", "mstorage");
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

Context l_context;

void main(char[][] args)
{
    Tid[ P_MODULE ] tids;
    process_name = "mstorage";
    string node_id = null;

    tids[ P_MODULE.subject_manager ] = spawn(&individuals_manager, P_MODULE.subject_manager, individuals_db_path, node_id);
    if (wait_starting_thread(P_MODULE.subject_manager, tids) == false)
        return;

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

    spawn(&ws_interface, cast(short)8091);
    //spawn (&ws_interface, cast(short)8092);

    while (f_listen_exit == false)
        core.thread.Thread.sleep(dur!("seconds")(1000));

    writefln("send signals EXIT to threads");

    exit(P_MODULE.commiter);
    exit(P_MODULE.acl_preparer);
    exit(P_MODULE.subject_manager);
    exit(P_MODULE.ticket_manager);

    thread_term();
}

private void ws_interface(short ws_port)
{
    log.trace("start ws channel");
    VedaServer veda_server = new VedaServer("127.0.0.1", ws_port, log);
    veda_server.init(null);
    veda_server.listen(&ev_LWS_CALLBACK_GET_THREAD_ID, &ev_LWS_CALLBACK_CLIENT_WRITEABLE, &ev_LWS_CALLBACK_CLIENT_RECEIVE);
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
        res = execute(cast(string)msg, l_context);
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
        super(host, port, "/ws", "module-name=mstorage", log);
    }

    void init(string node_id)
    {
        if (node_id is null || node_id.length < 2)
            node_id = "cfg:standart_node";

        log.trace("init_core: node_id=[%s]", node_id);

        Backtrace.install(stderr);

        io_msg = new Logger("pacahon", "io", "mstorage");

        try
        {
            Individual node;

            Ticket     sticket;

            core_context = PThreadContext.create_new(node_id, "core_context-" ~ text(port), individuals_db_path, log);
            l_context    = core_context;

            sticket = core_context.sys_ticket();
            node    = core_context.get_configuration();
            if (node.getStatus() == ResultCode.OK)
                log.trace_log_and_console("VEDA NODE CONFIGURATION: [%s]", node);

            log.trace("init core");

            sticket = core_context.sys_ticket(true);
            string guest_ticket = core_context.get_ticket_from_storage("guest");

            if (guest_ticket is null)
                core_context.create_new_ticket("cfg:Guest", "900000000", "guest");

            /////////////////////////////////////////////////////////////////////////////////////////////////////////
            if (node.getStatus() != ResultCode.OK)
            {
                core_context.reopen_ro_subject_storage_db();
                core_context.reopen_ro_acl_storage_db();
                node = core_context.get_individual(&sticket, node_id);

                log.trace_log_and_console("VEDA NODE CONFIGURATION:[%s]", node);
            }

            return;
        } catch (Throwable ex)
        {
            writeln("Exception: ", ex.msg);
            return;
        }
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

        veda.mstorage.storage_manager.flush_int_module(P_MODULE.subject_manager, false);
        veda.mstorage.acl_manager.flush(false);
        veda.mstorage.storage_manager.flush_int_module(P_MODULE.ticket_manager, false);
    }
}

public string execute(string in_msg, Context ctx)
{
    JSONValue res;
    JSONValue jsn;

    try
    {
        jsn = parseJSON(in_msg);
    }
    catch (Throwable tr)
    {
        log.trace("ERR! fail parse msg=%s, err=%s", in_msg, tr.msg);
        res[ "type" ]   = "OpResult";
        res[ "result" ] = ResultCode.Internal_Server_Error;
        res[ "op_id" ]  = -1;

        return res.toString();
    }
    //log.trace("get msg=%s", jsn);
    try
    {
        JSONValue fn = jsn[ "function" ];

        string    sfn = fn.str();

        if (sfn == "authenticate")
        {
            JSONValue login    = jsn[ "login" ];
            JSONValue password = jsn[ "password" ];

            Ticket    ticket = ctx.authenticate(login.str, password.str);

            res[ "type" ]     = "ticket";
            res[ "id" ]       = ticket.id;
            res[ "user_uri" ] = ticket.user_uri;
            res[ "result" ]   = ticket.result;
            res[ "end_time" ] = ticket.end_time;

            //log.trace("authenticate: res=%s", res);
        }
        else if (sfn == "get_ticket_trusted")
        {
            JSONValue ticket_id = jsn[ "ticket" ];
            JSONValue login     = jsn[ "login" ];

            Ticket    ticket = ctx.get_ticket_trusted(ticket_id.str, login.str);

            res[ "type" ]     = "ticket";
            res[ "id" ]       = ticket.id;
            res[ "user_uri" ] = ticket.user_uri;
            res[ "result" ]   = ticket.result;
            res[ "end_time" ] = ticket.end_time;
        }
        else if (sfn == "put" || sfn == "remove" || sfn == "add_to" || sfn == "set_in" || sfn == "remove_from")
        {
            OpResult[] rc;

            JSONValue  _ticket         = jsn[ "ticket" ];
            JSONValue  jprepare_events = jsn[ "prepare_events" ];

            bool       prepare_events;
            if (jprepare_events.type() == JSON_TYPE.TRUE)
                prepare_events = true;

            JSONValue event_id       = jsn[ "event_id" ];
            long      transaction_id = 0;

            Ticket    *ticket = ctx.get_ticket(_ticket.str);

            if (sfn == "put")
            {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json)
                {
                    Individual individual = json_to_individual(individual_json);
                    OpResult   ires       =
                        ctx.put_individual(ticket, individual.uri, individual, prepare_events, event_id.str, transaction_id, false,
                                           true);
                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }
            else if (sfn == "add_to")
            {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json)
                {
                    Individual individual = json_to_individual(individual_json);
                    OpResult   ires       =
                        ctx.add_to_individual(ticket, individual.uri, individual, prepare_events, event_id.str, transaction_id, false,
                                              true);
                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }
            else if (sfn == "set_in")
            {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json)
                {
                    Individual individual = json_to_individual(individual_json);
                    OpResult   ires       =
                        ctx.set_in_individual(ticket, individual.uri, individual, prepare_events, event_id.str, transaction_id, false,
                                              true);
                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }
            else if (sfn == "remove_from")
            {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json)
                {
                    Individual individual = json_to_individual(individual_json);
                    OpResult   ires       =
                        ctx.remove_from_individual(ticket, individual.uri, individual, prepare_events, event_id.str, transaction_id, false,
                                                   true);
                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }
            else if (sfn == "remove")
            {
                JSONValue uri  = jsn[ "uri" ];
                OpResult  ires = ctx.remove_individual(ticket, uri.str, prepare_events, event_id.str, transaction_id, false, true);
                rc ~= ires;
                if (transaction_id <= 0)
                    transaction_id = ires.op_id;
            }

            JSONValue[] all_res;

            foreach (rr; rc)
            {
                JSONValue ires;
                ires[ "result" ] = rr.result;
                ires[ "op_id" ]  = rr.op_id;
                all_res ~= ires;
            }

            res[ "type" ] = "OpResult";
            res[ "data" ] = all_res;
        }
        else if (sfn == "flush")
        {
            P_MODULE   f_module_id = cast(P_MODULE)jsn[ "module_id" ].integer;
            long       wait_op_id  = jsn[ "wait_op_id" ].integer;

            ResultCode rc;

            if (f_module_id == P_MODULE.subject_manager)
                rc = subject_storage_module.flush_int_module(P_MODULE.subject_manager, false);
            else if (f_module_id == P_MODULE.acl_preparer)
                rc = acl_module.flush(false);
            else if (f_module_id == P_MODULE.fulltext_indexer)
                subject_storage_module.flush_ext_module(f_module_id, wait_op_id);

            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.OK;
            res[ "op_id" ]  = -1;
        }
        else if (sfn == "send_to_module")
        {
            P_MODULE   f_module_id = cast(P_MODULE)jsn[ "module_id" ].integer;
            string     msg         = jsn[ "msg" ].str;

            ResultCode rc;

            subject_storage_module.msg_to_module(f_module_id, msg, false);

            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.OK;
            res[ "op_id" ]  = -1;
        }
        else if (sfn == "backup")
        {
            bool to_binlog = jsn[ "to_binlog" ].type() == JSON_TYPE.TRUE;
            bool rc        = ctx.backup(to_binlog, 0);

            res[ "type" ] = "OpResult";
            if (rc == true)
                res[ "result" ] = ResultCode.OK;
            else
                res[ "result" ] = ResultCode.Internal_Server_Error;
            res[ "op_id" ] = -1;
        }
        else if (sfn == "freeze")
        {
            ctx.freeze();
            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.OK;
            res[ "op_id" ]  = -1;
        }
        else if (sfn == "unfreeze")
        {
            ctx.unfreeze();
            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.OK;
            res[ "op_id" ]  = -1;
        }
        else
        {
            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.Bad_Request;
            res[ "op_id" ]  = -1;
        }

        return res.toString();
    }
    catch (Throwable tr)
    {
        log.trace("ERR! fail execute msg=%s, err=%s", in_msg, tr.msg);
        res[ "type" ]   = "OpResult";
        res[ "result" ] = ResultCode.Internal_Server_Error;
        res[ "op_id" ]  = -1;

        return res.toString();
    }
}

