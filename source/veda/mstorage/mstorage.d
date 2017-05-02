/**
 * master storage
 */
module veda.mstorage.server;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
    import core.thread, std.stdio, std.string, core.stdc.string, std.outbuffer, std.datetime, std.conv, std.concurrency, std.process, std.json,
           std.regex, std.uuid;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.bind.libwebsocketd, veda.mstorage.wslink;
    import veda.core.common.context;
    import veda.core.common.know_predicates, veda.core.common.log_msg, veda.core.impl.thread_context, veda.core.search.vql;
    import veda.core.common.define, veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.bj8individual.individual8json;
    import veda.common.logger, veda.core.util.utils, veda.core.common.transaction;
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
Storage inividuals_storage_r;
VQL     vql_r;
Ticket  sticket;

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
    init(null);
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
    ushort port;
    string host;

    this(string _host, ushort _port, Logger log)
    {
        host = _host;
        port = _port;
        super(host, port, "/ws", "module-name=mstorage", log);
    }
}

void init(string node_id)
{
    Context core_context;

    if (node_id is null || node_id.length < 2)
        node_id = "cfg:standart_node";

    log.trace("init_core: node_id=[%s]", node_id);

    Backtrace.install(stderr);

    io_msg = new Logger("pacahon", "io", "mstorage");

    try
    {
        Individual node;

        core_context         = PThreadContext.create_new(node_id, "core_context-mstorage", individuals_db_path, log);
        l_context            = core_context;
        inividuals_storage_r = l_context.get_inividuals_storage_r();
        vql_r                = l_context.get_vql();

        sticket = sys_ticket(core_context);
        node    = core_context.get_configuration();
        if (node.getStatus() == ResultCode.OK)
            log.trace_log_and_console("VEDA NODE CONFIGURATION: [%s]", node);

        log.trace("init core");

        sticket = sys_ticket(core_context, true);
        string guest_ticket = core_context.get_ticket_from_storage("guest");

        if (guest_ticket is null)
            create_new_ticket("cfg:Guest", "900000000", "guest");

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        if (node.getStatus() != ResultCode.OK)
        {
            core_context.reopen_ro_individuals_storage_db();
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

////////////////////////////////////////////////////////////////////////////////////////////////////

public Individual get_individual(Ticket *ticket, string uri)
{
    Individual individual = Individual.init;

    if (ticket is null)
    {
        log.trace("get_individual, uri=%s, ticket is null", uri);
        return individual;
    }

    string individual_as_binobj = inividuals_storage_r.find(true, ticket.user_uri, uri);
    if (individual_as_binobj !is null && individual_as_binobj.length > 1)
    {
//                if (acl_indexes.authorize(uri, ticket, Access.can_read, true, null, null) == Access.can_read)
        {
            if (individual.deserialize(individual_as_binobj) > 0)
                individual.setStatus(ResultCode.OK);
            else
            {
                individual.setStatus(ResultCode.Unprocessable_Entity);
                writeln("ERR!: invalid binobj: [", individual_as_binobj, "] ", uri);
            }
        }
//                else
//                {
//                    if (trace_msg[ T_API_160 ] == 1)
//                        log.trace("get_individual, not authorized, uri=%s, user_uri=%s", uri, ticket.user_uri);
//                    individual.setStatus(ResultCode.Not_Authorized);
//                }
    }
    else
    {
        individual.setStatus(ResultCode.Unprocessable_Entity);
        //writeln ("ERR!: empty binobj: [", individual_as_binobj, "] ", uri);
    }

    return individual;
}

public Ticket create_new_ticket(string user_id, string duration = "40000", string ticket_id = null)
{
    if (trace_msg[ T_API_50 ] == 1)
        log.trace("create_new_ticket, ticket__accessor=%s", user_id);

    Ticket     ticket;
    Individual new_ticket;

    ticket.result = ResultCode.Fail_Store;

    Resources type = [ Resource(ticket__Ticket) ];

    new_ticket.resources[ rdf__type ] = type;

    if (ticket_id !is null && ticket_id.length > 0)
        new_ticket.uri = ticket_id;
    else
    {
        UUID new_id = randomUUID();
        new_ticket.uri = new_id.toString();
    }

    new_ticket.resources[ ticket__accessor ] ~= Resource(user_id);
    new_ticket.resources[ ticket__when ] ~= Resource(getNowAsString());
    new_ticket.resources[ ticket__duration ] ~= Resource(duration);

    version (isMStorage)
    {
        // store ticket
        string     ss_as_binobj = new_ticket.serialize();

        long       op_id;
        ResultCode rc =
            ticket_storage_module.update(P_MODULE.ticket_manager, false, INDV_OP.PUT, null, new_ticket.uri, null, ss_as_binobj, -1, null, -1,
                                         false,
                                         op_id);
        ticket.result = rc;

        if (rc == ResultCode.OK)
        {
            subject2Ticket(new_ticket, &ticket);
            user_of_ticket[ ticket.id ] = new Ticket(ticket);
        }

        log.trace("create new ticket %s, user=%s, start=%s, end=%s", ticket.id, ticket.user_uri, SysTime(ticket.start_time, UTC()).toISOExtString(
                                                                                                                                                  ),
                  SysTime(ticket.end_time, UTC()).toISOExtString());
    }

    return ticket;
}

public Ticket authenticate(string login, string password)
{
    StopWatch sw; sw.start;

    Ticket    ticket;

    if (trace_msg[ T_API_70 ] == 1)
        log.trace("authenticate, login=[%s] password=[%s]", login, password);

    try
    {
        ticket.result = ResultCode.Authentication_Failed;

        if (login == null || login.length < 1 || password == null || password.length < 6)
            return ticket;

        login = replaceAll(login, regex(r"[-]", "g"), " +");

        //Ticket       sticket         = sys_ticket;
        Individual[] candidate_users;
        vql_r.get(&sticket, "'" ~ veda_schema__login ~ "' == '" ~ login ~ "'", null, null, 10, 10000, candidate_users, false, false);
        foreach (user; candidate_users)
        {
            string user_id = user.getFirstResource(veda_schema__owner).uri;
            if (user_id is null)
                continue;

            string pass;
            string usesCredential_uri = user.getFirstLiteral("v-s:usesCredential");
            if (usesCredential_uri !is null)
            {
                log.trace("authenticate:found v-s:usesCredential, uri=%s", usesCredential_uri);
                Individual i_usesCredential = get_individual(&sticket, usesCredential_uri);
                pass = i_usesCredential.getFirstLiteral("v-s:password");
            }
            else
            {
                pass = user.getFirstLiteral("v-s:password");

                Individual i_usesCredential;
                i_usesCredential.uri = user.uri ~ "-crdt";
                i_usesCredential.addResource("rdf:type", Resource(DataType.Uri, "v-s:Credential"));
                i_usesCredential.addResource("v-s:password", Resource(DataType.String, pass));
                OpResult op_res = l_context.put_individual(&sticket, i_usesCredential.uri, i_usesCredential, false, "", -1, false, true);
                log.trace("authenticate: create v-s:Credential[%s], res=%s", i_usesCredential, op_res);
                user.addResource("v-s:usesCredential", Resource(DataType.Uri, i_usesCredential.uri));
                user.removeResource("v-s:password");
                op_res = l_context.put_individual(&sticket, user.uri, user, false, "", -1, false, true);
                log.trace("authenticate: update user[%s], res=%s", user, op_res);
            }

            if (pass !is null && pass == password)
            {
                ticket = create_new_ticket(user_id);
                return ticket;
            }
        }

        log.trace("authenticate:fail authenticate, login=[%s] password=[%s]", login, password);

        ticket.result = ResultCode.Authentication_Failed;

        return ticket;
    }
    finally
    {
        stat(CMD_PUT, sw);
    }
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

            Ticket    ticket = authenticate(login.str, password.str);

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
            bool rc        = backup(ctx, to_binlog, 0);

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

public void freeze()
{
    subject_storage_module.freeze(P_MODULE.subject_manager);
}

public void unfreeze()
{
    subject_storage_module.unfreeze(P_MODULE.subject_manager);
}

private Ticket *[ string ] user_of_ticket;

public Ticket sys_ticket(Context ctx, bool is_new = false)
{
    Ticket ticket = get_global_systicket();

    if (ticket == Ticket.init || ticket.user_uri == "" || is_new)
    {
        try
        {
            ticket = create_new_ticket("cfg:VedaSystem", "90000000");

            long op_id;
            ticket_storage_module.update(P_MODULE.ticket_manager, false, INDV_OP.PUT, null, "systicket", null, ticket.id, -1, null, -1, false,
                                         op_id);
            log.trace("systicket [%s] was created", ticket.id);

            Individual sys_account_permission;
            sys_account_permission.uri = "p:" ~ ticket.id;
            sys_account_permission.addResource("rdf:type", Resource(DataType.Uri, "v-s:PermissionStatement"));
            sys_account_permission.addResource("v-s:canCreate", Resource(DataType.Boolean, "true"));
            sys_account_permission.addResource("v-s:permissionObject", Resource(DataType.Uri, "v-s:AllResourcesGroup"));
            sys_account_permission.addResource("v-s:permissionSubject", Resource(DataType.Uri, "cfg:VedaSystem"));
            OpResult opres = ctx.put_individual(&ticket, sys_account_permission.uri, sys_account_permission, false, "srv", -1, false,
                                                false);

            if (opres.result == ResultCode.OK)
                log.trace("permission [%s] was created", sys_account_permission);
        }
        catch (Exception ex)
        {
            //printPrettyTrace(stderr);
            log.trace("context.sys_ticket:EX!%s", ex.msg);
        }

        if (ticket.user_uri == "")
            ticket.user_uri = "cfg:VedaSystem";

        set_global_systicket(ticket);
    }

    return ticket;
}

public bool backup(Context ctx, bool to_binlog, int level = 0)
{
    bool result = false;

    if (level == 0)
        freeze();

    Ticket sticket = sys_ticket(ctx);

    try
    {
        string backup_id = "to_binlog";

        if (to_binlog)
        {
            long count = ctx.get_inividuals_storage_r.dump_to_binlog();
            if (count > 0)
                result = true;
        }
        else
        {
            backup_id = subject_storage_module.backup(P_MODULE.subject_manager);

            if (backup_id != "")
            {
                result = true;

                string res;         // = veda.core.threads.acl_manager.backup(backup_id);

                if (res == "")
                    result = false;
                else
                {
                    Tid tid_ticket_manager = getTid(P_MODULE.ticket_manager);
                    send(tid_ticket_manager, CMD_BACKUP, backup_id, thisTid);
                    receive((string _res) { res = _res; });
                    if (res == "")
                        result = false;
                    else
                    {
                        //res = veda.core.threads.xapian_indexer.backup(backup_id);

                        if (res == "")
                            result = false;
                    }
                }
            }

            if (result == false)
            {
                if (level < 10)
                {
                    log.trace_log_and_console("BACKUP FAIL, repeat(%d) %s", level, backup_id);

                    core.thread.Thread.sleep(dur!("msecs")(500));
                    return backup(ctx, to_binlog, level + 1);
                }
                else
                    log.trace_log_and_console("BACKUP FAIL, %s", backup_id);
            }
        }

        if (result == true)
            log.trace_log_and_console("BACKUP Ok, %s", backup_id);
    }
    finally
    {
        if (level == 0)
            unfreeze();
    }

    return result;
}

public ResultCode commit(Transaction *in_tnx, Context ctx)
{
    ResultCode  rc;
    long        op_id;

    Transaction normalized_tnx;

    normalized_tnx.id = in_tnx.id;
    foreach (item; in_tnx.get_queue())
    {
        if (item.cmd != INDV_OP.REMOVE && item.new_indv == Individual.init)
            continue;

        if (item.rc != ResultCode.OK)
            return item.rc;

        Ticket *ticket = ctx.get_ticket(item.ticket_id);

        //log.trace ("transaction: cmd=%s, indv=%s ", item.cmd, item.indv);

        rc = ctx.add_to_transaction(normalized_tnx, ticket, item.cmd, &item.new_indv, true, item.event_id, false, true).result;

        if (rc == ResultCode.No_Content)
        {
            ctx.get_logger().trace("WARN!: Rejected attempt to save an empty object: %s", item.new_indv);
        }

        if (rc != ResultCode.OK && rc != ResultCode.No_Content)
        {
            ctx.get_logger().trace("FAIL COMMIT");
            return rc;
        }
        //else
        //log.trace ("SUCCESS COMMIT");
    }

    if (in_tnx.is_autocommit == false)
    {
        rc = subject_storage_module.update(P_MODULE.subject_manager, true, in_tnx.get_immutable_queue(), in_tnx.id, true, op_id);
    }
    return ResultCode.OK;
}



