/**
 * master storage
 */
module veda.mstorage.server;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
    import core.thread, std.stdio, std.string, core.stdc.string, std.outbuffer, std.datetime, std.conv, std.concurrency, std.process, std.json,
           std.regex, std.uuid, std.random;
    import veda.util.properd, veda.core.impl.app_context_creator;
    import veda.core.common.context, veda.core.impl.thread_context, veda.search.xapian.xapian_search;
    import veda.core.common.define, veda.core.common.type, veda.common.type, veda.onto.individual, veda.onto.resource,
           veda.onto.bj8individual.individual8json;
    import veda.common.logger, veda.core.util.utils, veda.core.common.transaction;
    import veda.mstorage.storage_manager, veda.mstorage.nanomsg_channel, veda.storage.storage;
    import veda.storage.common, veda.authorization.authorization, veda.authorization.az_lib;
    import veda.onto.individual;
}

alias veda.mstorage.storage_manager indv_storage_thread;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log(){
    if (_log is null)
        _log = new Logger("veda-core-mstorage", "log", "mstorage");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

bool   f_listen_exit = false;
Logger io_msg;

static this()
{
    io_msg = new Logger("pacahon", "io", "mstorage");
    bsd_signal(SIGINT, &handleTermination2);
}

extern (C) void handleTermination2(int _signal){
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));

    if (_log !is null)
        _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    //_log.close();

    writeln("!SYS: ", process_name, ": preparation for the exit.");

    f_listen_exit = true;

    //thread_term();
    Runtime.terminate();
}

private Context l_context;

void main(char[][] args){
    Tid[ P_MODULE ] tids;
    process_name = "mstorage";
    string node_id = null;

    tids[ P_MODULE.subject_manager ] = spawn(&individuals_manager, P_MODULE.subject_manager, node_id);
    if (wait_starting_thread(P_MODULE.subject_manager, tids) == false)
        return;

    tids[ P_MODULE.commiter ] =
        spawn(&commiter, text(P_MODULE.commiter));
    wait_starting_thread(P_MODULE.commiter, tids);

    tids[ P_MODULE.n_channel ] = spawn(&nanomsg_channel, text(P_MODULE.n_channel));
    wait_starting_thread(P_MODULE.n_channel, tids);

    foreach (key, value; tids)
        register(text(key), value);

    init(null);

    while (f_listen_exit == false)
        core.thread.Thread.sleep(dur!("seconds")(1000));

    writefln("send signals EXIT to threads");

    exit(P_MODULE.commiter);
    exit(P_MODULE.subject_manager);

    //thread_term();
}

public Authorization get_acl_client(Logger log){
    return new AuthorizationUseLib(log);
}

void init(string node_id){
    Context core_context;
    Ticket  sticket;

    if (node_id is null || node_id.length < 2)
        node_id = "cfg:standart_node";

    log.trace("init_core: node_id=[%s]", node_id);

    io_msg = new Logger("pacahon", "io", "mstorage");

    try
    {
        Individual node;

        core_context = create_new_ctx("core_context-mstorage", log);
        core_context.set_az(get_acl_client(log));
        core_context.set_vql(new XapianSearch(core_context));

        l_context = core_context;

        node = core_context.get_configuration();
        if (node.getStatus() == ResultCode.Ok)
            log.trace_log_and_console("VEDA NODE CONFIGURATION: [%s]", node);

        log.trace("init core");

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        if (node.getStatus() != ResultCode.Ok) {
            core_context.reopen_ro_individuals_storage_db();
            core_context.get_az().reopen();
            node = core_context.get_individual(node_id);

            log.trace_log_and_console("VEDA NODE CONFIGURATION:[%s]", node);
        }

        while (sticket.result == ResultCode.TicketNotFound || sticket.result == ResultCode.zero) {
            sticket = *core_context.get_systicket_from_storage();

            if (sticket.result == ResultCode.TicketNotFound || sticket.result == ResultCode.zero) {
                log.trace("system ticket not found, sleep and repeate...");
                core.thread.Thread.sleep(dur!("msecs")(100));
            }
        }

        log.trace("system ticket=%s", sticket);
        set_global_systicket(sticket);

        return;
    } catch (Throwable ex)
    {
        writeln("Exception: ", ex.msg);
        return;
    }
}

bool wait_starting_thread(P_MODULE tid_idx, ref Tid[ P_MODULE ] tids){
    bool res;
    Tid  tid = tids[ tid_idx ];

    if (tid == Tid.init)
        throw new Exception("wait_starting_thread: Tid=" ~ text(tid_idx) ~ " not found", __FILE__, __LINE__);

    log.trace("START THREAD... : %s", text(tid_idx));
    send(tid, thisTid);
    receive((bool isReady)
            {
                res = isReady;
                log.trace("START THREAD IS SUCCESS: %s", text(tid_idx));
                if (res == false)
                    log.trace("FAIL START THREAD: %s", text(tid_idx));
            });
    return res;
}

public void exit(P_MODULE module_id){
    Tid tid_module = getTid(module_id);

    if (tid_module != Tid.init) {
        writefln("send command EXIT to thread_%s", text(module_id));
        send(tid_module, CMD_EXIT, thisTid);
        receive((bool _res) {});
    }
}

void commiter(string thread_name){
    core.thread.Thread.getThis().name = thread_name;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    bool is_exit = false;

    while (is_exit == false) {
        receiveTimeout(dur!("msecs")(600),
                       (byte cmd, Tid tid_response_reciever)
                       {
                           if (cmd == CMD_EXIT) {
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
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

private KeyValueDB inividuals_storage_r;

private Individual get_individual(Context ctx, Ticket *ticket, string uri){
    if (inividuals_storage_r is null)
        inividuals_storage_r = ctx.get_storage().get_inividuals_storage_r();

    Individual individual = Individual.init;

    if (inividuals_storage_r is null) {
        log.trace("ERR! storage not ready");
        return individual;
    }

    if (ticket is null) {
        log.trace("get_individual, uri=%s, ticket is null", uri);
        return individual;
    }

    inividuals_storage_r.get_individual(uri, individual);

    return individual;
}

public string execute_json(string in_msg, Context ctx){
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
        res[ "result" ] = ResultCode.InternalServerError;
        res[ "op_id" ]  = -1;

        return res.toString();
    }
    //log.trace("get msg=%s", jsn);
    try
    {
        JSONValue fn = jsn[ "function" ];

        string    sfn = fn.str();

        if (sfn == "put" || sfn == "remove" || sfn == "add_to" || sfn == "set_in" || sfn == "remove_from") {
            OpResult[] rc;

            JSONValue  _ticket           = jsn[ "ticket" ];
            JSONValue  jassigned_modules = jsn[ "assigned_subsystems" ];

            long       assigned_subsystems = jassigned_modules.integer();

            JSONValue  event_id = jsn[ "event_id" ];

            string     src = null;
            if (auto p = "src" in jsn)
                src = jsn[ "src" ].str;

            long   transaction_id = 0;

            Ticket *ticket = ctx.get_ticket(_ticket.str, false);

            if (sfn == "put") {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json) {
                    Individual  individual = json_to_individual(individual_json);

                    Transaction tnx;
                    tnx.id            = transaction_id;
                    tnx.src           = src;
                    OpResult ires = add_to_transaction(
                                                       ctx.get_az(), tnx, ticket, INDV_OP.PUT, &individual, assigned_subsystems,
                                                       event_id.str,
                                                       OptAuthorize.YES,
                                                       OptTrace.NONE);

                    //commit (OptAuthorize.YES, tnx);

                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }else if (sfn == "add_to") {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json) {
                    Individual  individual = json_to_individual(individual_json);

                    Transaction tnx;
                    tnx.id            = transaction_id;
                    tnx.src           = src;
                    OpResult ires = add_to_transaction(
                                                       ctx.get_az(), tnx, ticket, INDV_OP.ADD_IN, &individual,
                                                       assigned_subsystems, event_id.str,
                                                       OptAuthorize.YES,
                                                       OptTrace.NONE);

                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }else if (sfn == "set_in") {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json) {
                    Individual  individual = json_to_individual(individual_json);

                    Transaction tnx;
                    tnx.id            = transaction_id;
                    tnx.src           = src;
                    OpResult ires = add_to_transaction(
                                                       ctx.get_az(), tnx, ticket, INDV_OP.SET_IN, &individual,
                                                       assigned_subsystems, event_id.str,
                                                       OptAuthorize.YES,
                                                       OptTrace.NONE);

                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }else if (sfn == "remove_from") {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json) {
                    Individual  individual = json_to_individual(individual_json);

                    Transaction tnx;
                    tnx.id            = transaction_id;
                    tnx.src           = src;
                    OpResult ires = add_to_transaction(
                                                       ctx.get_az(), tnx, ticket, INDV_OP.REMOVE_FROM, &individual,
                                                       assigned_subsystems,
                                                       event_id.str,
                                                       OptAuthorize.YES,
                                                       OptTrace.NONE);

                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }else if (sfn == "remove") {
                JSONValue[] individuals_json = jsn[ "individuals" ].array;

                foreach (individual_json; individuals_json) {
                    Individual  individual = json_to_individual(individual_json);

                    Transaction tnx;
                    tnx.id            = transaction_id;
                    tnx.src           = src;
                    OpResult ires = add_to_transaction(
                                                       ctx.get_az(), tnx, ticket, INDV_OP.REMOVE, &individual,
                                                       assigned_subsystems, event_id.str,
                                                       OptAuthorize.YES,
                                                       OptTrace.NONE);

                    rc ~= ires;
                    if (transaction_id <= 0)
                        transaction_id = ires.op_id;
                }
            }

            JSONValue[] all_res;
            foreach (rr; rc) {
                JSONValue ires;
                ires[ "result" ] = rr.result;
                ires[ "op_id" ]  = rr.op_id;
                all_res ~= ires;
            }

            res[ "type" ] = "OpResult";
            res[ "data" ] = all_res;
        }else if (sfn == "flush") {
            P_MODULE   f_module_id = cast(P_MODULE)jsn[ "module_id" ].integer;
            long       wait_op_id  = jsn[ "wait_op_id" ].integer;

            ResultCode rc;

            if (f_module_id == P_MODULE.subject_manager)
                rc = flush_storage();
            else if (f_module_id == cast(P_MODULE)MODULE.fulltext_indexer)
                flush_ext_module(f_module_id, wait_op_id);

            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.Ok;
            res[ "op_id" ]  = -1;
        }else if (sfn == "send_to_module") {
            P_MODULE   f_module_id = cast(P_MODULE)jsn[ "module_id" ].integer;
            string     msg         = jsn[ "msg" ].str;

            ResultCode rc;

            msg_to_module(f_module_id, msg, false);

            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.Ok;
            res[ "op_id" ]  = -1;
        }else  {
            res[ "type" ]   = "OpResult";
            res[ "result" ] = ResultCode.BadRequest;
            res[ "op_id" ]  = -1;
        }

        return res.toString();
    }
    catch (Throwable tr)
    {
        log.trace("ERR! fail execute msg=%s, err=%s", in_msg, tr.msg);
        res[ "type" ]   = "OpResult";
        res[ "result" ] = ResultCode.InternalServerError;
        res[ "op_id" ]  = -1;

        return res.toString();
    }
}

public string[]   owl_tags = [ "rdf:Property", "owl:Restriction", "owl:ObjectProperty", "owl:DatatypeProperty", "owl:Class", "rdfs:Class" ];

static const byte NEW_TYPE    = 0;
static const byte EXISTS_TYPE = 1;

private OpResult add_to_transaction(Authorization acl_client, ref Transaction tnx, Ticket *ticket, INDV_OP cmd, Individual *indv,
                                    long assigned_subsystems,
                                    string event_id,
                                    OptAuthorize opt_request,
                                    OptTrace opt_trace){
    if (ticket !is null && get_global_systicket().user_uri == ticket.user_uri) {
        //log.trace("WARN! add_to_transaction: [%s %s] from sysuser, skip authorization", text(cmd), indv.uri);
        opt_request = OptAuthorize.NO;
    }

    //log.trace("@add_to_transaction: target=%s op=%s indv=%s", subsystem_byte_to_string(cast(ubyte)assigned_subsystems), text(cmd), *indv);

    OpResult res = OpResult(ResultCode.FailStore, -1);

    if (ticket is null) {
        log.trace("ERR! add_to_transaction: %s %s, ticket is null", text(cmd), *indv);
        res = OpResult(ResultCode.AuthenticationFailed, -1);
        return res;
    }

    try
    {
        if (indv !is null && (indv.uri is null || indv.uri.length < 2)) {
            res.result = ResultCode.InvalidIdentifier;
            return res;
        }
        if (indv is null || (cmd != INDV_OP.REMOVE && indv.resources.length == 0)) {
            res.result = ResultCode.NoContent;
            return res;
        }

        Tid         tid_subject_manager;

        MapResource rdfType;
        Resources   _types = set_map_of_type(indv, rdfType);

        string      prev_state;
        Individual  prev_indv;

        
            try
            {
                prev_state = indv_storage_thread.find(P_MODULE.subject_manager, indv.uri);

                if ((prev_state is null ||
                     prev_state.length == 0) && (cmd == INDV_OP.ADD_IN || cmd == INDV_OP.SET_IN || cmd == INDV_OP.REMOVE_FROM))
                    log.trace("ERR! add_to_transaction, cmd=%s: not read prev_state uri=[%s]", text(cmd), indv.uri);
            }
            catch (Exception ex)
            {
                res.result = ResultCode.UnprocessableEntity;
                log.trace("ERR! add_to_transaction: not read prev_state uri=[%s], ex=%s", indv.uri, ex.msg);
                return res;
            }

            if (prev_state !is null) {
                int code = prev_indv.deserialize(prev_state);
                if (code < 0) {
                    log.trace("ERR! add_to_transaction: invalid prev_state [%s], uri=%s", prev_state, indv.uri);
                    res.result = ResultCode.UnprocessableEntity;
                    return res;
                }

                if (cmd == INDV_OP.REMOVE) {
                    indv.deserialize(prev_state);
                    _types = set_map_of_type(indv, rdfType);
                }

                if (opt_request == OptAuthorize.YES && cmd != INDV_OP.REMOVE) {
                    if (indv.isExists("v-s:deleted", true)) {
                        if (acl_client.authorize(indv.uri, ticket.user_uri, Access.can_delete, true, null, null, null) != Access.can_delete) {
                            // для устаноки аттрибута v-s:deleted у индивида проверим доступность бита Delete
                            log.trace("ERR! add_to_transaction: Not Authorized, user [%s] request [can delete] [%s] ", ticket.user_uri, indv.uri);
                            res.result = ResultCode.NotAuthorized;
                            return res;
                        }
                    }else if (acl_client.authorize(indv.uri, ticket.user_uri, Access.can_update, true, null, null, null) != Access.can_update) {
                        // для обновляемого индивида проверим доступность бита Update
                        log.trace("ERR! add_to_transaction: Not Authorized, user [%s] request [can update] [%s] ", ticket.user_uri, indv.uri);
                        res.result = ResultCode.NotAuthorized;
                        return res;
                    }

                    // найдем какие из типов были добавлены по сравнению с предыдущим набором типов
                    foreach (rs; _types) {
                        string   itype = rs.get!string;

                        Resource rr = rdfType.get(itype, Resource.init);

                        if (rr !is Resource.init) {
                            rr.info          = EXISTS_TYPE;
                            rdfType[ itype ] = rr;
                        }
                    }
                }
            }
        

        if (opt_request == OptAuthorize.YES && cmd != INDV_OP.REMOVE) {
            // для новых типов проверим доступность бита Create
            foreach (key, rr; rdfType) {
                if (rr.info == NEW_TYPE) {
                    if (acl_client.authorize(key, ticket.user_uri, Access.can_create, true, null, null, null) != Access.can_create) {
                        log.trace("ERR! add_to_transaction: Not Authorized, user [%s] request [can_create] [%s] ", ticket.user_uri, key);
                        res.result = ResultCode.NotAuthorized;
                        return res;
                    }
                }
            }
        }

        long   update_counter = prev_indv.getFirstInteger("v-s:updateCounter", 0) + 1;
        string new_state;

        if (cmd == INDV_OP.REMOVE) {
            if (prev_state !is null) {
                prev_indv.setResources("v-s:deleted", [ Resource(true) ]);

                new_state = prev_indv.serialize();
                if (new_state.length > max_size_of_individual) {
                    res.result = ResultCode.SizeTooLarge;
                    return res;
                }

                immutable TransactionItem ti =
                    immutable TransactionItem(INDV_OP.PUT, ticket.user_uri, indv.uri, prev_state, new_state, update_counter,
                                              event_id, assigned_subsystems);


                    res.result = indv_storage_thread.save(tnx.src, P_MODULE.subject_manager, [ ti ], tnx.id, res.op_id);
            }else
                res.result = ResultCode.Ok;


            immutable TransactionItem ti1 =
                immutable TransactionItem(INDV_OP.REMOVE, ticket.user_uri, indv.uri, prev_state, null, update_counter,
                                          event_id, assigned_subsystems);

                if (res.result == ResultCode.Ok) {
                    res.result = indv_storage_thread.save(tnx.src, P_MODULE.subject_manager, [ ti1 ], tnx.id, res.op_id);
                }
        }else  {
            if (cmd == INDV_OP.ADD_IN || cmd == INDV_OP.SET_IN || cmd == INDV_OP.REMOVE_FROM) {
                //log.trace("++ add_to_transaction (%s), prev_indv: %s, op_indv: %s", text (cmd), prev_indv, *indv);
                indv = indv_apply_cmd(cmd, &prev_indv, indv);
                //log.trace("++ add_to_transaction (%s), final indv: %s", text (cmd), *indv);
            }

            indv.setResources("v-s:updateCounter", [ Resource(update_counter) ]);

            new_state = indv.serialize();
            if (new_state.length > max_size_of_individual) {
                res.result = ResultCode.SizeTooLarge;
                return res;
            }

            immutable TransactionItem ti =
                immutable TransactionItem(INDV_OP.PUT, ticket.user_uri, indv.uri, prev_state, new_state, update_counter,
                                          event_id, assigned_subsystems);

                res.result = indv_storage_thread.save(tnx.src, P_MODULE.subject_manager, [ ti ], tnx.id, res.op_id);
        }

        return res;
    }
    finally
    {
        if (res.result != ResultCode.Ok)
            log.trace("ERR! add_to_transaction (%s): no store individual: errcode=[%s], ticket=[%s], indv=[%s]", text(cmd), text(res.result),
                      ticket !is null ? text(*ticket) : "null",
                      indv !is null ? text(*indv) : "null");

        if (opt_trace == OptTrace.TRACE)
            log.trace("add_to_transaction [%s] = %s", indv.uri, res);
    }
}

private Resources set_map_of_type(Individual *indv, ref MapResource rdfType){
    Resources _types;

    if (indv is null)
        return _types;

    _types = indv.resources.get("rdf:type", Resources.init);

    foreach (idx, rs; _types)
        _types[ idx ].info = NEW_TYPE;
    setMapResources(_types, rdfType);

    return _types;
}

public ResultCode flush_storage(){
    ResultCode rc;

    rc = ResultCode.Ok;
    return rc;
}

private void flush_ext_module(P_MODULE f_module, long wait_op_id){
    Tid tid = getTid(P_MODULE.subject_manager);

    if (tid != Tid.init) {
        send(tid, CMD_COMMIT, f_module, wait_op_id);
    }
}

private ResultCode msg_to_module(P_MODULE f_module, string msg, bool is_wait){
    ResultCode rc;

    Tid        tid = getTid(P_MODULE.subject_manager);

    if (tid != Tid.init) {
        if (is_wait == false) {
            send(tid, CMD_MSG, f_module, msg);
        }else  {
            send(tid, CMD_MSG, msg, f_module, thisTid);
            receive((bool isReady) {});
        }
        rc = ResultCode.Ok;
    }
    return rc;
}
