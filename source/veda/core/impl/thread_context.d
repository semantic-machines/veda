/**
 * Внешнее API - Реализация
 */

module veda.core.impl.thread_context;

private
{
    import core.thread, std.stdio, std.format, std.datetime, std.concurrency, std.conv, std.outbuffer, std.string, std.uuid, std.file, std.path,
           std.json, std.regex;
    import veda.bind.xapian_d_header;
    import veda.util.container, veda.common.logger, veda.core.util.utils, veda.onto.bj8individual.individual8json, veda.core.common.log_msg,
           veda.util.module_info;
    import veda.common.type, veda.core.common.know_predicates, veda.core.common.define, veda.core.common.context;
    import veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.storage.lmdb.lmdb_driver, veda.storage.common, veda.storage.storage;
    import veda.core.search.vql, veda.core.common.transaction, veda.util.module_info, veda.common.logger, veda.storage.lmdb.lmdb_storage;

    version (isMStorage)
    {
        alias veda.storage.storage_manager ticket_storage_module;
        alias veda.storage.storage_manager subject_storage_module;
    }
}

/// реализация интерфейса Context
class PThreadContext : Context
{
    private Onto        onto;

    private string      name;

    private             string[ string ] prefix_map;

    private VQL         _vql;

    private LmdbStorage storage;

    private long        local_last_update_time;
    private Individual  node = Individual.init;
    private string      node_id;

    private bool        API_ready = true;
    private string      main_module_url;
    private Logger      log;

    Storage get_storage()
    {
        return storage;
    }

    public Logger get_logger()
    {
        return log;
    }

    version (isModule)
    {
        import kaleidic.nanomsg.nano;

        private int sock_main_module = -1;

        private int get_sock_2_main_module()
        {
            if (sock_main_module >= 0)
                return sock_main_module;

            sock_main_module = nn_socket(AF_SP, NN_REQ);
            if (sock_main_module < 0)
            {
                log.trace("ERR! cannot create socket");
                return -1;
            }
            else if (nn_connect(sock_main_module, cast(char *)main_module_url) < 0)
            {
                log.trace("ERR! cannot connect socket to %s", main_module_url);
                return -1;
            }
            else
            {
                log.trace("success connect %s", main_module_url);
                return sock_main_module;
            }
        }

        private OpResult[] reqrep_binobj_2_main_module(string req)
        {
            OpResult[] ress;

            int        sock = get_sock_2_main_module();

            if (sock >= 0)
            {
                char *buf = cast(char *)0;
                int  bytes;

                bytes = nn_send(sock, cast(char *)req, req.length + 1, 0);
                //log.trace("N_CHANNEL BINOBJ send [%d](%s)", req.length, req);
                bytes = nn_recv(sock, &buf, NN_MSG, 0);
                if (bytes > 0)
                {
                    string rep = to!string(buf);
                    //log.trace("N_CHANNEL BINOBJ recv (%s)", rep);

                    JSONValue jres = parseJSON(rep);

                    if (jres[ "type" ].str == "OpResult")
                    {
                        JSONValue data = jres[ "data" ];
                        if (data !is JSONValue.init)
                        {
                            foreach (ii; data.array)
                            {
                                OpResult res;


                                res.op_id  = ii[ "op_id" ].integer;
                                res.result = cast(ResultCode)ii[ "result" ].integer;
                                ress ~= res;
                            }
                        }
                        else
                        {
                            OpResult res;
                            res.op_id  = jres[ "op_id" ].integer;
                            res.result = cast(ResultCode)jres[ "result" ].integer;
                            ress ~= res;
                        }
                    }

                    nn_freemsg(buf);
                }
            }
            else
            {
                log.trace("ERR! N_CHANNEL: invalid socket");
            }

            return ress;
        }


        private OpResult[] reqrep_json_2_main_module(ref JSONValue jreq)
        {
            string     req = jreq.toString();
            string     rep;

            OpResult[] ress;

            try
            {
                int sock = get_sock_2_main_module();

                if (sock >= 0)
                {
                    char *buf = cast(char *)0;
                    int  bytes;

                    bytes = nn_send(sock, cast(char *)req, req.length, 0);
                    //log.trace("N_CHANNEL send (%s)", req);
                    bytes = nn_recv(sock, &buf, NN_MSG, 0);
                    if (bytes > 0)
                    {
                        rep = to!string(buf);
                        //log.trace("N_CHANNEL recv (%s)", rep);

                        JSONValue jres = parseJSON(rep);

                        if (jres[ "type" ].str == "OpResult")
                        {
                            if ("data" in jres)
                            {
                                JSONValue data = jres[ "data" ];
                                if (data !is JSONValue.init)
                                {
                                    foreach (ii; data.array)
                                    {
                                        OpResult res;

                                        res.op_id  = ii[ "op_id" ].integer;
                                        res.result = cast(ResultCode)ii[ "result" ].integer;
                                        ress ~= res;
                                    }
                                }
                            }
                            else
                            {
                                OpResult res;
                                res.op_id  = jres[ "op_id" ].integer;
                                res.result = cast(ResultCode)jres[ "result" ].integer;
                                ress ~= res;
                            }
                        }

                        nn_freemsg(buf);
                    }
                }
                else
                {
                    log.trace("ERR! N_CHANNEL: invalid socket");
                }

                if (ress.length == 0)
                {
                    OpResult res;
                    res.op_id  = -1;
                    res.result = ResultCode.Internal_Server_Error;
                    return [ res ];
                }

                return ress;
            }
            catch (Throwable tr)
            {
                log.trace("ERR! reqrep_json_2_main_module, %s", tr.info);
                log.trace("req: %s", req);
                log.trace("rep: %s", rep);

                if (ress.length == 0)
                {
                    OpResult res;
                    res.op_id  = -1;
                    res.result = ResultCode.Internal_Server_Error;
                    return [ res ];
                }

                return ress;
            }
        }
    }

    public string get_config_uri()
    {
        return node_id;
    }

    public static Context create_new(string _node_id, string context_name, Logger _log, string _main_module_url)
    {
        PThreadContext ctx = new PThreadContext();

        ctx.log = _log;

        if (ctx.log is null)
            writefln("context_name [%s] log is null", context_name);

        ctx.main_module_url = _main_module_url;
        ctx.node_id         = _node_id;

        ctx.storage = new LmdbStorage(context_name, ctx.log);

        ctx.name = context_name;

        ctx.get_configuration();

        ctx._vql = new VQL(ctx);

        ctx.onto = new Onto(ctx);
        ctx.onto.load();

        ctx.log.trace_log_and_console("NEW CONTEXT [%s]", context_name);

        return ctx;
    }

    bool isReadyAPI()
    {
        return API_ready;
    }

    public Ticket sys_ticket(bool is_new = false)
    {
        Ticket ticket = get_global_systicket();

        version (isModule)
        {
            ticket = *(storage.get_systicket_from_storage());
            set_global_systicket(ticket);
        }

        version (WebServer)
        {
            ticket = *(storage.get_systicket_from_storage());
            set_global_systicket(ticket);
        }

        return ticket;
    }

    public Individual get_configuration()
    {
        if (node == Individual.init && node_id !is null)
        {
            this.reopen_ro_individuals_storage_db();
            Ticket sticket = sys_ticket();

            node = get_individual(&sticket, node_id, OptAuthorize.NO);
            if (node.getStatus() != ResultCode.OK)
                node = Individual.init;
        }
        return node;
    }

    private long local_count_onto_update = -1;

    public Onto get_onto()
    {
        if (onto !is null)
        {
            long g_count_onto_update = get_count_onto_update();
            if (g_count_onto_update > local_count_onto_update)
            {
                local_count_onto_update = g_count_onto_update;
                onto.load();
            }
        }

        return onto;
    }

    public string get_name()
    {
        return name;
    }

    public Individual[ string ] get_onto_as_map_individuals()
    {
        if (onto !is null)
        {
            long g_count_onto_update = get_count_onto_update();
            if (g_count_onto_update > local_count_onto_update)
            {
                local_count_onto_update = g_count_onto_update;
                onto.load();
            }

            return onto.get_individuals;
        }
        else
            return (Individual[ string ]).init;
    }

    ref string[ string ] get_prefix_map()
    {
        return prefix_map;
    }

    void add_prefix_map(ref string[ string ] arg)
    {
        foreach (key, value; arg)
        {
            prefix_map[ key ] = value;
        }
    }

    // *************************************************** external api *********************************** //

    // /////////////////////////////////////////////////////// TICKET //////////////////////////////////////////////

    public bool is_ticket_valid(string ticket_id)
    {
        Ticket *ticket = storage.get_ticket(ticket_id, false);

        if (ticket is null)
            return false;

        SysTime now = Clock.currTime();
        if (now.stdTime < ticket.end_time)
            return true;

        return false;
    }

    // //////////////////////////////////////////// INDIVIDUALS IO /////////////////////////////////////
    public Individual[] get_individuals_via_query(string user_uri, string query_str, OptAuthorize op_auth, int top = 10, int limit = 10000)
    {
        Individual[] res;

        try
        {
            if (query_str.indexOf("==") > 0 || query_str.indexOf("&&") > 0 || query_str.indexOf("||") > 0)
            {
            }
            else
            {
                query_str = "'*' == '" ~ query_str ~ "'";
            }

            _vql.get(user_uri, query_str, null, null, top, limit, res, op_auth, false);
            return res;
        }
        finally
        {
//            stat(CMD_GET, sw);
//
            if (trace_msg[ T_API_140 ] == 1)
                log.trace("get_individuals_via_query: end, query_str=%s, result=%s", query_str, res);
        }
    }

    public VQL get_vql()
    {
        return _vql;
    }

    public void reopen_ro_fulltext_indexer_db()
    {
        if (_vql !is null)
            _vql.reopen_db();
    }

    public void reopen_ro_individuals_storage_db()
    {
        storage.get_inividuals_storage_r().reopen();
    }

    public void reopen_ro_acl_storage_db()
    {
        storage.get_acl_client().reopen();
    }

    // ////////// external ////////////

    public ubyte get_rights(Ticket *ticket, string uri, ubyte access)
    {
        if (ticket is null)
            return 0;

        return storage.get_acl_client().authorize(uri, ticket.user_uri, access, true, null, null,
                                                  null);
    }

    public void get_rights_origin_from_acl(Ticket *ticket, string uri, OutBuffer trace_acl, OutBuffer trace_info)
    {
        if (ticket is null)
            return;

        storage.get_acl_client().authorize(uri, ticket.user_uri, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, trace_acl, null,
                                           trace_info);
    }

    public void get_membership_from_acl(Ticket *ticket, string uri, OutBuffer trace_group)
    {
        if (ticket is null)
            return;

        storage.get_acl_client().authorize(uri, ticket.user_uri, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, null, trace_group,
                                           null);
    }

    public SearchResult get_individuals_ids_via_query(string user_uri, string query_str, string sort_str, string db_str, int from, int top, int limit,
                                                      void delegate(string uri) prepare_element_event, OptAuthorize op_auth, bool trace)
    {
        SearchResult sr;

        if ((query_str.indexOf("==") > 0 || query_str.indexOf("&&") > 0 || query_str.indexOf("||") > 0) == false)
            query_str = "'*' == '" ~ query_str ~ "'";

        sr = _vql.get(user_uri, query_str, sort_str, db_str, from, top, limit, prepare_element_event, op_auth, trace);

        return sr;
    }

    public Individual get_individual(Ticket *ticket, string uri, OptAuthorize opt_authorize = OptAuthorize.YES)
    {
        Individual individual = Individual.init;

        if (ticket is null)
        {
            log.trace("get_individual, uri=%s, ticket is null", uri);
            return individual;
        }

        if (trace_msg[ T_API_150 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individual, uri=%s, ticket=%s", uri, ticket.id);
        }

        try
        {
            string individual_as_binobj = storage.get_from_individual_storage(ticket.user_uri, uri);
            if (individual_as_binobj !is null && individual_as_binobj.length > 1)
            {
                if (opt_authorize == OptAuthorize.NO ||
                    storage.get_acl_client().authorize(uri, ticket.user_uri, Access.can_read, true, null, null, null) == Access.can_read)
                {
                    if (individual.deserialize(individual_as_binobj) > 0)
                        individual.setStatus(ResultCode.OK);
                    else
                    {
                        individual.setStatus(ResultCode.Unprocessable_Entity);
                        writeln("ERR!: invalid binobj: [", individual_as_binobj, "] ", uri);
                    }
                }
                else
                {
                    if (trace_msg[ T_API_160 ] == 1)
                        log.trace("get_individual, not authorized, uri=%s, user_uri=%s", uri, ticket.user_uri);
                    individual.setStatus(ResultCode.Not_Authorized);
                }
            }
            else
            {
                individual.setStatus(ResultCode.Unprocessable_Entity);
                //writeln ("ERR!: empty binobj: [", individual_as_binobj, "] ", uri);
            }

            return individual;
        }
        finally
        {
//            stat(CMD_GET, sw);
            if (trace_msg[ T_API_170 ] == 1)
                log.trace("get_individual: end, uri=%s", uri);
        }
    }

    public Individual[] get_individuals(Ticket *ticket, string[] uris)
    {
        StopWatch sw; sw.start;

        try
        {
            Individual[] res = Individual[].init;

            if (ticket is null)
            {
                log.trace("get_individuals, uris=%s, ticket is null", uris);
                return res;
            }

            foreach (uri; uris)
            {
                if (storage.get_acl_client().authorize(uri, ticket.user_uri, Access.can_read, true, null, null, null) == Access.can_read)
                {
                    Individual individual           = Individual.init;
                    string     individual_as_binobj = storage.get_from_individual_storage(ticket.user_uri, uri);

                    if (individual_as_binobj !is null && individual_as_binobj.length > 1)
                    {
                        if (individual.deserialize(individual_as_binobj) > 0)
                            res ~= individual;
                        else
                        {
                            Individual indv;
                            indv.uri = uri;
                            indv.setStatus(ResultCode.Unprocessable_Entity);
                            res ~= indv;
                        }
                    }
                }
            }

            return res;
        }
        finally
        {
            //stat(CMD_GET, sw);
        }
    }

    public OpResult update(long tnx_id, Ticket *ticket, INDV_OP cmd, Individual *indv, string event_id, MODULES_MASK assigned_subsystems,
                           OptFreeze opt_freeze,
                           OptAuthorize opt_request)
    {
        //log.trace("[%s] add_to_transaction: %s %s", name, text(cmd), *indv);

        StopWatch sw; sw.start;

        OpResult  res = OpResult(ResultCode.Fail_Store, -1);

        try
        {
            if (indv !is null && (indv.uri is null || indv.uri.length < 2))
            {
                res.result = ResultCode.Invalid_Identifier;
                return res;
            }
            if (indv is null || (cmd != INDV_OP.REMOVE && indv.resources.length == 0))
            {
                res.result = ResultCode.No_Content;
                return res;
            }

            version (isModule)
            {
                //log.trace("[%s] add_to_transaction: isModule", name);

                string scmd;

                if (cmd == INDV_OP.PUT)
                    scmd = "put";
                else if (cmd == INDV_OP.ADD_IN)
                    scmd = "add_to";
                else if (cmd == INDV_OP.SET_IN)
                    scmd = "set_in";
                else if (cmd == INDV_OP.REMOVE_FROM)
                    scmd = "remove_from";
                else if (cmd == INDV_OP.REMOVE)
                    scmd = "remove";

                JSONValue req_body;
                req_body[ "function" ]            = scmd;
                req_body[ "ticket" ]              = ticket.id;
                req_body[ "individuals" ]         = [ individual_to_json(*indv) ];
                req_body[ "assigned_subsystems" ] = assigned_subsystems;
                req_body[ "event_id" ]            = event_id;
                req_body[ "tnx_id" ]              = tnx_id;

                //log.trace("[%s] add_to_transaction: (isModule), req=(%s)", name, req_body.toString());

                res = reqrep_json_2_main_module(req_body)[ 0 ];
                //log.trace("[%s] add_to_transaction: (isModule), rep=(%s)", name, res);
            }

            return res;
        }
        finally
        {
            if (res.result != ResultCode.OK)
                log.trace("ERR! update: no store individual: errcode=[%s], ticket=[%s] indv=[%s]", text(res.result),
                          indv !is null ? text(*indv) : "null",
                          ticket !is null ? text(*ticket) : "null");

            if (trace_msg[ T_API_240 ] == 1)
                log.trace("[%s] add_to_transaction [%s] = %s", name, indv.uri, res);

            //stat(CMD_PUT, sw);
        }
    }

    public void freeze()
    {
        version (isModule)
        {
            JSONValue req_body;
            req_body[ "function" ] = "freeze";
            OpResult  res = reqrep_json_2_main_module(req_body)[ 0 ];
        }
    }

    public void unfreeze()
    {
        version (isModule)
        {
            JSONValue req_body;
            req_body[ "function" ] = "unfreeze";
            OpResult  res = reqrep_json_2_main_module(req_body)[ 0 ];
        }
    }

    //////////////////////////////////////////////// MODULES INTERACTION

    public long get_operation_state(MODULE module_id, long wait_op_id)
    {
        long  res = -1;

        MInfo info = storage.get_info(module_id);

        if (info.is_Ok)
        {
            if (module_id == MODULE.fulltext_indexer || module_id == MODULE.scripts_main)
                res = info.committed_op_id;
            else
                res = info.op_id;
        }

        version (WebServer)
        {
            if (module_id == MODULE.subject_manager)
                this.reopen_ro_individuals_storage_db();

            if (module_id == MODULE.acl_preparer)
                this.reopen_ro_acl_storage_db();

            if (module_id == MODULE.fulltext_indexer)
                this.reopen_ro_fulltext_indexer_db();
        }

        log.trace("get_operation_state(%s) res=%s, wait_op_id=%d", text(module_id), info, wait_op_id);

        return res;
    }

    public ResultCode commit(Transaction *in_tnx, OptAuthorize opt_authorize = OptAuthorize.YES)
    {
        ResultCode rc;
        long       op_id;

        if (in_tnx.get_queue().length == 0)
        {
            return ResultCode.OK;
        }

        if (in_tnx.is_autocommit == true)
        {
            bool[ string ] uri2exists;

            foreach (item; in_tnx.get_queue())
            {
                if (item.cmd != INDV_OP.REMOVE && item.new_indv == Individual.init)
                    continue;

                if (item.rc != ResultCode.OK)
                    return item.rc;

                Ticket *ticket = storage.get_ticket(item.ticket_id, false);

                //log.trace ("transaction: cmd=%s, indv=%s ", item.cmd, item.indv);

                if (uri2exists.get(item.uri, false) == true && item.new_indv.getResources("v-s:updateCounter").length == 0)
                {
                    item.new_indv.setResources("v-s:updateCounter", [ Resource(-1) ]);
                }

                rc = this.update(in_tnx.id, ticket, item.cmd, &item.new_indv, item.event_id, item.assigned_subsystems, OptFreeze.NONE, opt_authorize).result;

                uri2exists[ item.uri ] = true;

                if (rc == ResultCode.No_Content)
                {
                    this.get_logger().trace("WARN!: Rejected attempt to save an empty object: %s", item.new_indv);
                }

                if (rc != ResultCode.OK && rc != ResultCode.No_Content)
                {
                    this.get_logger().trace("FAIL COMMIT");
                    return rc;
                }
                //else
                //log.trace ("SUCCESS COMMIT");
            }
        }
        else
        {
            version (isModule)
            {
                //log.trace("@0 -------------------------------------------------------------------------------------------");
                //log.trace("@1 commit, tnx.id=%s tnx.len=%d", in_tnx.id, in_tnx.get_queue().length);

                Individual imm;
                imm.uri = "tnx:" ~ text(in_tnx.id);
                imm.addResource("fn", Resource(DataType.String, "commit"));

                Resources items;

                int       idx = 0;
                foreach (ti; in_tnx.get_queue())
                {
                    //log.trace("@2 ti=%s", ti);
                    Individual iti;

                    iti.uri = "el:" ~ text(idx);
                    iti.addResource("cmd", Resource(ti.cmd));

                    if (ti.user_uri !is null && ti.user_uri.length > 0)
                        iti.addResource("user_uri", Resource(DataType.Uri, ti.user_uri));

                    iti.addResource("uri", Resource(DataType.Uri, ti.uri));

                    if (ti.prev_binobj !is null && ti.prev_binobj.length > 0)
                        iti.addResource("prev_binobj", Resource(DataType.String, ti.prev_binobj));

                    if (ti.new_binobj !is null && ti.new_binobj.length > 0)
                        iti.addResource("new_binobj", Resource(DataType.String, ti.new_binobj));

                    iti.addResource("update_counter", Resource(DataType.Integer, ti.update_counter));
                    iti.addResource("event_id", Resource(DataType.String, ti.event_id));

                    string iti_binobj = iti.serialize();
                    items ~= Resource(iti_binobj);
                }

                imm.setResources("items", items);

                string binobj = imm.serialize();

                //log.trace("[%s] commit: (isModule), req=(%s)", name, binobj);

                OpResult res = reqrep_binobj_2_main_module(binobj)[ 0 ];
                //log.trace("[%s] commit: (isModule), rep=(%s)", name, res);

                if (res.result != ResultCode.OK && res.result != ResultCode.No_Content)
                {
                    this.get_logger().trace("FAIL COMMIT");
                    return res.result;
                }
            }
        }
        return ResultCode.OK;
    }
}
