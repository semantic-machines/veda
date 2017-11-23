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
    import veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.core.storage.lmdb_storage;
    import veda.core.az.acl, veda.core.search.vql, veda.core.common.transaction, veda.util.module_info, veda.common.logger;

    version (isMStorage)
    {
        alias veda.mstorage.storage_manager ticket_storage_module;
        alias veda.mstorage.storage_manager subject_storage_module;
    }
}

/// реализация интерфейса Context
class PThreadContext : Context
{
    //bool[ P_MODULE ] is_traced_module;

    private Ticket *[ string ] user_of_ticket;

    // // // authorization
    private Authorization _acl_indexes;

    private Onto          onto;

    private string        name;

    private               string[ string ] prefix_map;

    private Storage       inividuals_storage_r;
    private Storage       tickets_storage_r;
    private VQL           _vql;

    private long          local_last_update_time;
    private Individual    node = Individual.init;
    private string        node_id;

    private bool          API_ready = true;
    private string        main_module_url;
    private Logger        log;

    private long          last_ticket_manager_op_id = 0;

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

                    bytes = nn_send(sock, cast(char *)req, req.length + 1, 0);
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

    public Authorization acl_indexes()
    {
        if (_acl_indexes is null)
            _acl_indexes = new Authorization(acl_indexes_db_path, DBMode.R, name ~ ":acl", this.log);

        return _acl_indexes;
    }

    public string get_config_uri()
    {
        return node_id;
    }

    public static Context create_new(string _node_id, string context_name, string individuals_db_path, Logger _log, string _main_module_url,
                                     Authorization in_acl_indexes, Storage in_inividuals_storage_r, Storage in_tickets_storage_r)
    {
        PThreadContext ctx = new PThreadContext();

        ctx.log = _log;

        if (ctx.log is null)
            writefln("context_name [%s] log is null", context_name);

        ctx._acl_indexes = in_acl_indexes;

        ctx.main_module_url = _main_module_url;
/*
        {
            import std.experimental.Logger;
            import std.experimental.Logger.core;

            std.experimental.Logger.core.globalLogLevel(LogLevel.info);
        }
 */
        ctx.node_id = _node_id;

        if (in_inividuals_storage_r is null)
            ctx.inividuals_storage_r = new LmdbStorage(individuals_db_path, DBMode.R, context_name ~ ":inividuals", ctx.log);
        else
            ctx.inividuals_storage_r = in_inividuals_storage_r;

        if (in_tickets_storage_r is null)
            ctx.tickets_storage_r = new LmdbStorage(tickets_db_path, DBMode.R, context_name ~ ":tickets", ctx.log);
        else
            ctx.tickets_storage_r = in_tickets_storage_r;

        ctx.name = context_name;

        ctx.get_configuration();

        ctx._vql = new VQL(ctx);

        ctx.onto = new Onto(ctx);
        ctx.onto.load();

        ctx.log.trace_log_and_console("NEW CONTEXT [%s]", context_name);

        return ctx;
    }

    ~this()
    {
        log.trace_log_and_console("DELETE CONTEXT [%s]", name);
        inividuals_storage_r.close_db();
        tickets_storage_r.close_db();
    }

    bool isReadyAPI()
    {
        return API_ready;
    }

    public Storage get_inividuals_storage_r()
    {
        return inividuals_storage_r;
    }


    public Ticket sys_ticket(bool is_new = false)
    {
        Ticket ticket = get_global_systicket();

        version (isModule)
        {
            ticket = *get_systicket_from_storage();
            set_global_systicket(ticket);
        }

        version (WebServer)
        {
            ticket = *get_systicket_from_storage();
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

    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    bool authorize(string uri, Ticket *ticket, ubyte request_acess, bool is_check_for_reload)
    {
        if (ticket is null)
        {
            printPrettyTrace(stderr);
        }

        //writeln ("@p ### uri=", uri, " ", request_acess);
        ubyte res = acl_indexes.authorize(uri, ticket, request_acess, is_check_for_reload, null, null, null);

        //writeln ("@p ### uri=", uri, " ", request_acess, " ", request_acess == res);
        return request_acess == res;
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


    public string get_from_individual_storage(string user_id, string uri)
    {
        //writeln ("@ get_individual_as_binobj, uri=", uri);
        string res;

        if (inividuals_storage_r !is null)
            res = inividuals_storage_r.find(true, user_id, uri);

        if (res !is null && res.length < 10)
            log.trace_log_and_console("ERR! get_individual_from_storage, found invalid BINOBJ, uri=%s", uri);

        return res;
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
        Ticket *ticket = get_ticket(ticket_id);

        if (ticket is null)
            return false;

        SysTime now = Clock.currTime();
        if (now.stdTime < ticket.end_time)
            return true;

        return false;
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

        version (WebServer)
        {
            subject2Ticket(new_ticket, &ticket);
            user_of_ticket[ ticket.id ] = new Ticket(ticket);
        }

        return ticket;
    }


    public string get_ticket_from_storage(string ticket_id)
    {
        return tickets_storage_r.find(false, null, ticket_id);
    }

    public Ticket *get_systicket_from_storage()
    {
        string systicket_id = tickets_storage_r.find(false, null, "systicket");

        if (systicket_id is null)
            log.trace("SYSTICKET NOT FOUND");

        return get_ticket(systicket_id, true);
    }

    public Ticket *get_ticket(string ticket_id, bool is_systicket = false)
    {
        //StopWatch sw; sw.start;

        try
        {
            Ticket *tt;
            if (ticket_id is null || ticket_id == "" || ticket_id == "systicket")
                ticket_id = "guest";

            tt = user_of_ticket.get(ticket_id, null);

            if (tt is null)
            {
                string when     = null;
                int    duration = 0;

                MInfo  mi = get_info(MODULE.ticket_manager);

                //log.trace ("last_ticket_manager_op_id=%d, mi.op_id=%d,  mi.committed_op_id=%d", last_ticket_manager_op_id, mi.op_id, mi.committed_op_id);
                if (last_ticket_manager_op_id < mi.op_id)
                {
                    last_ticket_manager_op_id = mi.op_id;
                    this.reopen_ro_ticket_manager_db();
                }

                string ticket_str = tickets_storage_r.find(false, null, ticket_id);
                if (ticket_str !is null && ticket_str.length > 120)
                {
                    tt = new Ticket;
                    Individual ticket;

                    if (ticket.deserialize(ticket_str) > 0)
                    {
                        subject2Ticket(ticket, tt);
                        tt.result               = ResultCode.OK;
                        user_of_ticket[ tt.id ] = tt;

                        if (trace_msg[ T_API_80 ] == 1)
                            log.trace("тикет найден в базе, id=%s", ticket_id);
                    }
                    else
                    {
                        tt.result = ResultCode.Unprocessable_Entity;
                        log.trace("ERR! invalid individual=%s", ticket_str);
                    }
                }
                else
                {
                    tt        = new Ticket;
                    tt.result = ResultCode.Ticket_not_found;

                    if (trace_msg[ T_API_90 ] == 1)
                        log.trace("тикет не найден в базе, id=%s", ticket_id);
                }
            }
            else
            {
                if (trace_msg[ T_API_100 ] == 1)
                    log.trace("тикет нашли в кеше, id=%s, end_time=%d", tt.id, tt.end_time);

                SysTime now = Clock.currTime();
                if (now.stdTime >= tt.end_time && !is_systicket)
                {
                    log.trace("ticket %s expired, user=%s, start=%s, end=%s, now=%s", tt.id, tt.user_uri, SysTime(tt.start_time,
                                                                                                                  UTC()).toISOExtString(),
                              SysTime(tt.end_time, UTC()).toISOExtString(), now.toISOExtString());

                    if (ticket_id == "guest")
                    {
                        Ticket guest_ticket = create_new_ticket("cfg:Guest", "900000000", "guest");
                        tt = &guest_ticket;
                    }
                    else
                    {
                        tt        = new Ticket;
                        tt.id     = "?";
                        tt.result = ResultCode.Ticket_expired;
                    }
                    return tt;
                }
                else
                {
                    tt.result = ResultCode.OK;
                }

                if (trace_msg[ T_API_120 ] == 1)
                    log.trace("ticket: %s", *tt);
            }
            return tt;
        }
        finally
        {
            //stat(CMD_GET, sw);
        }
    }


    // //////////////////////////////////////////// INDIVIDUALS IO /////////////////////////////////////
    public Individual[] get_individuals_via_query(Ticket *ticket, string query_str, OptAuthorize op_auth, int top = 10, int limit = 10000)
    {
//        StopWatch sw; sw.start;

        if (trace_msg[ T_API_130 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individuals_via_query: start, query_str=%s, ticket=%s", query_str, ticket.id);
            else
                log.trace("get_individuals_via_query: start, query_str=%s, ticket=null", query_str);
        }

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

            _vql.get(ticket, query_str, null, null, top, limit, res, op_auth, false);
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

    public void reopen_ro_ticket_manager_db()
    {
        if (tickets_storage_r !is null)
            tickets_storage_r.reopen_db();
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
        if (inividuals_storage_r !is null)
            inividuals_storage_r.reopen_db();
    }

    public void reopen_ro_acl_storage_db()
    {
        if (acl_indexes !is null)
            acl_indexes.reopen_db();
        //log.trace ("reopen_ro_acl_storage_db");
    }

    // ////////// external ////////////

    public ubyte get_rights(Ticket *ticket, string uri)
    {
        return acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, null, null, null);
    }

    public void get_rights_origin_from_acl(Ticket *ticket, string uri,
                                           void delegate(string resource_group, string subject_group, string right) trace_acl, void delegate(string log) trace_info)
    {
        acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, trace_acl, null, trace_info);
    }

    public void get_membership_from_acl(Ticket *ticket, string uri,
                                        void delegate(string resource_group) trace_group)
    {
        acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, null, trace_group, null);
    }

    public SearchResult get_individuals_ids_via_query(Ticket *ticket, string query_str, string sort_str, string db_str, int from, int top, int limit,
                                                      void delegate(string uri) prepare_element_event, OptAuthorize op_auth, bool trace)
    {
        SearchResult sr;
        
        if ((query_str.indexOf("==") > 0 || query_str.indexOf("&&") > 0 || query_str.indexOf("||") > 0) == false)
            query_str = "'*' == '" ~ query_str ~ "'";
    
        sr = _vql.get(ticket, query_str, sort_str, db_str, from, top, limit, prepare_element_event, op_auth, trace);
    
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
            string individual_as_binobj = get_from_individual_storage(ticket.user_uri, uri);
            if (individual_as_binobj !is null && individual_as_binobj.length > 1)
            {
                if (opt_authorize == OptAuthorize.NO || acl_indexes.authorize(uri, ticket, Access.can_read, true, null, null, null) == Access.can_read)
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

            foreach (uri; uris)
            {
                if (acl_indexes.authorize(uri, ticket, Access.can_read, true, null, null, null) == Access.can_read)
                {
                    Individual individual           = Individual.init;
                    string     individual_as_binobj = get_from_individual_storage(ticket.user_uri, uri);

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

    public string get_individual_as_binobj(Ticket *ticket, string uri, out ResultCode rs)
    {
        string    res;
        StopWatch sw; sw.start;

        rs = ResultCode.Unprocessable_Entity;

        if (ticket is null)
        {
            rs = ResultCode.Ticket_not_found;
            log.trace("get_individual as binobj, uri=%s, ticket is null", uri);
            return null;
        }

        if (trace_msg[ T_API_180 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individual as binobj, uri=[%s], ticket=[%s]", uri, ticket.id);
        }

        try
        {
            string individual_as_binobj = get_from_individual_storage(ticket.user_uri, uri);
            if (individual_as_binobj !is null && individual_as_binobj.length > 1)
            {
                res = individual_as_binobj;
                rs  = ResultCode.OK;
            }
            else
            {
                return res;
            }

            if (acl_indexes.authorize(uri, ticket, Access.can_read, true, null, null, null) != Access.can_read)
            {
                if (trace_msg[ T_API_190 ] == 1)
                    log.trace("get_individual as binobj, not authorized, uri=[%s], user_uri=[%s]", uri, ticket.user_uri);
                rs  = ResultCode.Not_Authorized;
                res = null;
            }

            return res;
        }
        finally
        {
            //stat(CMD_GET, sw);
            if (trace_msg[ T_API_200 ] == 1)
                log.trace("get_individual as binobj: end, uri=%s", uri);
        }
    }

    static const byte NEW_TYPE    = 0;
    static const byte EXISTS_TYPE = 1;

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

    public OpResult put_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                   OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES)
    {
        individual.uri = uri;
        return update(transaction_id, ticket, INDV_OP.PUT, &individual, event_id, assigned_subsystems, opt_freeze, opt_request);
    }

    public OpResult remove_individual(Ticket *ticket, string uri, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                      OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES)
    {
        Individual individual;

        individual.uri = uri;
        return update(transaction_id, ticket, INDV_OP.REMOVE, &individual, event_id, assigned_subsystems, opt_freeze, opt_request);
    }

    public OpResult add_to_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                      OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES)
    {
        individual.uri = uri;
        return update(transaction_id, ticket, INDV_OP.ADD_IN, &individual, event_id, assigned_subsystems, opt_freeze, opt_request);
    }

    public OpResult set_in_individual(Ticket *ticket, string uri, Individual individual, string event_id, long transaction_id, MODULES_MASK assigned_subsystems,
                                      OptFreeze opt_freeze = OptFreeze.NONE, OptAuthorize opt_request = OptAuthorize.YES)
    {
        individual.uri = uri;
        return update(transaction_id, ticket, INDV_OP.SET_IN, &individual, event_id, assigned_subsystems, opt_freeze, opt_request);
    }

    public OpResult remove_from_individual(Ticket *ticket, string uri, Individual individual, string event_id,
                                           long transaction_id, MODULES_MASK assigned_subsystems, OptFreeze opt_freeze = OptFreeze.NONE,
                                           OptAuthorize opt_request = OptAuthorize.YES)
    {
        individual.uri = uri;
        return update(transaction_id, ticket, INDV_OP.REMOVE_FROM, &individual, event_id, assigned_subsystems, opt_freeze, opt_request);
    }

    public void set_trace(int idx, bool state)
    {
//        writeln("set trace idx=", idx, ":", state);
//        foreach (mid; is_traced_module.keys)
//        {
//            Tid tid = getTid(mid);
//            if (tid != Tid.init)
//                send(tid, CMD_SET_TRACE, idx, state);
//        }

//        veda.core.log_msg.set_trace(idx, state);
    }

    public long count_individuals()
    {
        long count = 0;

        if (inividuals_storage_r !is null)
            count = inividuals_storage_r.count_entries();

        return count;
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

    private ModuleInfoFile[ MODULE ] info_r__2__pmodule;
    public MInfo get_info(MODULE module_id)
    {
        ModuleInfoFile mdif = info_r__2__pmodule.get(module_id, null);

        if (mdif is null)
        {
            mdif                            = new ModuleInfoFile(text(module_id), log, OPEN_MODE.READER);
            info_r__2__pmodule[ module_id ] = mdif;
        }
        MInfo info = mdif.get_info();
        return info;
    }


    public long get_operation_state(MODULE module_id, long wait_op_id)
    {
        long  res = -1;

        MInfo info = get_info(module_id);

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

    private bool wait_module(MODULE pm, long wait_op_id, long timeout)
    {
        long wait_time         = 0;
        long op_id_from_module = 0;

        //writefln ("wait_module pm=%s op_id=%d", text (pm), op_id);
        while (wait_op_id > op_id_from_module)
        {
            MInfo info = get_info(pm);

            if (info.is_Ok)
                op_id_from_module = info.committed_op_id;
            else
                return false;

            if (op_id_from_module >= wait_op_id)
                return true;

            core.thread.Thread.sleep(dur!("msecs")(100));
            wait_time += 100;

            if (wait_time > timeout)
            {
                log.trace("WARN! timeout (wait opid=%d, opid from module = %d) wait_module:%s", wait_op_id, op_id_from_module, text(pm));
                return false;
            }
        }
        return true;
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
            foreach (item; in_tnx.get_queue())
            {
                if (item.cmd != INDV_OP.REMOVE && item.new_indv == Individual.init)
                    continue;

                if (item.rc != ResultCode.OK)
                    return item.rc;

                Ticket *ticket = this.get_ticket(item.ticket_id);

                //log.trace ("transaction: cmd=%s, indv=%s ", item.cmd, item.indv);
                rc = this.update(in_tnx.id, ticket, item.cmd, &item.new_indv, item.event_id, item.assigned_subsystems, OptFreeze.NONE, opt_authorize).result;

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
