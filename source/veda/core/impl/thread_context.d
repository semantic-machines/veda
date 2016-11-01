/**
 * Внешнее API - Реализация
 */

module veda.core.impl.thread_context;

private
{
    import core.thread, std.stdio, std.format, std.datetime, std.concurrency, std.conv, std.outbuffer, std.string, std.uuid, std.file, std.path,
           std.json, std.regex;
    import veda.bind.xapian_d_header;
    import veda.util.container, veda.common.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.individual8json;
    import veda.common.type, veda.core.common.know_predicates, veda.core.common.define, veda.core.common.context,
           veda.core.common.log_msg, veda.util.module_info;
    import veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.core.storage.lmdb_storage;
    import veda.core.az.acl, veda.core.search.vql;
    import veda.util.module_info;
    import veda.common.logger;

    version (isServer)
    {
        alias veda.server.storage_manager storage_module;
        alias veda.server.acl_manager     acl_module;
        alias veda.server.load_info       load_info;
    }
}

Tid  dummy_tid;

File *ff_key2slot_r = null;
public int[ string ] read_key2slot()
{
    int[ string ] key2slot;

    if (ff_key2slot_r is null)
    {
        string file_name_key2slot = xapian_info_path ~ "/key2slot";
        try
        {
            ff_key2slot_r = new File(file_name_key2slot, "r");
        }
        catch (Exception ex) {}
    }

    if (ff_key2slot_r !is null)
    {
        ff_key2slot_r.seek(0);
        auto buf = ff_key2slot_r.rawRead(new char[ 100 * 1024 ]);
        key2slot = deserialize_key2slot(cast(string)buf);
//            writeln("@context:read_key2slot:key2slot", key2slot);
    }
    return key2slot;
}

/// реализация интерфейса Context
class PThreadContext : Context
{
    long local_count_put;

    bool[ P_MODULE ] is_traced_module;

    private Ticket *[ string ] user_of_ticket;

    // // // authorization
    private Authorization _acl_indexes;

    private Onto          onto;

    private string        name;
    private P_MODULE      id;

    private string        old_msg_key2slot;
    private int[ string ] old_key2slot;

    private             string[ string ] prefix_map;

    private LmdbStorage inividuals_storage_r;
    private LmdbStorage tickets_storage_r;
    private VQL         _vql;

    private long        local_last_update_time;
    private Individual  node = Individual.init;
    private string      node_id;

    private bool        API_ready = true;
    private string      main_module_url;
    private Logger      log;

    private long        last_ticket_manager_op_id = 0;

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

        private OpResult reqrep_2_main_module(ref JSONValue jreq)
        {
            OpResult res;
            string   req = jreq.toString();

            int      sock = get_sock_2_main_module();

            if (sock >= 0)
            {
                char *buf = cast(char *)0;
                int  bytes;

                bytes = nn_send(sock, cast(char *)req, req.length + 1, 0);
                //log.trace("N_CHANNEL send (%s)", req);
                bytes = nn_recv(sock, &buf, NN_MSG, 0);
                if (bytes > 0)
                {
                    string rep = to!string(buf);
                    //log.trace("N_CHANNEL recv (%s)", rep);

                    JSONValue jres = parseJSON(rep);

                    if (jres[ "type" ].str == "OpResult")
                    {
                        res.op_id  = jres[ "op_id" ].integer;
                        res.result = cast(ResultCode)jres[ "result" ].integer;
                    }

                    nn_freemsg(buf);
                }
            }
            else
            {
                log.trace("ERR! N_CHANNEL: invalid socket");
            }

            return res;
        }
    }

    public Authorization acl_indexes()
    {
        if (_acl_indexes is null)
            _acl_indexes = new Authorization(acl_indexes_db_path, DBMode.R, name ~ ":acl", this.log);

        return _acl_indexes;
    }

    this(string _node_id, string context_name, P_MODULE _id, Logger _log, string _main_module_url = null, Authorization in_acl_indexes = null)
    {
        log = _log;

        if (log is null)
            writeln("P_MODULE _id=", text(_id), " log is null");

        _acl_indexes = in_acl_indexes;

        main_module_url = _main_module_url;
/*
        {
            import std.experimental.Logger;
            import std.experimental.Logger.core;

            std.experimental.Logger.core.globalLogLevel(LogLevel.info);
        }
 */
        node_id = _node_id;

        inividuals_storage_r = new LmdbStorage(individuals_db_path, DBMode.R, context_name ~ ":inividuals", this.log);
        tickets_storage_r    = new LmdbStorage(tickets_db_path, DBMode.R, context_name ~ ":tickets", this.log);

        name = context_name;
        id   = _id;

        is_traced_module[ P_MODULE.ticket_manager ]  = true;
        is_traced_module[ P_MODULE.subject_manager ] = true;
//        is_traced_module[ P_MODULE.acl_preparer ]      = true;
//        is_traced_module[ P_MODULE.fulltext_indexer ] = true;
//        is_traced_module[ P_MODULE.scripts ]          = true;

        getConfiguration();

        _vql = new VQL(this);

        onto = new Onto(this);
        onto.load();

        local_count_put = get_subject_manager_op_id();
        //ft_local_count  = get_count_indexed();

        log.trace_log_and_console("NEW CONTEXT [%s]", context_name);
    }

    string begin_transaction()
    {
        string res;

        version (isServer)
        {
            res = storage_module.begin_transaction(P_MODULE.subject_manager);
        }

        return res;
    }

    void commit_transaction(string transaction_id)
    {
        version (isServer)
        {
            storage_module.commit_transaction(P_MODULE.subject_manager, transaction_id);
        }
    }

    void abort_transaction(string transaction_id)
    {
        version (isServer)
        {
            storage_module.abort_transaction(P_MODULE.subject_manager, transaction_id);
        }
    }


    bool isReadyAPI()
    {
        return API_ready;
    }

    public Storage get_subject_storage_db()
    {
        return inividuals_storage_r;
    }

    @property
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

        version (isServer)
        {
            if (ticket == Ticket.init || ticket.user_uri == "" || is_new)
            {
                try
                {
                    ticket = create_new_ticket("cfg:VedaSystem", "400000");

                    long op_id;
                    storage_module.put(P_MODULE.ticket_manager, null, Resources.init, "systicket", null, ticket.id, -1, null, false, op_id);
                    log.trace("systicket [%s] was created", ticket.id);

                    Individual sys_account_permission;
                    sys_account_permission.uri = "p:" ~ ticket.id;
                    sys_account_permission.addResource("rdf:type", Resource(DataType.Uri, "v-s:PermissionStatement"));
                    sys_account_permission.addResource("v-s:canCreate", Resource(DataType.Boolean, "true"));
                    sys_account_permission.addResource("v-s:permissionObject", Resource(DataType.Uri, "v-s:AllResourcesGroup"));
                    sys_account_permission.addResource("v-s:permissionSubject", Resource(DataType.Uri, "cfg:VedaSystem"));
                    OpResult opres = this.put_individual(&ticket, sys_account_permission.uri, sys_account_permission, false, "srv", false, false);

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
        }
        return ticket;
    }

    public Individual getConfiguration()
    {
        if (node == Individual.init && node_id !is null)
        {
            this.reopen_ro_subject_storage_db();
            Ticket sticket = sys_ticket();

            node = get_individual(&sticket, node_id);
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
        ubyte res = acl_indexes.authorize(uri, ticket, request_acess, this, is_check_for_reload);

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


    public string get_from_individual_storage(string uri)
    {
        //writeln ("@ get_individual_as_cbor, uri=", uri);
        string res;

        if (inividuals_storage_r !is null)
            res = inividuals_storage_r.find(uri);
        else
            res = get_from_individual_storage_thread(uri);

        if (res !is null && res.length < 10)
            log.trace_log_and_console("ERR! get_individual_from_storage, found invalid CBOR, uri=%s", uri);

        return res;
    }

    public int[ string ] get_key2slot()
    {
        return read_key2slot();
//        string key2slot_str;

//        if (inividuals_storage !is null)
//            key2slot_str = inividuals_storage.find(xapian_metadata_doc_id);
//        else
//            key2slot_str = get_from_individual_storage_thread(xapian_metadata_doc_id);

//        if (key2slot_str !is null)
//        {
//            int[ string ] key2slot = deserialize_key2slot(key2slot_str);
//            return key2slot;
//        }
//        return (int[ string ]).init;
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

    private void subject2Ticket(ref Individual ticket, Ticket *tt)
    {
        string when;
        long   duration;

        tt.id       = ticket.uri;
        tt.user_uri = ticket.getFirstLiteral(ticket__accessor);
        when        = ticket.getFirstLiteral(ticket__when);
        string dd = ticket.getFirstLiteral(ticket__duration);

        try
        {
            duration = parse!uint (dd);
        }
        catch (Exception ex)
        {
            writeln("Ex!: ", __FUNCTION__, ":", text(__LINE__), ", ", ex.msg);
        }

        if (tt.user_uri is null)
        {
            if (trace_msg[ T_API_10 ] == 1)
                log.trace("found a session ticket is not complete, the user can not be found.");
        }

        if (tt.user_uri !is null && (when is null || duration < 10))
        {
            if (trace_msg[ T_API_20 ] == 1)
                log.trace("found a session ticket is not complete, we believe that the user has not been found.");
            tt.user_uri = null;
        }

        if (when !is null)
        {
            if (trace_msg[ T_API_30 ] == 1)
                log.trace("session ticket %s Ok, user=%s, when=%s, duration=%d", tt.id, tt.user_uri, when,
                          duration);

            tt.end_time = stringToTime(when) + duration * 10_000_000;
        }
    }

    public void stat(byte command_type, ref StopWatch sw) nothrow
    {
        version (isServer)
            load_info.stat(command_type, sw);
    }

    int _timeout = 10;

//    long ft_local_count;
//    long ft_local_time_check = 0;
//    public bool ft_check_for_reload(void delegate() load)
//    {
//        return _check_for_reload(ft_local_time_check, ft_local_count, &get_count_indexed, load);
//    }

    //long acl_local_count;
    //long acl_local_time_check = 0;
    long last_committed_op_id_acl_manager = 0;
    public bool acl_check_for_reload(void delegate() load)
    {
        MInfo mi = get_info(P_MODULE.acl_preparer);

        //log.trace ("acl_check_for_reload #1, last_committed_op_id_acl_manager=%d, mi=%s", last_committed_op_id_acl_manager, mi);
        if (last_committed_op_id_acl_manager < mi.committed_op_id)
        {
            last_committed_op_id_acl_manager = mi.committed_op_id;
            //log.trace ("acl_check_for_reload #2, last_committed_op_id_acl_manager=%d", last_committed_op_id_acl_manager);
            return true;
        }
        return false;
        //return _check_for_reload(acl_local_time_check, acl_local_count, &get_acl_manager_op_id, load);
    }
/*
    public bool _check_for_reload(ref long local_time_check, ref long local_count, long function() get_now_count, void delegate() load)
    {
        long now = Clock.currStdTime() / 10000000;

   //        log.trace ("@ft_check_for_reload: #1");

        if (now - local_time_check > _timeout)
        {
            long count_now = get_now_count();
            //log.trace("@_check_for_reload:count_now=%d, local_count=%d", count_now, local_count);

            local_time_check = now;
            if (count_now > local_count)
            {
                //log.trace("__check_for_reload:execute reload");
                local_count = count_now;
                load();
                return true;
            }
        }
        return false;
    }
 */
    // *************************************************** external api *********************************** //

    // /////////////////////////////////////////////////////// TICKET //////////////////////////////////////////////

    public bool is_ticket_valid(string ticket_id)
    {
        //StopWatch sw; sw.start;

        try
        {
//        writeln("@is_ticket_valid, ", ticket_id);
            Ticket *ticket = get_ticket(ticket_id);

            if (ticket is null)
            {
                return false;
            }

            SysTime now = Clock.currTime();
            if (now.stdTime < ticket.end_time)
                return true;

            return false;
        }
        finally
        {
//            stat(CMD_GET, sw);
        }
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

        // store ticket
        string ss_as_cbor = individual2cbor(&new_ticket);

        version (isServer)
        {
            long       op_id;
            ResultCode rc = storage_module.put(P_MODULE.ticket_manager, null, type, new_ticket.uri, null, ss_as_cbor, -1, null, false, op_id);
            ticket.result = rc;

            if (rc == ResultCode.OK)
            {
                subject2Ticket(new_ticket, &ticket);
                user_of_ticket[ ticket.id ] = new Ticket(ticket);
            }

            if (trace_msg[ T_API_50 ] == 1)
                log.trace("create_new_ticket, new ticket=%s", ticket);
        }

        return ticket;
    }

    Ticket get_ticket_trusted(string tr_ticket_id, string login)
    {
        Ticket ticket;

        //if (trace_msg[ T_API_60 ] == 1)
        log.trace("trusted authenticate, ticket=[%s] login=[%s]", ticket, login);

        ticket.result = ResultCode.Authentication_Failed;

        if (login == null || login.length < 1 || tr_ticket_id.length < 6)
        {
            log.trace("WARN: trusted authenticate: invalid login [%s] or ticket [%s]", login, ticket);
            return ticket;
        }

        Ticket *tr_ticket = get_ticket(tr_ticket_id);
        if (tr_ticket.result == ResultCode.OK)
        {
            bool is_superadmin = false;

            void trace(string resource_group, string subject_group, string right)
            {
                if (subject_group == "cfg:SuperUser")
                    is_superadmin = true;
            }

            get_rights_origin(tr_ticket, "cfg:SuperUser", &trace);

            if (is_superadmin)
            {
                login = replaceAll(login, regex(r"[-]", "g"), " +");

                Ticket       sticket         = sys_ticket;
                Individual[] candidate_users = get_individuals_via_query(&sticket, "'" ~ veda_schema__login ~ "' == '" ~ login ~ "'");
                foreach (user; candidate_users)
                {
                    string user_id = user.getFirstResource(veda_schema__owner).uri;
                    if (user_id is null)
                        continue;

                    ticket = create_new_ticket(user_id);

                    return ticket;
                }
            }
        }
        else
            log.trace("WARN: trusted authenticate: problem ticket [%s]", ticket);

        log.trace("failed trusted authenticate, ticket=[%s] login=[%s]", tr_ticket_id, login);

        ticket.result = ResultCode.Authentication_Failed;
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

            Ticket       sticket         = sys_ticket;
            Individual[] candidate_users = get_individuals_via_query(&sticket, "'" ~ veda_schema__login ~ "' == '" ~ login ~ "'");
            foreach (user; candidate_users)
            {
                string user_id = user.getFirstResource(veda_schema__owner).uri;
                if (user_id is null)
                    continue;

                Resources pass = user.resources.get(veda_schema__password, _empty_Resources);
                if (pass.length > 0 && pass[ 0 ] == password)
                {
                    ticket = create_new_ticket(user_id);
                    return ticket;
                }
            }

            log.trace("fail authenticate, login=[%s] password=[%s]", login, password);

            ticket.result = ResultCode.Authentication_Failed;

            return ticket;
        }
        finally
        {
            stat(CMD_PUT, sw);
        }
    }

    public string get_ticket_from_storage(string ticket_id)
    {
        return tickets_storage_r.find(ticket_id);
    }

    public Ticket *get_systicket_from_storage()
    {
        string systicket_id = tickets_storage_r.find("systicket");

        if (systicket_id is null)
            log.trace("SYSTICKET NOT FOUND");

        return get_ticket(systicket_id);
    }

    public Ticket *get_ticket(string ticket_id)
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

                MInfo  mi = get_info(P_MODULE.ticket_manager);

                //log.trace ("last_ticket_manager_op_id=%d, mi.op_id=%d,  mi.committed_op_id=%d", last_ticket_manager_op_id, mi.op_id, mi.committed_op_id);
                if (last_ticket_manager_op_id < mi.op_id)
                {
                    last_ticket_manager_op_id = mi.op_id;
                    this.reopen_ro_ticket_manager_db();
                }

                string ticket_str = tickets_storage_r.find(ticket_id);
                if (ticket_str !is null && ticket_str.length > 120)
                {
                    tt = new Ticket;
                    Individual ticket;

                    if (cbor2individual(&ticket, ticket_str) > 0)
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
                if (now.stdTime >= tt.end_time)
                {
                    if (trace_msg[ T_API_110 ] == 1)
                        log.trace("тикет просрочен, id=%s", ticket_id);

                    if (ticket_id == "guest")
                    {
                        Ticket guest_ticket = create_new_ticket("cfg:Guest", "4000000", "guest");
                        tt = &guest_ticket;
                    }
                    else
                    {
                        tt        = new Ticket;
                        tt.result = ResultCode.Ticket_expired;
                    }
                    return tt;
                }
                else
                {
                    tt.result = ResultCode.OK;
                }

                if (trace_msg[ T_API_120 ] == 1)
                    log.trace("тикет, %s", *tt);
            }
            return tt;
        }
        finally
        {
            //stat(CMD_GET, sw);
        }
    }


    // //////////////////////////////////////////// INDIVIDUALS IO /////////////////////////////////////
    public Individual[] get_individuals_via_query(Ticket *ticket, string query_str, bool inner_get = false, int top = 10, int limit = 10000)
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

            _vql.get(ticket, query_str, null, null, top, limit, res, inner_get);
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
//        try
//        {
//            if (getTid(P_MODULE.ticket_manager) != Tid.init)
//                this.wait_operation_complete(P_MODULE.ticket_manager, 0);
//        }
//        catch (Exception ex) {}

        if (tickets_storage_r !is null)
            tickets_storage_r.reopen_db();
    }

    public void reopen_ro_fulltext_indexer_db()
    {
        if (_vql !is null)
            _vql.reopen_db();
    }

    public void reopen_ro_subject_storage_db()
    {
//        try
//        {
//            if (getTid(P_MODULE.subject_manager) != Tid.init)
//                this.wait_operation_complete(P_MODULE.subject_manager, 0);
//        }
//        catch (Exception ex) {}

        if (inividuals_storage_r !is null)
            inividuals_storage_r.reopen_db();
    }

    public void reopen_ro_acl_storage_db()
    {
//        try
//        {
//            if (getTid(P_MODULE.acl_preparer) != Tid.init)
//                this.wait_operation_complete(P_MODULE.acl_preparer, 0);
//        }
//        catch (Exception ex) {}

        if (acl_indexes !is null)
            acl_indexes.reopen_db();
        //log.trace ("reopen_ro_acl_storage_db");
    }

    // ////////// external ////////////

    public ubyte get_rights(Ticket *ticket, string uri)
    {
        return acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, this, true);
    }

    public void get_rights_origin(Ticket *ticket, string uri,
                                  void delegate(string resource_group, string subject_group, string right) trace)
    {
        acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, this, true, trace);
    }

    public immutable(string)[] get_individuals_ids_via_query(Ticket * ticket, string query_str, string sort_str, string db_str, int top, int limit)
    {
        //StopWatch sw; sw.start;

        try
        {
            if (query_str.indexOf("==") > 0 || query_str.indexOf("&&") > 0 || query_str.indexOf("||") > 0)
            {
            }
            else
            {
                query_str = "'*' == '" ~ query_str ~ "'";
            }

            immutable(string)[] res;
            _vql.get(ticket, query_str, sort_str, db_str, top, limit, res);
            return res;
        }
        finally
        {
//            stat(CMD_GET, sw);
        }
    }

    public Individual get_individual(Ticket *ticket, string uri)
    {
        //       StopWatch sw; sw.start;

        if (trace_msg[ T_API_150 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individual, uri=%s, ticket=%s", uri, ticket.id);
            else
                log.trace("get_individual, uri=%s, ticket=null", uri);
        }

        try
        {
            Individual individual = Individual.init;

            if (acl_indexes.authorize(uri, ticket, Access.can_read, this, true) == Access.can_read)
            {
                string individual_as_cbor = get_from_individual_storage(uri);

                if (individual_as_cbor !is null && individual_as_cbor.length > 1)
                {
                    if (cbor2individual(&individual, individual_as_cbor) > 0)
                        individual.setStatus(ResultCode.OK);
                    else
                    {
                        individual.setStatus(ResultCode.Unprocessable_Entity);
                        writeln("ERR!: invalid cbor: [", individual_as_cbor, "] ", uri);
                    }
                }
                else
                {
                    individual.setStatus(ResultCode.Unprocessable_Entity);
                    //writeln ("ERR!: empty cbor: [", individual_as_cbor, "] ", uri);
                }
            }
            else
            {
                if (trace_msg[ T_API_160 ] == 1)
                    log.trace("get_individual, not authorized, uri=%s", uri);
                individual.setStatus(ResultCode.Not_Authorized);
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
                if (acl_indexes.authorize(uri, ticket, Access.can_read, this, true) == Access.can_read)
                {
                    Individual individual         = Individual.init;
                    string     individual_as_cbor = get_from_individual_storage(uri);

                    if (individual_as_cbor !is null && individual_as_cbor.length > 1)
                    {
                        if (cbor2individual(&individual, individual_as_cbor) > 0)
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
            stat(CMD_GET, sw);
        }
    }

    public string get_individual_as_cbor(Ticket *ticket, string uri, out ResultCode rs)
    {
        string    res;
        StopWatch sw; sw.start;

        rs = ResultCode.Unprocessable_Entity;


        if (trace_msg[ T_API_180 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individual as cbor, uri=%s, ticket=%s", uri, ticket.id);
            else
                log.trace("get_individual as cbor, uri=%s, ticket=null", uri);
        }

        try
        {
            if (acl_indexes.authorize(uri, ticket, Access.can_read, this, true) == Access.can_read)
            {
                string individual_as_cbor = get_from_individual_storage(uri);

                if (individual_as_cbor !is null && individual_as_cbor.length > 1)
                {
                    res = individual_as_cbor;
                    rs  = ResultCode.OK;
                }
                else
                {
                    //writeln ("ERR!: empty cbor: ", uri);
                }
            }
            else
            {
                if (trace_msg[ T_API_190 ] == 1)
                    log.trace("get_individual as cbor, not authorized, uri=%s", uri);
                rs = ResultCode.Not_Authorized;
            }

            return res;
        }
        finally
        {
            stat(CMD_GET, sw);
            if (trace_msg[ T_API_200 ] == 1)
                log.trace("get_individual as cbor: end, uri=%s", uri);
        }
    }

    static const byte NEW_TYPE    = 0;
    static const byte EXISTS_TYPE = 1;

    private OpResult _remove_individual(Ticket *ticket, string uri, bool prepare_events, string event_id, bool ignore_freeze)
    {
        OpResult res = OpResult(ResultCode.Fail_Store, -1);

        try
        {
            EVENT      ev = EVENT.REMOVE;

            string     prev_state;
            Individual indv;
            Individual prev_indv;

            prev_state = get_from_individual_storage_thread(uri);
            if (prev_state !is null)
            {
                int code = cbor2individual(&prev_indv, prev_state);
                if (code < 0)
                {
                    log.trace("ERR! store_individual: invalid prev_state [%s]", prev_state);
                    res.result = ResultCode.Unprocessable_Entity;
                    return res;
                }
            }

            version (isServer)
            {
                res.result = storage_module.remove(P_MODULE.subject_manager, uri, ignore_freeze, res.op_id);
                //veda.core.threads.xapian_indexer.send_delete(null, prev_state, res.op_id);
            }
            if (main_module_url !is null)
            {
                //writeln("context:store_individual #3 ", process_name);
            }
            else
            {
                Resources   _types = prev_indv.resources.get(rdf__type, Resources.init);
                MapResource rdfType;
                setMapResources(_types, rdfType);

                if (rdfType.anyExists(owl_tags) == true)
                {
                    // изменения в онтологии, послать в interthread сигнал о необходимости перезагрузки (context) онтологии
                    inc_count_onto_update();
                }
            }
            //veda.core.fanout.send_put(this, null, prev_state, res.op_id);

            return res;
        }
        finally
        {
            if (res.result != ResultCode.OK)
                log.trace("ERR! no remove subject :uri=%s, errcode=[%s], ticket=[%s]",
                          uri, text(res.result), ticket !is null ? text(*ticket) : "null");

            if (trace_msg[ T_API_210 ] == 1)
                log.trace("[%s] remove_individual [%s] uri = %s", name, uri, res);

//            stat(CMD_PUT, sw);
        }
    }

    public void subject_storage_commmit(bool isWait = true)
    {
        version (isServer)
        {
            storage_module.flush_int_module(P_MODULE.subject_manager, isWait);
        }
    }

    public long unload_subject_storage(string queue_id)
    {
        long res = -1;

        version (isServer)
        {
            res = storage_module.unload(P_MODULE.subject_manager, queue_id);
        }

        if (main_module_url !is null)
        {
            //writeln("context:store_individual #3 ", process_name);
        }

        return res;
    }

    private OpResult store_individual(INDV_OP cmd, Ticket *ticket, Individual *indv, bool prepare_events, string event_id, bool ignore_freeze,
                                      bool is_api_request)
    {
        //if (trace_msg[ T_API_230 ] == 1)
        //log.trace("[%s] store_individual: %s %s", name, text(cmd), *indv);

        StopWatch sw; sw.start;

        OpResult  res = OpResult(ResultCode.Fail_Store, -1);

        try
        {
            if (indv !is null && (indv.uri is null || indv.uri.length < 2))
            {
                res.result = ResultCode.Invalid_Identifier;
                return res;
            }
            if (indv is null || indv.resources.length == 0)
            {
                res.result = ResultCode.No_Content;
                return res;
            }

            //log.trace ("context:store_individual #2 main_module_url=%s", main_module_url);

            version (isModule)
            {
                //log.trace("[%s] store_individual: isModule", name);

                string scmd;

                if (cmd == INDV_OP.PUT)
                    scmd = "put";
                else if (cmd == INDV_OP.ADD_IN)
                    scmd = "add_to";
                else if (cmd == INDV_OP.SET_IN)
                    scmd = "set_in";
                else if (cmd == INDV_OP.REMOVE_FROM)
                    scmd = "remove_from";

                JSONValue req_body;
                req_body[ "function" ]       = scmd;
                req_body[ "ticket" ]         = ticket.id;
                req_body[ "individual" ]     = individual_to_json(*indv);
                req_body[ "prepare_events" ] = prepare_events;
                req_body[ "event_id" ]       = event_id;
                req_body[ "transaction_id" ] = "";

                //log.trace("[%s] store_individual: (isModule), req=(%s)", name, req_body.toString());

                res = reqrep_2_main_module(req_body);
                //log.trace("[%s] store_individual: (isModule), rep=(%s)", name, res);
            }

            //                  writeln("context:store_individual #5 ", process_name);
            version (isServer)
            {
                //log.trace("[%s] store_individual: (isServer)", name);
                Tid       tid_subject_manager;
                Tid       tid_acl;

                Resources _types = indv.resources.get(rdf__type, Resources.init);
                foreach (idx, rs; _types)
                {
                    _types[ idx ].info = NEW_TYPE;
                }

                MapResource rdfType;
                setMapResources(_types, rdfType);

                EVENT      ev = EVENT.CREATE;

                string     prev_state;
                Individual prev_indv;

                try
                {
                    prev_state = get_from_individual_storage_thread(indv.uri);

                    if ((prev_state is null ||
                         prev_state.length == 0) && (cmd == INDV_OP.ADD_IN || cmd == INDV_OP.SET_IN || cmd == INDV_OP.REMOVE_FROM))
                        log.trace("ERR! store_individual: not read prev_state uri=[%s]", indv.uri);
                }
                catch (Exception ex)
                {
                    log.trace("ERR! store_individual: not read prev_state uri=[%s], ex=%s", indv.uri, ex.msg);
                    return res;
                }

                if (prev_state !is null)
                {
                    ev = EVENT.UPDATE;
                    int code = cbor2individual(&prev_indv, prev_state);
                    if (code < 0)
                    {
                        log.trace("ERR! store_individual: invalid prev_state [%s]", prev_state);
                        res.result = ResultCode.Unprocessable_Entity;
                        return res;
                    }

                    if (is_api_request)
                    {
                        // для обновляемого индивида проверим доступность бита Update
                        if (acl_indexes.authorize(indv.uri, ticket, Access.can_update, this, true) != Access.can_update)
                        {
                            res.result = ResultCode.Not_Authorized;
                            return res;
                        }

                        // найдем какие из типов были добавлены по сравнению с предыдущим набором типов
                        foreach (rs; _types)
                        {
                            string   itype = rs.get!string;

                            Resource *rr = rdfType.get(itype, null);

                            if (rr !is null)
                                rr.info = EXISTS_TYPE;
                        }
                    }
                }

                if (is_api_request)
                {
                    // для новых типов проверим доступность бита Create
                    foreach (key, rr; rdfType)
                    {
                        if (rr.info == NEW_TYPE)
                        {
                            if (acl_indexes.authorize(key, ticket, Access.can_create, this, true) != Access.can_create)
                            {
                                res.result = ResultCode.Not_Authorized;
                                return res;
                            }
                        }
                    }
                }

                if (cmd == INDV_OP.ADD_IN || cmd == INDV_OP.SET_IN || cmd == INDV_OP.REMOVE_FROM)
                {
                    //log.trace("[%s] ++ store_individual, prev_indv: %s", name, prev_indv);
                    indv = indv_apply_cmd(cmd, &prev_indv, indv);
                    //log.trace("[%s] ++ store_individual, final indv: %s", name, *indv);
                }

                long update_counter = prev_indv.getFirstInteger("v-s:updateCounter", 0);
                update_counter++;
                indv.setResources("v-s:updateCounter", [ Resource(update_counter) ]);

                string new_state = individual2cbor(indv);

                if (new_state.length > max_size_of_individual)
                {
                    res.result = ResultCode.Size_too_large;
                    return res;
                }

                res.result =
                    storage_module.put(P_MODULE.subject_manager, ticket.user_uri, _types, indv.uri, prev_state, new_state, update_counter, event_id,
                                       ignore_freeze,
                                       res.op_id);
                //log.trace("res.result=%s", res.result);

                if (res.result != ResultCode.OK)
                    return res;

                if (ev == EVENT.CREATE || ev == EVENT.UPDATE)
                {
                    if (rdfType.anyExists(owl_tags) == true && new_state != prev_state)
                    {
                        // изменения в онтологии, послать в interthread сигнал о необходимости перезагрузки (context) онтологии
                        inc_count_onto_update();
                    }

                    if (rdfType.anyExists(veda_schema__PermissionStatement) == true || rdfType.anyExists(veda_schema__Membership) == true)
                    {
                        tid_acl = getTid(P_MODULE.acl_preparer);
                        if (tid_acl != Tid.init)
                        {
                            send(tid_acl, CMD_PUT, ev, prev_state, new_state, res.op_id);
                        }
                    }

/*
                        version (libV8)
                        {
                            if (rdfType.anyExists("v-s:ExecuteScript"))
                            {
                                // передать вызов отдельной нити по выполнению Long Time Run Scripts
                                veda.core.glue_code.ltrs.execute_script(new_state);
                            }
                        }
 */
//                    if (event_id != "fanout")
/*
                        version (WebServer)
                        {
                            veda.core.threads.dcs_manager.ev_update_individual(cmd, ticket.user_uri, indv.uri, new_state, prev_state, event_id,
                                                                               res.op_id,
                                                                               update_counter);
                        }
 */
                    res.result = ResultCode.OK;
                }
                else
                {
                    res.result = ResultCode.Internal_Server_Error;
                }
            }
            return res;
        }
        finally
        {
            if (res.result != ResultCode.OK)
                log.trace("ERR! no store subject :%s, errcode=[%s], ticket=[%s]",
                          indv !is null ? text(*indv) : "null",
                          text(res.result), ticket !is null ? text(*ticket) : "null");

            if (trace_msg[ T_API_240 ] == 1)
                log.trace("[%s] store_individual [%s] = %s", name, indv.uri, res);

            stat(CMD_PUT, sw);
        }
    }

    public OpResult put_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, bool ignore_freeze = false,
                                   bool is_api_request = true)
    {
        individual.uri = uri;
        return store_individual(INDV_OP.PUT, ticket, &individual, prepareEvents, event_id, ignore_freeze, is_api_request);
    }

    public OpResult remove_individual(Ticket *ticket, string uri, bool prepareEvents, string event_id, bool ignore_freeze, bool is_api_request = true)
    {
        return _remove_individual(ticket, uri, prepareEvents, event_id, ignore_freeze);
    }

    public OpResult add_to_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, bool ignore_freeze =
                                          false, bool is_api_request = true)
    {
        individual.uri = uri;
        return store_individual(INDV_OP.ADD_IN, ticket, &individual, prepareEvents, event_id, ignore_freeze, is_api_request);
    }

    public OpResult set_in_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id, bool ignore_freeze =
                                          false, bool is_api_request = true)
    {
        individual.uri = uri;
        return store_individual(INDV_OP.SET_IN, ticket, &individual, prepareEvents, event_id, ignore_freeze, is_api_request);
    }

    public OpResult remove_from_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id,
                                           bool ignore_freeze = false, bool is_api_request = true)
    {
        individual.uri = uri;
        return store_individual(INDV_OP.REMOVE_FROM, ticket, &individual, prepareEvents, event_id, ignore_freeze, is_api_request);
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

    public bool backup(bool to_binlog, int level = 0)
    {
        bool result = false;

        version (isServer)
        {
            if (level == 0)
                freeze();

            Ticket sticket = sys_ticket();

            try
            {
                string backup_id = "to_binlog";

                if (to_binlog)
                {
                    long count = this.inividuals_storage_r.dump_to_binlog();
                    if (count > 0)
                        result = true;
                }
                else
                {
                    backup_id = storage_module.backup(P_MODULE.subject_manager);

                    if (backup_id != "")
                    {
                        result = true;

                        string res; // = veda.core.threads.acl_manager.backup(backup_id);

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
                            return backup(to_binlog, level + 1);
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
        }
        return result;
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
        version (isServer)
        {
            storage_module.freeze(P_MODULE.subject_manager);
        }
        version (isModule)
        {
            JSONValue req_body;
            req_body[ "function" ] = "freeze";
            OpResult  res = reqrep_2_main_module(req_body);
        }
    }

    public void unfreeze()
    {
        version (isServer)
        {
            storage_module.unfreeze(P_MODULE.subject_manager);
        }
        version (isModule)
        {
            JSONValue req_body;
            req_body[ "function" ] = "unfreeze";
            OpResult  res = reqrep_2_main_module(req_body);
        }
    }

    private string get_from_individual_storage_thread(string uri)
    {
        string res;

        version (isServer)
        {
            res = storage_module.find(P_MODULE.subject_manager, uri);
        }
        return res;
    }

    //////////////////////////////////////////////// MODULES INTERACTION

    private ModuleInfoFile[ P_MODULE ] info_r__2__pmodule;
    private MInfo get_info(P_MODULE module_id)
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


    public long get_operation_state(P_MODULE module_id, long wait_op_id)
    {
        long res = -1;

        version (isServer)
        {
            if (module_id == P_MODULE.acl_preparer)
            {
                return get_acl_manager_op_id;
            }
            else if (module_id == P_MODULE.subject_manager)
            {
                return get_subject_manager_op_id;
            }
        }

        MInfo info = get_info(module_id);

        if (info.is_Ok)
        {
            if (module_id == P_MODULE.fulltext_indexer || module_id == P_MODULE.scripts)
                res = info.committed_op_id;
            else
                res = info.op_id;
        }

        version (isModule)
        {
            if (module_id == P_MODULE.subject_manager)
                this.reopen_ro_subject_storage_db();

            if (module_id == P_MODULE.acl_preparer)
                this.reopen_ro_acl_storage_db();

            if (module_id == P_MODULE.fulltext_indexer)
                this.reopen_ro_fulltext_indexer_db();
        }

        version (WebServer)
        {
            if (module_id == P_MODULE.subject_manager)
                this.reopen_ro_subject_storage_db();

            if (module_id == P_MODULE.acl_preparer)
                this.reopen_ro_acl_storage_db();

            if (module_id == P_MODULE.fulltext_indexer)
                this.reopen_ro_fulltext_indexer_db();
        }

        log.trace("get_operation_state(%s) res=%s, wait_op_id=%d", text(module_id), info, wait_op_id);

        return res;
    }

    public long restart_module(P_MODULE module_id)
    {
        return 0;
    }


    public bool wait_operation_complete(P_MODULE module_id, long op_id, long timeout = 10_000)
    {
        if (module_id == id)
            return false;

        version (isServer)
        {
            if (module_id == P_MODULE.scripts || module_id == P_MODULE.fulltext_indexer || module_id == P_MODULE.fanout_email ||
                module_id == P_MODULE.ltr_scripts || module_id == P_MODULE.fanout_sql)
            {
                return wait_module(module_id, op_id, timeout);
            }
            else
            {
                Tid tid = getTid(module_id);
                if (tid != Tid.init)
                {
                    send(tid, CMD_NOP, thisTid);
                    //                receiveTimeout(1000.msecs, (bool res) {});
                    receive((bool res) {});
                }
            }
        }
        return true;
    }

    private bool wait_module(P_MODULE pm, long wait_op_id, long timeout)
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

    version (isServer)
    {
        public string execute(string in_msg)
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

            JSONValue fn = jsn[ "function" ];

            string    sfn = fn.str();

            if (sfn == "authenticate")
            {
                JSONValue login    = jsn[ "login" ];
                JSONValue password = jsn[ "password" ];

                Ticket    ticket = this.authenticate(login.str, password.str);

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

                Ticket    ticket = this.get_ticket_trusted(ticket_id.str, login.str);

                res[ "type" ]     = "ticket";
                res[ "id" ]       = ticket.id;
                res[ "user_uri" ] = ticket.user_uri;
                res[ "result" ]   = ticket.result;
                res[ "end_time" ] = ticket.end_time;
            }
            else if (sfn == "put" || sfn == "remove" || sfn == "add_to" || sfn == "set_in" || sfn == "remove_from")
            {
                OpResult  rc;

                JSONValue _ticket         = jsn[ "ticket" ];
                JSONValue jprepare_events = jsn[ "prepare_events" ];

                bool      prepare_events;
                if (jprepare_events.type() == JSON_TYPE.TRUE)
                    prepare_events = true;

                JSONValue event_id       = jsn[ "event_id" ];
                JSONValue transaction_id = jsn[ "transaction_id" ];

                Ticket    *ticket = this.get_ticket(_ticket.str);

                if (sfn == "put")
                {
                    JSONValue  individual_json = jsn[ "individual" ];
                    Individual individual      = json_to_individual(individual_json);
                    rc = this.put_individual(ticket, individual.uri, individual, prepare_events, event_id.str, false, true);
                }
                else if (sfn == "add_to")
                {
                    JSONValue  individual_json = jsn[ "individual" ];
                    Individual individual      = json_to_individual(individual_json);
                    rc = this.add_to_individual(ticket, individual.uri, individual, prepare_events, event_id.str, false, true);
                }
                else if (sfn == "set_in")
                {
                    JSONValue  individual_json = jsn[ "individual" ];
                    Individual individual      = json_to_individual(individual_json);
                    rc = this.set_in_individual(ticket, individual.uri, individual, prepare_events, event_id.str, false, true);
                }
                else if (sfn == "remove_from")
                {
                    JSONValue  individual_json = jsn[ "individual" ];
                    Individual individual      = json_to_individual(individual_json);
                    rc = this.remove_from_individual(ticket, individual.uri, individual, prepare_events, event_id.str, false, true);
                }
                else if (sfn == "remove")
                {
                    JSONValue uri = jsn[ "uri" ];
                    rc = this.remove_individual(ticket, uri.str, prepare_events, event_id.str, false, true);
                }

                res[ "type" ]   = "OpResult";
                res[ "result" ] = rc.result;
                res[ "op_id" ]  = rc.op_id;
            }
            else if (sfn == "flush")
            {
                P_MODULE   f_module_id = cast(P_MODULE)jsn[ "module_id" ].integer;
                long       wait_op_id  = jsn[ "wait_op_id" ].integer;

                ResultCode rc;

                if (f_module_id == P_MODULE.subject_manager)
                    rc = storage_module.flush_int_module(P_MODULE.subject_manager, false);
                else if (f_module_id == P_MODULE.acl_preparer)
                    rc = acl_module.flush(false);
                else if (f_module_id == P_MODULE.fulltext_indexer)
                    storage_module.flush_ext_module(f_module_id, wait_op_id);

                res[ "type" ]   = "OpResult";
                res[ "result" ] = ResultCode.OK;
                res[ "op_id" ]  = -1;
            }
            else if (sfn == "send_to_module")
            {
                P_MODULE   f_module_id = cast(P_MODULE)jsn[ "module_id" ].integer;
                string     msg         = jsn[ "msg" ].str;

                ResultCode rc;

                storage_module.msg_to_module(f_module_id, msg, false);

                res[ "type" ]   = "OpResult";
                res[ "result" ] = ResultCode.OK;
                res[ "op_id" ]  = -1;
            }
            else if (sfn == "backup")
            {
                bool to_binlog = jsn[ "to_binlog" ].type() == JSON_TYPE.TRUE;
                bool rc        = this.backup(to_binlog, 0);

                res[ "type" ] = "OpResult";
                if (rc == true)
                    res[ "result" ] = ResultCode.OK;
                else
                    res[ "result" ] = ResultCode.Internal_Server_Error;
                res[ "op_id" ] = -1;
            }
            else if (sfn == "freeze")
            {
                this.freeze();
                res[ "type" ]   = "OpResult";
                res[ "result" ] = ResultCode.OK;
                res[ "op_id" ]  = -1;
            }
            else if (sfn == "unfreeze")
            {
                this.unfreeze();
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
    }
}
