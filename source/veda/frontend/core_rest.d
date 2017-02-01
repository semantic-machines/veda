module veda.frontend.core_rest;

import std.stdio, std.datetime, std.conv, std.string, std.datetime, std.file, core.runtime, core.thread, core.sys.posix.signal, std.uuid;
import core.vararg, core.stdc.stdarg, core.atomic;
import vibe.d, vibe.core.core, vibe.core.log, vibe.core.task, vibe.inet.mimetypes;
import properd, TrailDB;
import veda.common.type, veda.core.common.context, veda.core.common.know_predicates, veda.core.common.define, veda.core.common.log_msg;
import veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.frontend.individual8vjson;
import veda.frontend.cbor8vjson;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
veda.common.logger.Logger _log;
veda.common.logger.Logger log()
{
    if (_log is null)
        _log = new veda.common.logger.Logger("veda-core-webserver-" ~ text(http_port), "log", "REST");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

short               http_port = 8080;

public const string veda_schema__File          = "v-s:File";
public const string veda_schema__fileName      = "v-s:fileName";
public const string veda_schema__fileSize      = "v-s:fileSize";
public const string veda_schema__fileThumbnail = "v-s:fileThumbnail";

static this() {
    Lang =
    [
        "NONE":LANG.NONE, "none":LANG.NONE,
        "RU":LANG.RU, "ru":LANG.RU,
        "EN":LANG.EN, "en":LANG.EN
    ];

    Resource_type =
    [
        "Uri":DataType.Uri,
        "String":DataType.String,
        "Integer":DataType.Integer,
        "Datetime":DataType.Datetime,
        "Decimal":DataType.Decimal,
        "Boolean":DataType.Boolean,
    ];

    try
    {
        mkdir(attachments_db_path);
    }
    catch (Exception ex)
    {
    }
}

//////////////////////////////////////////////////// Rest API /////////////////////////////////////////////////////////////////

interface VedaStorageRest_API {
    /**
     * получить для индивида список прав на ресурс.
     * Params: ticket = указывает на индивида
     *	       uri    = uri ресурса
     * Returns: JSON
     */
    @path("get_rights") @method(HTTPMethod.GET)
    Json get_rights(string ticket, string uri);

    /**
     * получить для индивида детализированный список прав на ресурс.
     * Params: ticket = указывает на индивида
     *	       uri    = uri ресурса
     * Returns: JSON
     */
    @path("get_rights_origin") @method(HTTPMethod.GET)
    Json[] get_rights_origin(string ticket, string uri);

    @path("get_membership") @method(HTTPMethod.GET)
    Json get_membership(string ticket, string uri);

    @path("authenticate") @method(HTTPMethod.GET)
    Ticket authenticate(string login, string password);

    @path("get_ticket_trusted") @method(HTTPMethod.GET)
    Ticket get_ticket_trusted(string ticket, string login);

    @path("is_ticket_valid") @method(HTTPMethod.GET)
    bool is_ticket_valid(string ticket);

    @path("get_operation_state") @method(HTTPMethod.GET)
    long get_operation_state(int module_id, long wait_op_id);

    @path("send_to_module") @method(HTTPMethod.GET)
    void send_to_module(int module_id, string msg);

    @path("flush") @method(HTTPMethod.GET)
    void flush(int module_id, long wait_op_id);

    @path("set_trace") @method(HTTPMethod.GET)
    void set_trace(int idx, bool state);

    @path("backup") @method(HTTPMethod.GET)
    void backup(bool to_binlog);

    @path("count_individuals") @method(HTTPMethod.GET)
    long count_individuals();

    @path("query") @method(HTTPMethod.GET)
    SearchResult query(string ticket, string query, string sort = null, string databases = null, bool reopen = false, int from = 0, int top = 10000,
                       int limit = 10000);

    @path("get_individuals") @method(HTTPMethod.POST)
    Json[] get_individuals(string ticket, string[] uris);

    @path("get_individual") @method(HTTPMethod.GET)
    Json get_individual(string ticket, string uri, bool reopen = false);

    @path("put_individual") @method(HTTPMethod.PUT)
    OpResult put_individual(string ticket, Json individual, bool prepare_events, string event_id, string transaction_id);

    @path("remove_individual") @method(HTTPMethod.PUT)
    OpResult remove_individual(string ticket, string uri, bool prepare_events, string event_id, string transaction_id);

    @path("remove_from_individual") @method(HTTPMethod.PUT)
    OpResult remove_from_individual(string ticket, Json individual, bool prepare_events, string event_id, string transaction_id);

    @path("set_in_individual") @method(HTTPMethod.PUT)
    OpResult set_in_individual(string ticket, Json individual, bool prepare_events, string event_id, string transaction_id);

    @path("add_to_individual") @method(HTTPMethod.PUT)
    OpResult add_to_individual(string ticket, Json individual, bool prepare_events, string event_id, string transaction_id);

    @path("begin_transaction") @method(HTTPMethod.PUT)
    string begin_transaction();

    @path("commit_transaction") @method(HTTPMethod.PUT)
    void commit_transaction(string transaction_id);

    @path("abort_transaction") @method(HTTPMethod.PUT)
    void abort_transaction(string transaction_id);

    //void trail(string ticket_id, string user_id, string action, Json args, string result, ResultCode result_code, int duration);
}

extern (C) void handleTerminationR(int _signal)
{
//    log.trace("!SYS: veda.app: caught signal: %s", text(_signal));

    if (tdb_cons !is null)
    {
        log.trace("flush trail db");
        tdb_cons.finalize();
    }

    writefln("!SYS: veda.app.rest: caught signal: %s", text(_signal));

    vibe.core.core.exitEventLoop();

    writeln("!SYS: veda.app: exit");

    thread_term();
    Runtime.terminate();

//    kill(getpid(), SIGKILL);
//    exit(_signal);
}


class VedaStorageRest : VedaStorageRest_API
{
    private Context context;
//    string[ string ] properties;
    int             last_used_tid = 0;
    void function(int sig) shutdown;

    this(Context _local_context, void function(int sig) _shutdown)
    {
        bsd_signal(SIGINT, &handleTerminationR);
        shutdown = _shutdown;
        context  = _local_context;
    }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

    void fileManager(HTTPServerRequest req, HTTPServerResponse res)
    {
        //log.trace ("@v req.path=%s", req.path);

        string uri;
        // uri субьекта

        long pos = lastIndexOf(req.path, "/");

        if (pos > 0)
        {
            uri = req.path[ pos + 1..$ ];
        }
        else
            return;

        // найдем в хранилище указанного субьекта

        //log.trace("@v uri=%s", uri);
        auto   ticket_ff = "ticket" in req.query;
        string _ticket;

        if (ticket_ff !is null)
            _ticket = cast(string)*ticket_ff;
        else
            _ticket = req.cookies.get("ticket", "");

        //log.trace("@v ticket=%s", _ticket);

        if (uri.length > 3 && _ticket !is null)
        {
            Ticket     *ticket = context.get_ticket(_ticket);

            Individual file_info;

            ResultCode rc = ticket.result;
            if (rc == ResultCode.OK)
            {
                file_info = context.get_individual(ticket, uri);

                //log.trace("@v file_info=%s", file_info);
                auto fileServerSettings = new HTTPFileServerSettings;
                fileServerSettings.encodingFileExtension = [ "jpeg":".JPG" ];

                string path     = file_info.getFirstResource("v-s:filePath").get!string;
                string file_uri = file_info.getFirstResource("v-s:fileUri").get!string;

                if (path !is null && file_uri !is null && file_uri.length > 0)
                {
                    if (path.length > 0)
                        path = path ~ "/";

                    string full_path = attachments_db_path ~ "/" ~ path ~ file_uri;

                    enforce(exists(full_path), "No file found!");

                    HTTPServerRequestDelegate dg =
                        serveStaticFile(full_path, fileServerSettings);

                    string originFileName = file_info.getFirstResource(veda_schema__fileName).get!string;

                    //log.trace("@v originFileName=%s", originFileName);
                    //log.trace("@v getMimeTypeForFile(originFileName)=%s", getMimeTypeForFile(originFileName));

                    res.headers[ "Content-Disposition" ] = "attachment; filename=\"" ~ originFileName ~ "\"";

                    res.contentType = getMimeTypeForFile(originFileName);
                    dg(req, res);
                }
                else
                {
                    log.trace("ERR! get_file:incomplete individual of v-s:File, It does not contain predicate v-s:filePath or v-s:fileUri: %s",
                              file_info);
                }
            }
        }
    }

    override :

    Json get_rights(string _ticket, string uri)
    {
        ResultCode rc;
        ubyte      right_res;
        Json       res;
        Ticket     *ticket;

        try
        {
            ticket = context.get_ticket(_ticket);

            if (ticket.result != ResultCode.OK)
                throw new HTTPStatusException(ticket.result);

            right_res = context.get_rights(ticket, uri);

            Individual indv_res;
            indv_res.uri = "_";

            indv_res.addResource(rdf__type, Resource(DataType.Uri, veda_schema__PermissionStatement));

            if ((right_res & Access.can_read) > 0)
                indv_res.addResource("v-s:canRead", Resource(true));

            if ((right_res & Access.can_update) > 0)
                indv_res.addResource("v-s:canUpdate", Resource(true));

            if ((right_res & Access.can_delete) > 0)
                indv_res.addResource("v-s:canDelete", Resource(true));

            if ((right_res & Access.can_create) > 0)
                indv_res.addResource("v-s:canCreate", Resource(true));

            res = individual_to_json(indv_res);
            return res;
        }
        finally
        {
        }
    }

    Json get_membership(string _ticket, string uri)
    {
        Json       json;

        Ticket     *ticket;
        ResultCode rc;

        try
        {
            ticket = context.get_ticket(_ticket);
            if (ticket.result != ResultCode.OK)
                throw new HTTPStatusException(ticket.result);

            Individual indv_res = Individual.init;
            indv_res.uri = "_";
            indv_res.addResource(rdf__type,
                                 Resource(DataType.Uri, "v-s:Membership"));
            indv_res.addResource("v-s:resource",
                                 Resource(DataType.Uri, uri));

            void trace_group(string resource_group)
            {
                indv_res.addResource("v-s:memberOf",
                                     Resource(DataType.Uri, resource_group));
            }

            context.get_membership_from_acl(ticket, uri, &trace_group);

            json = individual_to_json(indv_res);
            return json;
        }
        finally
        {
        }
    }

    Json[] get_rights_origin(string _ticket, string uri)
    {
        Json[]     json;

        Ticket     *ticket;
        ResultCode rc;

        try
        {
            Individual[] res;

            ticket = context.get_ticket(_ticket);
            if (ticket.result != ResultCode.OK)
                throw new HTTPStatusException(ticket.result);


            void trace_acl(string resource_group, string subject_group, string right)
            {
                Individual indv_res = Individual.init;

                indv_res.uri = "_";
                indv_res.addResource(rdf__type,
                                     Resource(DataType.Uri, veda_schema__PermissionStatement));
                indv_res.addResource(veda_schema__permissionObject,
                                     Resource(DataType.Uri, resource_group));
                indv_res.addResource(veda_schema__permissionSubject,
                                     Resource(DataType.Uri, subject_group));
                indv_res.addResource(right, Resource(true));

                res ~= indv_res;
            }

            context.get_rights_origin_from_acl(ticket, uri, &trace_acl);


            json = Json[].init;
            foreach (individual; res)
                json ~= individual_to_json(individual);

            return json;
        }
        finally
        {
        }
    }

    Ticket authenticate(string login, string password)
    {
        Ticket ticket;

        try
        {
            if (wsc_server_task !is Task.init)
            {
                Json json = Json.emptyObject;
                json[ "function" ] = "authenticate";
                json[ "login" ]    = login;
                json[ "password" ] = password;

                vibe.core.concurrency.send(wsc_server_task, json, Task.getThis());

                string resp;
                vibe.core.concurrency.receive((string res){ resp = res; });

                if (resp is null)
                    throw new HTTPStatusException(ResultCode.Not_Authorized);

                Json jres = parseJson(resp);
                //log.trace("jres '%s'", jres);

                string type_msg = jres[ "type" ].get!string;

                //log.trace("type_msg '%s'", type_msg);

                if (type_msg != "ticket")
                    throw new HTTPStatusException(ResultCode.Not_Authorized);

                ticket.end_time = jres[ "end_time" ].get!long;
                ticket.id       = jres[ "id" ].get!string;
                ticket.user_uri = jres[ "user_uri" ].get!string;
                ticket.result   = cast(ResultCode)jres[ "result" ].get!long;

                //log.trace("new ticket= '%s'", ticket);
            }

//        Ticket ticket = context.authenticate(login, password, &create_new_ticket);

            if (ticket.result != ResultCode.OK)
                throw new HTTPStatusException(ticket.result);

            return ticket;
        }
        finally
        {
        }
    }

    Ticket get_ticket_trusted(string ticket_id, string login)
    {
        Ticket ticket;

        try
        {
            if (wsc_server_task !is Task.init)
            {
                Json json = Json.emptyObject;
                json[ "function" ] = "get_ticket_trusted";
                json[ "ticket" ]   = ticket_id;
                json[ "login" ]    = login;

                vibe.core.concurrency.send(wsc_server_task, json, Task.getThis());

                string resp;
                vibe.core.concurrency.receive((string res){ resp = res; });

                if (resp is null)
                    throw new HTTPStatusException(ResultCode.Not_Authorized);

                Json jres = parseJson(resp);
                //log.trace("jres '%s'", jres);

                string type_msg = jres[ "type" ].get!string;

                //log.trace("type_msg '%s'", type_msg);

                if (type_msg != "ticket")
                    throw new HTTPStatusException(ResultCode.Not_Authorized);

                ticket.end_time = jres[ "end_time" ].get!long;
                ticket.id       = jres[ "id" ].get!string;
                ticket.user_uri = jres[ "user_uri" ].get!string;
                ticket.result   = cast(ResultCode)jres[ "result" ].get!long;

                //log.trace("new ticket= '%s'", ticket);
            }

            if (ticket.result != ResultCode.OK)
                throw new HTTPStatusException(ticket.result, text(ticket.result));

            return ticket;
        }
        finally
        {
        }
    }

    long get_operation_state(int module_id, long wait_op_id)
    {
        long res = -1;

        try
        {
            try
            {
                res = context.get_operation_state(cast(P_MODULE)module_id, wait_op_id);
            }
            catch (Throwable tr)
            {
                log.trace("ERR! REST:get_operation_state, %s \n%s ", tr.msg, tr.info);
            }
        }
        finally
        {
        }

        return res;
    }

    void send_to_module(int module_id, string msg)
    {
        try
        {
            long     res = -1;

            OpResult op_res;

            Json     jreq = Json.emptyObject;

            jreq[ "function" ]  = "send_to_module";
            jreq[ "module_id" ] = module_id;
            jreq[ "msg" ]       = msg;

            vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
            vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

            //log.trace("send:flush #e");
            if (op_res.result != ResultCode.OK)
                throw new HTTPStatusException(op_res.result, text(op_res.result));
        }
        finally
        {
        }
    }

    void flush(int module_id, long wait_op_id)
    {
        ulong    timestamp = Clock.currTime().stdTime() / 10;

        Json     jreq = Json.emptyObject;
        OpResult op_res;

        try
        {
            long res = -1;

            jreq[ "function" ]   = "flush";
            jreq[ "module_id" ]  = module_id;
            jreq[ "wait_op_id" ] = wait_op_id;

            vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
            vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

            if (op_res.result != ResultCode.OK)
                throw new HTTPStatusException(op_res.result, text(op_res.result));
        }
        finally
        {
            trail(null, null, "flush", jreq, "", op_res.result, timestamp);
        }
    }

    void set_trace(int idx, bool state)
    {
        context.set_trace(idx, state);
    }

    void backup(bool to_binlog)
    {
        ulong    timestamp = Clock.currTime().stdTime() / 10;

        long     res  = -1;
        Json     jreq = Json.emptyObject;
        OpResult op_res;

        try
        {
            jreq[ "function" ]  = "backup";
            jreq[ "to_binlog" ] = to_binlog;

            vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
            vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

            //log.trace("send:flush #e");
            if (op_res.result != ResultCode.OK)
                throw new HTTPStatusException(op_res.result, text(op_res.result));

            return;
        }
        finally
        {
            trail(null, null, "backup", jreq, "", op_res.result, timestamp);
        }
    }

    long count_individuals()
    {
        ulong    timestamp = Clock.currTime().stdTime() / 10;
        long     res;

        OpResult op_res;

        res           = context.count_individuals();
        op_res.result = ResultCode.OK;

        trail(null, null, "count_individuals", Json.emptyObject, text(res), op_res.result, timestamp);
        return res;
    }

    bool is_ticket_valid(string ticket_id)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;
        bool       res;
        Ticket     *ticket;
        ResultCode rc;

        try
        {
            ticket = context.get_ticket(ticket_id);
            rc     = ticket.result;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc, text(rc));

            SysTime now = Clock.currTime();
            if (now.stdTime < ticket.end_time)
                res = true;
            else
                res = false;

            return res;
        }
        finally
        {
            trail(ticket_id, ticket.user_uri, "is_ticket_valid", Json.emptyObject, text(res), rc, timestamp);
        }
    }

    SearchResult query(string _ticket, string _query, string sort = null, string databases = null, bool reopen = false, int from = 0, int top = 10000,
                       int limit = 10000)
    {
        ulong        timestamp = Clock.currTime().stdTime() / 10;

        SearchResult sr;
        Ticket       *ticket;
        ResultCode   rc;

        int          count = 0;

        void prepare_element(string uri)
        {
            //           if (count > 100)
//            {
            vibe.core.core.yield();
//                count = 0;
//            }
//            count++;
        }

        try
        {
            ticket = context.get_ticket(_ticket);
            rc     = ticket.result;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc, text(rc));

            sr = context.get_individuals_ids_via_query(ticket, _query, sort, databases, from, top, limit, null); //&prepare_element);

            return sr;
        }
        finally
        {
            Json jreq = Json.emptyObject;
            jreq[ "query" ]     = _query;
            jreq[ "sort" ]      = sort;
            jreq[ "databases" ] = databases;
            jreq[ "reopen" ]    = reopen;
            jreq[ "from" ]      = from;
            jreq[ "top" ]       = top;
            jreq[ "limit" ]     = limit;

            trail(_ticket, ticket.user_uri, "query", jreq, text(sr.result), rc, timestamp);
        }
    }

    Json[] get_individuals(string _ticket, string[] uris)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;

        Json[]     res;
        ResultCode rc;
        Json       args = Json.emptyArray;

        Ticket     *ticket;
        try
        {
            ticket = context.get_ticket(_ticket);
            rc     = ticket.result;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc, text(rc));

            try
            {
                foreach (indv; context.get_individuals(ticket, uris))
                {
                    Json jj = individual_to_json(indv);
                    res ~= jj;
                    args ~= indv.uri;
                }
            }
            catch (Throwable ex)
            {
                throw new HTTPStatusException(ResultCode.Internal_Server_Error);
            }

            return res;
        }
        finally
        {
            Json jreq = Json.emptyObject;
            jreq[ "uris" ] = args;
            trail(_ticket, ticket.user_uri, "get_individuals", jreq, text(res), rc, timestamp);
        }
    }

    Json get_individual(string _ticket, string uri, bool reopen = false)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;

        Json       res;
        ResultCode rc = ResultCode.Internal_Server_Error;
        Ticket     *ticket;

        try
        {
            ticket = context.get_ticket(_ticket);
            rc     = ticket.result;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc, text(rc));

            try
            {
                Individual[ string ] onto_individuals =
                    context.get_onto_as_map_individuals();

                Individual individual = onto_individuals.get(uri, Individual.init);

                if (individual != Individual.init)
                {
                    res = individual_to_json(individual);
                    rc  = ResultCode.OK;
                }
                else
                {
                    if (reopen)
                    {
                        context.reopen_ro_acl_storage_db();
                        context.reopen_ro_subject_storage_db();
                    }

                    string cb = context.get_individual_as_binobj(ticket, uri, rc);

                    if (rc == ResultCode.OK)
                    {
                        res = Json.emptyObject;
                        cbor2json(&res, cb);
                    }
                }
            }
            catch (Throwable ex)
            {
                throw new HTTPStatusException(rc, ex.msg);
            }

            if (rc != ResultCode.OK)
            {
                throw new HTTPStatusException(rc, text(rc));
            }

            return res;
        }
        finally
        {
            Json jreq = Json.emptyObject;
            jreq[ "uri" ] = uri;
            trail(_ticket, ticket.user_uri, "get_individual", jreq, res.toString(), rc, timestamp);
        }
    }

    OpResult remove_individual(string _ticket, string uri, bool prepare_events, string event_id, string transaction_id)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;

        OpResult   op_res;
        ResultCode rc = ResultCode.Internal_Server_Error;
        Ticket     *ticket;
        Json       jreq = Json.emptyObject;

        try
        {
            ticket = context.get_ticket(_ticket);
            rc     = ticket.result;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc, text(rc));

            if (wsc_server_task is Task.init)
            {
                rc = ResultCode.Internal_Server_Error;
                throw new HTTPStatusException(rc, text(rc));
            }

            jreq[ "function" ]       = "remove";
            jreq[ "ticket" ]         = _ticket;
            jreq[ "uri" ]            = uri;
            jreq[ "prepare_events" ] = prepare_events;
            jreq[ "event_id" ]       = event_id;
            jreq[ "transaction_id" ] = transaction_id;

            vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
            vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });
            rc = op_res.result;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc, text(rc));

            return op_res;
        }
        finally
        {
            trail(_ticket, ticket.user_uri, "remove_individual", jreq, "", op_res.result, timestamp);
        }
    }

    OpResult put_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;

        Ticket     *ticket;
        ResultCode rc = ResultCode.Internal_Server_Error;

        ticket = context.get_ticket(_ticket);
        rc     = ticket.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        OpResult op_res = modify_individual(context, "put", _ticket, individual_json, prepare_events, event_id, transaction_id, timestamp);
        rc = op_res.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        return op_res;
    }

    OpResult add_to_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;
        Ticket     *ticket;
        ResultCode rc = ResultCode.Internal_Server_Error;

        ticket = context.get_ticket(_ticket);
        rc     = ticket.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        OpResult op_res = modify_individual(context, "add_to", _ticket, individual_json, prepare_events, event_id, transaction_id, timestamp);
        rc = op_res.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        return op_res;
    }

    OpResult set_in_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;
        Ticket     *ticket;
        ResultCode rc = ResultCode.Internal_Server_Error;

        ticket = context.get_ticket(_ticket);
        rc     = ticket.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        OpResult op_res = modify_individual(context, "set_in", _ticket, individual_json, prepare_events, event_id, transaction_id, timestamp);
        rc = op_res.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        return op_res;
    }

    OpResult remove_from_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        ulong      timestamp = Clock.currTime().stdTime() / 10;
        Ticket     *ticket;
        ResultCode rc = ResultCode.Internal_Server_Error;

        ticket = context.get_ticket(_ticket);
        rc     = ticket.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        OpResult op_res = modify_individual(context, "remove_from", _ticket, individual_json, prepare_events, event_id, transaction_id, timestamp);
        rc = op_res.result;

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        return op_res;
    }

    string begin_transaction()
    {
        return context.begin_transaction();
    }

    void commit_transaction(string transaction_id)
    {
        context.commit_transaction(transaction_id);
    }

    void abort_transaction(string transaction_id)
    {
        context.abort_transaction(transaction_id);
    }
}
//////////////////////////////

private TrailDBConstructor tdb_cons;
private bool               is_trail     = true;
private long               count_trails = 0;
private TrailDB            exist_trail;

void trail(string ticket_id, string user_id, string action, Json args, string result, ResultCode result_code, ulong start_time)
{
    if (!is_trail)
        return;

    try
    {
        ulong timestamp = Clock.currTime().stdTime() / 10;

        if (tdb_cons is null)
        {
            //try
            //{
            //	rename (trails_path ~ "/rest_trails_chunk.tdb", trails_path ~ "/rest_trails.tdb");
            //    exist_trail = new TrailDB(trails_path ~ "/rest_trails.tdb");
            //    //exist_trail.dontneed ();
            //}
            //catch (Throwable tr)
            //{
            //    log.trace("ERR! exist_trail: %s", tr.msg);
            //}

            log.trace("open trail db");

            tdb_cons =
                new TrailDBConstructor(trails_path ~ "/rest_trails_" ~ text(timestamp),
                                       [ "ticket", "user_id", "action", "args", "result", "result_code", "duration" ]);

            //if (exist_trail !is null)
            //{
            //    log.trace("append to exist trail db");
            //    tdb_cons.append(exist_trail);
            //    log.trace("merge is ok");
            //}
        }

        RawUuid  uuid = randomUUID().data;

        string[] vals;

        vals ~= ticket_id;
        vals ~= user_id;
        vals ~= action;
        vals ~= text(args);
        vals ~= result;
        vals ~= text(result_code);
        vals ~= text(timestamp - start_time);

        tdb_cons.add(uuid, timestamp / 1000, vals);
        count_trails++;

        if (count_trails > 1000)
        {
            log.trace("flush trail db");
            tdb_cons.finalize();
            delete tdb_cons;
//            exist_trail.close ();
            tdb_cons     = null;
            count_trails = 0;
        }
    }
    catch (Throwable tr)
    {
        log.trace("ERR: error=[%s], stack=%s", tr.msg, tr.info);
    }
}

//////////////////////////////////////////////////////////////////// ws-server-transport
private OpResult modify_individual(Context context, string cmd, string _ticket, Json individual_json, bool prepare_events, string event_id,
                                   string transaction_id, ulong start_time)
{
    OpResult op_res;

    Ticket   *ticket = context.get_ticket(_ticket);

    if (ticket.result != ResultCode.OK)
        throw new HTTPStatusException(ticket.result, text(ticket.result));

    if (wsc_server_task is Task.init)
        throw new HTTPStatusException(ResultCode.Internal_Server_Error, text(ResultCode.Internal_Server_Error));
    Json juri = individual_json[ "@" ];

    if (juri.type == Json.Type.undefined)
        throw new HTTPStatusException(ResultCode.Internal_Server_Error, text(ResultCode.Internal_Server_Error));

    string res;
    Json   jreq = Json.emptyObject;
    jreq[ "function" ]       = cmd;
    jreq[ "ticket" ]         = _ticket;
    jreq[ "individual" ]     = individual_json;
    jreq[ "prepare_events" ] = prepare_events;
    jreq[ "event_id" ]       = event_id;
    jreq[ "transaction_id" ] = transaction_id;

    vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
    vibe.core.concurrency.receive((string _res){ res = _res; op_res = parseOpResult(_res); });

    //if (trace_msg[ 500 ] == 1)
//    log.trace("put_individual #end : indv=%s, res=%s", individual_json, text(op_res));

    if (op_res.result != ResultCode.OK)
        throw new HTTPStatusException(op_res.result, text(op_res.result));
//    Json juc = individual_json[ "v-s:updateCounter" ];

//    long update_counter = 0;
//    if (juc.type != Json.Type.undefined && juc.length > 0)
//        update_counter = juc[ 0 ][ "data" ].get!long;
    //set_updated_uid(juri.get!string, op_res.op_id, update_counter + 1);

    trail(_ticket, ticket.user_uri, cmd, jreq, res, op_res.result, start_time);

    return op_res;
}


private OpResult parseOpResult(string str)
{
    OpResult res;

    res.result = ResultCode.Internal_Server_Error;

    if (str is null || str.length < 3)
        return res;

    try
    {
        Json jresp = parseJsonString(str);

        auto jtype = jresp[ "type" ];
        if (jtype.get!string == "OpResult")
        {
            res.op_id  = jresp[ "op_id" ].to!long;
            res.result = cast(ResultCode)jresp[ "result" ].to!int;
        }
    }
    catch (Throwable tr)
    {
        log.trace("ERR! parseOpResult: %s", tr.info);
    }

    return res;
}

private Task wsc_server_task;

//////////////////////////////////////  WS
void connectToWS()
{
    WebSocket ws;

    while (ws is null)
    {
        try
        {
            auto ws_url = URL("ws://127.0.0.1:8091/ws");
            ws = connectWebSocket(ws_url);

            log.tracec("WebSocket connected to %s", ws_url);
        } catch (Throwable tr)
        {
            log.tracec("ERR! %s", tr.msg);
            core.thread.Thread.sleep(dur!("seconds")(10));
        }
    }

    wsc_server_task = Task.getThis();

    while (true)
    {
        Json msg;
        Task result_task_to;

        vibe.core.concurrency.receive(
                                      (Json _msg, Task _to)
                                      {
                                          msg = _msg;
                                          result_task_to = _to;
                                      }
                                      );

        //log.trace("sending '%s'", msg);
        ws.send(msg.toString());
        //log.trace("Ok");
        string resp = ws.receiveText();
        //log.trace("recv '%s'", resp);
        vibe.core.concurrency.send(result_task_to, resp);
        //log.trace("send to task ok");
    }
    //logFatal("Connection lost!");
}