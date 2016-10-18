module veda.frontend.core_rest;

import std.stdio, std.datetime, std.conv, std.string, std.datetime, std.file, core.atomic;
import core.vararg, core.stdc.stdarg;
import vibe.d, vibe.core.core, vibe.core.log, vibe.core.task, vibe.inet.mimetypes;
import properd;
import veda.common.type, veda.core.common.context, veda.core.common.know_predicates, veda.core.common.define, veda.core.common.log_msg;
import veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.frontend.individual8vjson;
import veda.frontend.cbor8vjson;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-webserver", "log", "REST");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

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

    @path("authenticate") @method(HTTPMethod.GET)
    Ticket authenticate(string login, string password);

    @path("get_ticket_trusted") @method(HTTPMethod.GET)
    Ticket get_ticket_trusted(string ticket, string login);

    @path("is_ticket_valid") @method(HTTPMethod.GET)
    bool is_ticket_valid(string ticket);

    @path("get_operation_state") @method(HTTPMethod.GET)
    long get_operation_state(int module_id, long wait_op_id);

    @path("flush") @method(HTTPMethod.GET)
    void flush(int module_id, long wait_op_id);

    @path("restart") @method(HTTPMethod.GET)
    OpResult restart(string ticket);

    @path("set_trace") @method(HTTPMethod.GET)
    void set_trace(int idx, bool state);

    @path("backup") @method(HTTPMethod.GET)
    void backup(bool to_binlog);

    @path("count_individuals") @method(HTTPMethod.GET)
    long count_individuals();

    @path("query") @method(HTTPMethod.GET)
    immutable(string)[] query(string ticket, string query, string sort = null, string databases = null, bool reopen = false, int top = 10000,
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

    @path("unload_to_queue") @method(HTTPMethod.PUT)
    long unload_to_queue(string queue_id);
}


class VedaStorageRest : VedaStorageRest_API
{
    private Context context;
//    string[ string ] properties;
    int             last_used_tid = 0;
    void function(int sig) shutdown;

    this(Context _local_context, void function(int sig) _shutdown)
    {
        shutdown = _shutdown;
        context  = _local_context;
    }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

    void fileManager(HTTPServerRequest req, HTTPServerResponse res)
    {
        //writeln("@v req.path=", req.path);

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

        //writeln("@v uri=", uri);
        auto   ticket_ff = "ticket" in req.query;
        string _ticket;

        if (ticket_ff !is null)
            _ticket = cast(string)*ticket_ff;
        else
            _ticket = req.cookies.get("ticket", "");

        //writeln("@v ticket=", _ticket);

        if (uri.length > 3 && _ticket !is null)
        {
            Ticket     *ticket = context.get_ticket(_ticket);

            Individual file_info;

            ResultCode rc = ticket.result;
            if (rc == ResultCode.OK)
            {
                file_info = context.get_individual(ticket, uri);

                //writeln("@v file_info=", file_info);
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

                    //writeln("@v originFileName=", originFileName);
                    //writeln("@v getMimeTypeForFile(originFileName)=", getMimeTypeForFile(originFileName));

                    res.headers[ "Content-Disposition" ] = "attachment; filename=\"" ~ originFileName ~ "\"";

                    res.contentType = getMimeTypeForFile(originFileName);
                    dg(req, res);
                }
            }
        }
    }

    override :

    Json get_rights(string _ticket, string uri)
    {
        ResultCode rc;
        ubyte      res;

        Ticket     *ticket = context.get_ticket(_ticket);

        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);

        res = context.get_rights(ticket, uri);

        Individual indv_res;
        indv_res.uri = "_";

        indv_res.addResource(rdf__type, Resource(DataType.Uri, veda_schema__PermissionStatement));

        if ((res & Access.can_read) > 0)
            indv_res.addResource("v-s:canRead", Resource(true));

        if ((res & Access.can_update) > 0)
            indv_res.addResource("v-s:canUpdate", Resource(true));

        if ((res & Access.can_delete) > 0)
            indv_res.addResource("v-s:canDelete", Resource(true));

        if ((res & Access.can_create) > 0)
            indv_res.addResource("v-s:canCreate", Resource(true));


        Json json = individual_to_json(indv_res);
        return json;
    }

    Json[] get_rights_origin(string _ticket, string uri)
    {
        immutable(Individual)[] res;

        Ticket                  *ticket = context.get_ticket(_ticket);
        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);


        void trace(string resource_group, string subject_group, string right)
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

            res ~= indv_res.idup;
        }

        ResultCode rc;

        context.get_rights_origin(ticket, uri, &trace);


        Json[] json = Json[].init;
        foreach (individual; res)
            json ~= individual_to_json(individual);

        return json;
    }

    Ticket authenticate(string login, string password)
    {
        Ticket ticket;

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

    Ticket get_ticket_trusted(string ticket, string login)
    {
        Ticket new_ticket = context.get_ticket_trusted(ticket, login);

        if (new_ticket.result != ResultCode.OK)
            throw new HTTPStatusException(new_ticket.result, text(new_ticket.result));
        return new_ticket;
    }

    long get_operation_state(int module_id, long wait_op_id)
    {
        //log.trace ("REST:get_operation_state #1");
        long res = -1;

        try
        {
            res = context.get_operation_state(cast(P_MODULE)module_id, wait_op_id);
        }
        catch (Throwable tr)
        {
            log.trace("ERR! REST:get_operation_state, %s \n%s ", tr.msg, tr.info);
        }

        //log.trace ("REST:get_operation_state #e");
        return res;
    }

    void flush(int module_id, long wait_op_id)
    {
        //log.trace("send:flush #1 %s %d", text(cast(P_MODULE)module_id), wait_op_id);
        long     res = -1;

        OpResult op_res;

        Json     jreq = Json.emptyObject;

        jreq[ "function" ]   = "flush";
        jreq[ "module_id" ]  = module_id;
        jreq[ "wait_op_id" ] = wait_op_id;

        vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
        vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

        //log.trace("send:flush #e");
        if (op_res.result != ResultCode.OK)
            throw new HTTPStatusException(op_res.result, text (op_res.result));
    }

    OpResult restart(string _ticket)
    {
        OpResult res;

        Ticket   *ticket = context.get_ticket(_ticket);

        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);

        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            shutdown(-1);
        }

        if (res.result != ResultCode.OK)
            throw new HTTPStatusException(res.result);

        return res;
    }


    void set_trace(int idx, bool state)
    {
        context.set_trace(idx, state);
    }

    void backup(bool to_binlog)
    {
        long     res = -1;

        OpResult op_res;

        Json     jreq = Json.emptyObject;

        jreq[ "function" ]   = "backup";
        jreq[ "to_binlog" ]  = to_binlog;

        vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
        vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

        //log.trace("send:flush #e");
        if (op_res.result != ResultCode.OK)
            throw new HTTPStatusException(op_res.result, text (op_res.result));

        return;
    }

    long count_individuals()
    {
        return context.count_individuals();
    }

    bool is_ticket_valid(string ticket)
    {
        bool res = context.is_ticket_valid(ticket);

        return res;
    }

    immutable(string)[] query(string _ticket, string _query, string sort = null, string databases = null, bool reopen = false, int top = 10000,
                              int limit = 10000)
    {
        Ticket *ticket = context.get_ticket(_ticket);

        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);

        immutable(string)[] individuals_ids;
        individuals_ids = context.get_individuals_ids_via_query(ticket, _query, sort, databases, top, limit);
        return individuals_ids;
    }

    Json[] get_individuals(string _ticket, string[] uris)
    {
        Json[]     res;
        ResultCode rc;

        Ticket     *ticket = context.get_ticket(_ticket);
        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);

        try
        {
            foreach (indv; context.get_individuals(ticket, uris))
            {
                Json jj = individual_to_json(indv);
                res ~= jj;
            }
        }
        catch (Throwable ex)
        {
            throw new HTTPStatusException(ResultCode.Internal_Server_Error);
        }

        return res;
    }

    Json get_individual(string _ticket, string uri, bool reopen = false)
    {
        Json   res;

        Ticket *ticket = context.get_ticket(_ticket);

        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result, text(ticket.result));

        ResultCode rc = ResultCode.Internal_Server_Error;

        try
        {
            Individual[ string ] onto_individuals =
                context.get_onto_as_map_individuals();

            Individual individual = onto_individuals.get(uri, Individual.init);

            if (individual != Individual.init)
            {
                res = individual_to_json(individual);
                rc = ResultCode.OK;
            }
            else
            {
                if (reopen)
                {
                    context.reopen_ro_acl_storage_db();
                    context.reopen_ro_subject_storage_db();
                }

                string cb = context.get_individual_as_cbor(ticket, uri, rc);

                if (rc == ResultCode.OK)
                {
                    res = Json.emptyObject;
                    cbor2json(&res, cb);
                }
                //if (ii.getStatus() == ResultCode.OK)
                //    res ~= cast(immutable)individual_to_json(ii);
                //else
                //    rc = ii.getStatus();
            }
        }
        catch (Throwable ex)
        {
            throw new HTTPStatusException(rc, text(rc));
        }

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc, text(rc));

        return res;
    }

    OpResult remove_individual(string _ticket, string uri, bool prepare_events, string event_id, string transaction_id)
    {
        OpResult op_res;

        Ticket   *ticket = context.get_ticket(_ticket);

        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);

        if (wsc_server_task is Task.init)
            throw new HTTPStatusException(ResultCode.Internal_Server_Error);

        Json jreq = Json.emptyObject;
        jreq[ "function" ]       = "remove";
        jreq[ "ticket" ]         = _ticket;
        jreq[ "uri" ]            = uri;
        jreq[ "prepare_events" ] = prepare_events;
        jreq[ "event_id" ]       = event_id;
        jreq[ "transaction_id" ] = transaction_id;

        vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());

        vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

        //if (trace_msg[ 500 ] == 1)
        //    log.trace("remove_individual #end : uri=%s, res=%s", indv.uri, text(res));

        if (op_res.result != ResultCode.OK)
            throw new HTTPStatusException(op_res.result);

        return op_res;
    }

    OpResult put_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        return modify_individual(context, "put", _ticket, individual_json, prepare_events, event_id, transaction_id);
    }

    OpResult add_to_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        return modify_individual(context, "add_to", _ticket, individual_json, prepare_events, event_id, transaction_id);
    }

    OpResult set_in_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        return modify_individual(context, "set_in", _ticket, individual_json, prepare_events, event_id, transaction_id);
    }

    OpResult remove_from_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        return modify_individual(context, "remove_from", _ticket, individual_json, prepare_events, event_id, transaction_id);
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

    long unload_to_queue(string queue_id)
    {
        return context.unload_subject_storage(queue_id);
    }
}

//////////////////////////////////////////////////////////////////// ws-server-transport
private OpResult modify_individual(Context context, string cmd, string _ticket, Json individual_json, bool prepare_events, string event_id,
                                   string transaction_id)
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

    Json jreq = Json.emptyObject;
    jreq[ "function" ]       = cmd;
    jreq[ "ticket" ]         = _ticket;
    jreq[ "individual" ]     = individual_json;
    jreq[ "prepare_events" ] = prepare_events;
    jreq[ "event_id" ]       = event_id;
    jreq[ "transaction_id" ] = transaction_id;

    vibe.core.concurrency.send(wsc_server_task, jreq, Task.getThis());
    vibe.core.concurrency.receive((string res){ op_res = parseOpResult(res); });

    //if (trace_msg[ 500 ] == 1)
//    log.trace("put_individual #end : indv=%s, res=%s", individual_json, text(op_res));

    if (op_res.result != ResultCode.OK)
        throw new HTTPStatusException(op_res.result, text(op_res.result));
    Json juc = individual_json[ "v-s:updateCounter" ];

    long update_counter = 0;
    if (juc.type != Json.Type.undefined && juc.length > 0)
        update_counter = juc[ 0 ][ "data" ].get!long;
    set_updated_uid(juri.get!string, op_res.op_id, update_counter + 1);

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

                    log.trace("@module_name=%s", module_name);

                    if (module_name == "server")
                    {
                        wsc_server_task = Task.getThis();
                    }
                }
            }

            if (module_name !is null)
            {
                log.trace("create chanel [%s]", module_name);

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
                    socket.send(msg.toString());
                    //log.trace("Ok");
                    string resp = socket.receiveText();
                    //log.trace("recv '%s'", resp);
                    vibe.core.concurrency.send(result_task_to, resp);
                    //log.trace("send to task ok");
                }
            }
        }
    }
    catch (Throwable tr)
    {
        log.trace("ERR! on ws chanel [%s] ex=%s", module_name, tr.msg);
    }

    log.trace("ws chanel [%s] is closed", module_name);
}

/////////////////////////// CCUS WS
private shared        UidInfo[ string ] _info_2_uid;
private shared long   _last_opid;
private shared Object _mutex = new Object();

struct UidInfo
{
    long update_counter;
    long opid;
}

public void set_updated_uid(string uid, long opid, long update_counter)
{
    synchronized (_mutex)
    {
        //log.trace("set_updated_uid (uid=%s, opid=%d, update_counter=%d)", uid, opid, update_counter);
        if ((uid in _info_2_uid) !is null)
            _info_2_uid[ uid ] = UidInfo(update_counter, opid);

        atomicStore(_last_opid, opid);
    }
}

public long get_counter_4_uid(string uid)
{
    long res;

    synchronized (_mutex)
    {
        if ((uid in _info_2_uid) !is null)
            res = _info_2_uid[ uid ].update_counter;
        else
            _info_2_uid[ uid ] = UidInfo(0, 0);

        //log.trace ("get_counter_4_uid(uid=%s)=%d", uid, res);
    }
    return res;
}

public long get_last_opid()
{
    synchronized (_mutex)
    {
        //log.trace ("@ get_last_opid()=[%d]", _last_opid);
        return _last_opid;
    }
}

void handleWebSocketConnection_CCUS(scope WebSocket socket)
{
    const(HTTPServerRequest)hsr = socket.request();

    log.trace("spawn socket connection [%s]", text(hsr.clientAddress));

    // Client Cache Update Subscription
    string chid;
    long[ string ] count_2_uid;

    string get_list_of_changes()
    {
        string res;

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
        //log.trace("get_list_of_changes, res = (%s)", res);
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
        // log.trace("get_list_of_subscribe = (%s)", res);
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
                    if (!socket.connected)
                        break;

                    string msg_from_sock = null;

                    if (socket.waitForData(dur!("msecs")(1000)) == true)
                        msg_from_sock = socket.receiveText();

                    if (msg_from_sock !is null && msg_from_sock.length > 0)
                    {
                        if (msg_from_sock[ 0 ] == '=')
                        {
                            string res = get_list_of_subscribe();

                            if (res !is null)
                                socket.send("=" ~ res);
                            else
                                socket.send("=");
                        }
                        else if (msg_from_sock.length == 2 && msg_from_sock[ 0 ] == '-' && msg_from_sock[ 1 ] == '*')
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
                                                long g_count = get_counter_4_uid(uid);
                                                if (uid_counter < g_count)
                                                {
                                                    string res = get_list_of_changes();
                                                    if (res !is null)
                                                    {
                                                        socket.send(res);
                                                    }
                                                    last_check_opid = get_last_opid();
                                                }

                                                //log.trace("subscribe uid=%s, counter=%d", uid, uid_counter);
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
                                                //log.trace("unsubscribe uid=%s", uid);
                                            }
                                        }
                                    }
                                }
                                catch (Throwable tr)
                                {
                                    log.trace("Client Cache Update Subscription: recv msg:[%s], %s", data, tr.msg);
                                }
                            }
                        }
                    }

                    //writefln ("@ last_check_opid=%d", last_check_opid);
                    long last_opid = get_last_opid();
                    if (last_check_opid < last_opid)
                    {
                        string res = get_list_of_changes();
                        if (res !is null)
                        {
                        	//log.trace ("send list of change, res=%s", res);
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
