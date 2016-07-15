module veda.frontend.core_rest;

import std.stdio, std.datetime, std.conv, std.string, std.datetime, std.file;
import core.vararg, core.stdc.stdarg;
import vibe.d, vibe.core.core, vibe.core.log, vibe.core.task, vibe.inet.mimetypes;
import properd;
import veda.frontend.core_driver, veda.type, veda.core.common.context, veda.core.common.know_predicates, veda.core.common.define, veda.core.log_msg;
import veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.frontend.individual8vjson;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "REST");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

public const string veda_schema__File          = "v-s:File";
public const string veda_schema__fileName      = "v-s:fileName";
public const string veda_schema__fileSize      = "v-s:fileSize";
public const string veda_schema__fileThumbnail = "v-s:fileThumbnail";
public const string veda_schema__fileURI       = "v-s:fileURI";

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
    long get_operation_state(int module_id);

//    @path("wait_module") @method(HTTPMethod.GET)
//    long wait_module(int module_id, long op_id);

    @path("restart") @method(HTTPMethod.GET)
    OpResult restart(string ticket);

    @path("set_trace") @method(HTTPMethod.GET)
    void set_trace(int idx, bool state);

    @path("backup") @method(HTTPMethod.GET)
    void backup(bool to_binlog);

    @path("count_individuals") @method(HTTPMethod.GET)
    long count_individuals();

    @path("query") @method(HTTPMethod.GET)
    string[] query(string ticket, string query, string sort = null, string databases = null, bool reopen = false, int top = 10000, int limit = 10000);

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


struct Worker
{
    std.concurrency.Tid tid;
    int                 id;
    bool                ready      = true;
    bool                complete   = false;
    int                 count_busy = 0;

    // worker result data
    ResultCode          rc;
    union
    {
        string[] res_string_array;
        Json[]   res_json_array;
        long     res_long;
    }
}


class VedaStorageRest : VedaStorageRest_API
{
    private Context    context;
    private Worker *[] pool;
    string[ string ] properties;
    int                last_used_tid = 0;
    void function(int sig) shutdown;
        int max_count_workers_on_thread = 100;

    this(std.concurrency.Tid[] _pool, Context _local_context, void function(int sig) _shutdown)
    {
        shutdown = _shutdown;
        context  = _local_context;
        foreach (idx, tid; _pool)
        {
                for (int i = 0; i < max_count_workers_on_thread; i++)
                {
                    Worker *worker = new Worker(tid, cast(int)idx, true, false, 0, ResultCode.No_Content);
                    pool ~= worker;
                }
        }
        log.trace ("create pool.size=%d", pool.length);
    }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private std.concurrency.Tid getFreeTid()
    {
        writeln("#1 getFreeTid");
        Worker *res;
        last_used_tid++;

        if (last_used_tid >= pool.length)
            last_used_tid = 0;

        res = pool[ last_used_tid ];

        return res.tid;
    }

    private Worker *get_worker(int idx)
    {
        return pool[ idx ];
    }

    private Worker *allocate_worker()
    {
        Worker *worker = get_free_worker();

//        while (worker is null)
//        {
//            vibe.core.core.yield();
//            worker = get_free_worker();
//        }
        if (worker is null)
            return null;

        worker.ready    = false;
        worker.complete = false;
        return worker;
    }

    private Worker *get_free_worker()
    {
        Worker *res = null;

        for (int idx = 0; idx < pool.length; idx++)
        {
            res = pool[ idx ];
            if (res.ready == true)
            {
                res.count_busy = 0;
                return res;
            }
            res.count_busy++;
            if (res.count_busy > 1000)
            {
                //core.thread.Thread.sleep(dur!("msecs")(100));
                //writeln ("@allocate worker, count = ", count);

                //string ppp;
                //foreach (p ; pool)
                //ppp ~= "[" ~ text (p.count_busy) ~ "]";

                //writeln ("@pool= ", ppp);
            }
        }
        //writeln ("--- ALL WORKERS BUSY ----");
        return null;
    }

    private string[] put_another_get_my(int another_worker_id, string[] another_res, ResultCode another_result_code, Worker *my_worker)
    {
        // сохраняем результат
        Worker *another_worker = get_worker(another_worker_id);

        another_worker.res_string_array = another_res;
        another_worker.rc               = another_result_code;
        another_worker.complete         = true;

        // цикл по ожиданию своего результата
        while (my_worker.complete == false)
            vibe.core.core.yield();

        string[] res = my_worker.res_string_array.dup;

        my_worker.complete = false;
        my_worker.ready    = true;

        if (my_worker.rc != ResultCode.OK)
            throw new HTTPStatusException(my_worker.rc);

        //writeln ("free worker ", my_worker.id);

        return res;
    }

    private Json[] put_another_get_my(int another_worker_id, Json[] another_res, ResultCode another_result_code,
                                      Worker *my_worker)
    {
        // сохраняем результат
        Worker *another_worker = get_worker(another_worker_id);

        another_worker.res_json_array = another_res;
        another_worker.rc             = another_result_code;
        another_worker.complete       = true;

        // цикл по ожиданию своего результата
        while (my_worker.complete == false)
            vibe.core.core.yield();

        Json[] res = my_worker.res_json_array.dup;

        my_worker.complete = false;
        my_worker.ready    = true;

        if (my_worker.rc != ResultCode.OK)
            throw new HTTPStatusException(my_worker.rc);

        //writeln ("free worker ", my_worker.id);

        return res;
    }

    private long put_another_get_my(int another_worker_id, long another_res, ResultCode another_result_code,
                                    Worker *my_worker)
    {
        // сохраняем результат
        Worker *another_worker = get_worker(another_worker_id);

        another_worker.res_long = another_res;
        another_worker.rc       = another_result_code;
        another_worker.complete = true;

        // цикл по ожиданию своего результата
        while (my_worker.complete == false)
            vibe.core.core.yield();

        long res = my_worker.res_long;

        if (my_worker.rc != ResultCode.OK)
            throw new HTTPStatusException(my_worker.rc);

        my_worker.complete = false;
        my_worker.ready    = true;

        return res;
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

        string _ticket = req.cookies.get("ticket", "");

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

        rc = ticket.result;
        if (rc == ResultCode.OK)
        {
            res = context.get_rights(ticket, uri);
        }

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

    Json[] get_rights_origin(string ticket, string uri)
    {
        immutable(Individual)[] individuals;
        //Tid                     my_task = Task.getThis();

        //if (my_task !is Tid.init)
        {
            std.concurrency.send(getFreeTid(), Command.Get, Function.RightsOrigin, uri, ticket, std.concurrency.thisTid);
            //yield();
            individuals = std.concurrency.receiveOnly!(immutable(Individual)[]);
        }

        Json[] json = Json[].init;
        foreach (individual; individuals)
            json ~= individual_to_json(individual);

        return json;
    }

    Ticket authenticate(string login, string password)
    {
        Ticket ticket = context.authenticate(login, password);

        if (ticket.result != ResultCode.OK)
            throw new HTTPStatusException(ticket.result);
        return ticket;
    }

    Ticket get_ticket_trusted(string ticket, string login)
    {
        Ticket new_ticket = context.get_ticket_trusted(ticket, login);

        if (new_ticket.result != ResultCode.OK)
            throw new HTTPStatusException(new_ticket.result);
        return new_ticket;
    }

    long get_operation_state(int module_id)
    {
        return context.get_operation_state(cast(P_MODULE)module_id);
    }
/*
    long wait_module(int module_id, long op_id)
    {
        writeln ("@z1 wait_module ", cast(P_MODULE)module_id);
        long res = context.wait_thread(cast(P_MODULE)module_id, op_id);

        writeln ("@ze wait_module");
        return res;
    }
 */
    OpResult restart(string _ticket)
    {
        OpResult res;

        Ticket   *ticket = context.get_ticket(_ticket);

        if (ticket is null)
            return res;

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
        ResultCode rc = ResultCode.OK;
        int        recv_worker_id;

        long       res = -1;

        Worker     *worker = allocate_worker();

        if (worker is null)
            throw new HTTPStatusException(ResultCode.Too_Many_Requests);

        std.concurrency.send(getFreeTid(), Command.Execute, Function.Backup, to_binlog, worker.id, std.concurrency.thisTid);
        vibe.core.core.yield();
        std.concurrency.receive((bool _res, int _recv_worker_id) { res = _res; recv_worker_id = _recv_worker_id; });

        if (recv_worker_id == worker.id)
        {
            worker.complete = false;
            worker.ready    = true;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);
        }
        else
        {
            res = put_another_get_my(recv_worker_id, res, rc, worker);
        }
        return;
    }

    long count_individuals()
    {
        ResultCode rc = ResultCode.OK;
        int        recv_worker_id;

        long       res = -1;

        Worker     *worker = allocate_worker();

        if (worker is null)
            throw new HTTPStatusException(ResultCode.Too_Many_Requests);

        std.concurrency.send(worker.tid, Command.Execute, Function.CountIndividuals, worker.id, std.concurrency.thisTid);
        vibe.core.core.yield();
        std.concurrency.receive((long _res, int _recv_worker_id) { res = _res; recv_worker_id = _recv_worker_id; });

        if (recv_worker_id == worker.id)
        {
            worker.complete = false;
            worker.ready    = true;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);
        }
        else
        {
            res = put_another_get_my(recv_worker_id, res, rc, worker);
        }
        return res;
    }

    bool is_ticket_valid(string ticket)
    {
        bool res = context.is_ticket_valid(ticket);

        return res;
    }

    string[] query(string ticket, string _query, string sort = null, string databases = null, bool reopen = false, int top = 10000, int limit = 10000)
    {
        StopWatch sw; sw.start;

        try
        {
            ResultCode rc;
            int        recv_worker_id;

            string[]   individuals_ids;

            Worker     *worker = allocate_worker();
            if (worker is null)
                throw new HTTPStatusException(ResultCode.Too_Many_Requests);

            std.concurrency.send(worker.tid, Command.Get, Function.IndividualsIdsToQuery, _query, sort, databases, ticket, reopen,
                                 top, limit, worker.id, std.concurrency.thisTid);

            vibe.core.core.yield();

            std.concurrency.receive((immutable(
                                               string)[] _individuals_ids, ResultCode _rc, int _recv_worker_id)
                                    { individuals_ids = cast(string[])_individuals_ids;
                                      rc = _rc; recv_worker_id =
                                          _recv_worker_id; });

            if (recv_worker_id == worker.id)
            {
                worker.complete = false;
                worker.ready    = true;

                if (rc != ResultCode.OK)
                    throw new HTTPStatusException(rc);
            }
            else
            {
                individuals_ids = put_another_get_my(recv_worker_id, individuals_ids, rc, worker);
            }
            return individuals_ids;
        }
        finally
        {
            //context.stat(CMD.GET, sw);
        }
    }

    Json[] get_individuals(string ticket, string[] uris)
    {
        ResultCode rc;
        int        recv_worker_id;

        Json[]     res;

        Worker     *worker = allocate_worker();
        if (worker is null)
            throw new HTTPStatusException(ResultCode.Too_Many_Requests);

        std.concurrency.send(worker.tid, Command.Get, Function.Individuals, uris.idup, ticket, worker.id, std.concurrency.thisTid);
        vibe.core.core.yield();
        std.concurrency.receive((immutable(
                                           Json)[] _res, ResultCode _rc, int _recv_worker_id) { res = cast(Json[])_res; rc = _rc;
                                                                                                recv_worker_id =
                                                                                                    _recv_worker_id; });

        if (recv_worker_id == worker.id)
        {
            worker.complete = false;
            worker.ready    = true;

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);
        }
        else
        {
            res = put_another_get_my(recv_worker_id, res, rc, worker);
        }

        return res;
    }

    Json get_individual(string _ticket, string uri, bool reopen = false)
    {
        StopWatch sw; sw.start;

        try
        {
            if (trace_msg[ 500 ] == 1)
                log.trace("get_individual #start : %s ", uri);

            ResultCode rc;
            int        recv_worker_id;

            Json[]     res;

            Worker     *worker = allocate_worker();
            if (worker is null)
                throw new HTTPStatusException(ResultCode.Too_Many_Requests);

            std.concurrency.send(worker.tid, Command.Get, Function.Individual, uri, "", _ticket, reopen, worker.id, std.concurrency.thisTid);
            vibe.core.core.yield();
            std.concurrency.receive((immutable(
                                               Json)[] _res, ResultCode _rc, int _recv_worker_id) { res = cast(Json[])_res; rc = _rc;
                                                                                                    recv_worker_id = _recv_worker_id; });

            if (recv_worker_id == worker.id)
            {
                //writeln ("free worker ", worker.id);
                worker.complete = false;
                worker.ready    = true;

                if (rc != ResultCode.OK)
                {
                    if (trace_msg[ 500 ] == 1)
                        log.trace("ERR! get_individual : %s ", text(rc));

                    throw new HTTPStatusException(rc);
                }
            }
            else
            {
                res = put_another_get_my(recv_worker_id, res, rc, worker);
            }

            if (trace_msg[ 500 ] == 1)
                log.trace("get_individual #end : %s, res.length=%d", text(rc), res.length);

            if (res.length > 0)
                return res[ 0 ];
            else
                return Json.init;
        }
        finally
        {
            //context.stat(CMD.GET, sw);
            //if (trace_msg[ 25 ] == 1)
            //    log.trace("get_individual: end, uri=%s", uri);
        }
    }

    OpResult remove_individual(string _ticket, string uri, bool prepare_events, string event_id, string transaction_id)
    {
        OpResult res;

        if (trace_msg[ 500 ] == 1)
            log.trace("remove_individual #start : uri=%s", uri);

//        long fts_count_prep_put = veda.core.threads.xapian_indexer.get_count_prep_put();
//        long fts_count_recv_put = veda.core.threads.xapian_indexer.get_count_recv_put();

//        if (fts_count_recv_put - fts_count_prep_put > 200)
//        {
//            log.trace("remove_individual: uri=%s, res=%s, fts_count_prep_put=%d fts_count_recv_put=%d", uri, text(ResultCode.Too_Many_Requests), fts_count_prep_put, fts_count_recv_put);
//            throw new HTTPStatusException(ResultCode.Too_Many_Requests);
//        }

        Ticket     *ticket = context.get_ticket(_ticket);

        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            res = context.remove_individual(ticket, uri, prepare_events, event_id == "" ? null : event_id);

            if (trace_msg[ 500 ] == 1)
                log.trace("remove_individual #end : uri=%s, res=%s", uri, text(res));
        }

        if (res.result != ResultCode.OK)
            throw new HTTPStatusException(res.result);

        return res;
    }

    OpResult put_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        OpResult res;

        if (trace_msg[ 500 ] == 1)
            log.trace("put_individual #start : %s", text(individual_json));

//        long fts_count_prep_put = veda.core.threads.xapian_indexer.get_count_prep_put();
//        long fts_count_recv_put = veda.core.threads.xapian_indexer.get_count_recv_put();

//        if (fts_count_recv_put - fts_count_prep_put > 500)
//        {
//            log.trace("put_individual: uri=%s, res=%s, fts_count_prep_put=%d fts_count_recv_put=%d", "?", text(ResultCode.Too_Many_Requests), fts_count_prep_put, fts_count_recv_put);
//            throw new HTTPStatusException(ResultCode.Too_Many_Requests);
//        }

        Ticket     *ticket = context.get_ticket(_ticket);
//log.trace("put_individual #1");
        ResultCode rc = ticket.result;

        if (ticket.result == ResultCode.OK)
        {
            Individual indv = json_to_individual(individual_json);
            res = context.put_individual(ticket, indv.uri, indv, prepare_events, event_id == "" ? null : event_id);

            if (trace_msg[ 500 ] == 1)
                log.trace("put_individual #end : uri=%s, res=%s", indv.uri, text(res));
        }
        else
        {
            res.result = ticket.result;
        }

        if (res.result != ResultCode.OK)
        {
            throw new HTTPStatusException(res.result);
        }
//log.trace("put_individual #8");

        return res;
    }

    OpResult add_to_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        Ticket     *ticket = context.get_ticket(_ticket);

        OpResult   res;
        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            Individual indv = json_to_individual(individual_json);
            res = context.add_to_individual(ticket, indv.uri, indv, prepare_events, event_id == "" ? null : event_id);
        }

        if (res.result != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return res;
    }

    OpResult set_in_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        Ticket     *ticket = context.get_ticket(_ticket);

        OpResult   res;
        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            Individual indv = json_to_individual(individual_json);
            res = context.set_in_individual(ticket, indv.uri, indv, prepare_events, event_id == "" ? null : event_id);
        }

        if (res.result != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return res;
    }

    OpResult remove_from_individual(string _ticket, Json individual_json, bool prepare_events, string event_id, string transaction_id)
    {
        Ticket     *ticket = context.get_ticket(_ticket);

        OpResult   res;
        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            Individual indv = json_to_individual(individual_json);
            res = context.remove_from_individual(ticket, indv.uri, indv, prepare_events, event_id == "" ? null : event_id);
        }

        if (res.result != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return res;
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
