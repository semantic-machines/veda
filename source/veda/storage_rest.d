module veda.storage_rest;

import vibe.d;
import veda.pacahon_driver;

import std.stdio, std.datetime, std.conv, std.string, std.datetime, std.file;
import core.vararg;
import core.stdc.stdarg;
import vibe.core.core, vibe.core.log, vibe.core.task, vibe.inet.mimetypes;
import properd;

import type;
import pacahon.context;
import pacahon.know_predicates;
import onto.onto;
import onto.individual;
import onto.resource;
import onto.lang;
import veda.util;


public const string veda_schema__File          = "v-s:File";
public const string veda_schema__fileName      = "v-s:fileName";
public const string veda_schema__fileSize      = "v-s:fileSize";
public const string veda_schema__fileThumbnail = "v-s:fileThumbnail";
public const string veda_schema__fileURI       = "v-s:fileURI";

const string        attachments_db_path = "./data/files";

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

    @path("is_ticket_valid") @method(HTTPMethod.GET)
    bool is_ticket_valid(string ticket);

    @path("wait_pmodule") @method(HTTPMethod.GET)
    void wait_pmodule(int pmodule_id);

    @path("set_trace") @method(HTTPMethod.GET)
    void set_trace(int idx, bool state);

    @path("backup") @method(HTTPMethod.GET)
    void backup();

    @path("count_individuals") @method(HTTPMethod.GET)
    long count_individuals();

    @path("query") @method(HTTPMethod.GET)
    string[] query(string ticket, string query, string sort = null, string databases = null, bool reopen = false);

    @path("get_individuals") @method(HTTPMethod.POST)
    Json[] get_individuals(string ticket, string[] uris);

    @path("get_individual") @method(HTTPMethod.GET)
    Json get_individual(string ticket, string uri);

    @path("put_individual") @method(HTTPMethod.PUT)
    int put_individual(string ticket, Json individual, bool wait_for_indexing);

    @path("remove_from_individual") @method(HTTPMethod.PUT)
    int remove_from_individual(string ticket, Json individual, bool wait_for_indexing);

    @path("set_in_individual") @method(HTTPMethod.PUT)
    int set_in_individual(string ticket, Json individual, bool wait_for_indexing);

    @path("add_to_individual") @method(HTTPMethod.PUT)
    int add_to_individual(string ticket, Json individual, bool wait_for_indexing);
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
    int                count_thread;

    int                last_used_tid = 0;

    this(std.concurrency.Tid[] _pool, Context _local_context, ref string[ string ] _properties)
    {
        properties   = _properties;
        count_thread = properties.as!(int)("count_thread");

        context = _local_context;
        foreach (idx, tid; _pool)
        {
            Worker *worker = new Worker(tid, cast(int)idx, true, false, 0, ResultCode.No_Content);
            pool ~= worker;
        }
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
        //writeln ("@1 request worker");
        Worker *worker = get_free_worker();

        while (worker is null)
        {
            yield();
            worker = get_free_worker();
        }
        //writeln ("@2 allocate worker ", worker.id);
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
            yield();

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
            yield();

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
            yield();

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
        writeln("@v req.path=", req.path);

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

        writeln("@v uri=", uri);

        string _ticket = req.cookies.get("ticket", "");

        writeln("@v ticket=", _ticket);

        if (uri.length > 3 && _ticket !is null)
        {
            Ticket     *ticket = context.get_ticket(_ticket);

            Individual file_info;

            ResultCode rc = ticket.result;
            if (rc == ResultCode.OK)
            {
                file_info = context.get_individual(ticket, uri);

                writeln("@v file_info=", file_info);
                auto fileServerSettings = new HTTPFileServerSettings;
                fileServerSettings.encodingFileExtension = [ "jpeg":".JPG" ];

                string path     = file_info.getFirstResource("v-s:filePath").get!string;
                string file_uri = file_info.getFirstResource("v-s:fileUri").get!string;

                if (path !is null && file_uri !is null && file_uri.length > 0)
                {
                    if (path.length > 0)
                        path = path ~ "/";

                    HTTPServerRequestDelegate dg =
                        serveStaticFile(attachments_db_path ~ "/" ~ path ~ file_uri, fileServerSettings);
                    string                    originFileName = file_info.getFirstResource(veda_schema__fileName).get!string;

                    writeln("@v originFileName=", originFileName);
                    writeln("@v getMimeTypeForFile(originFileName)=", getMimeTypeForFile(originFileName));

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
            indv_res.addResource(veda_schema__canRead, Resource(true));

        if ((res & Access.can_update) > 0)
            indv_res.addResource(veda_schema__canUpdate, Resource(true));

        if ((res & Access.can_delete) > 0)
            indv_res.addResource(veda_schema__canDelete, Resource(true));

        if ((res & Access.can_create) > 0)
            indv_res.addResource(veda_schema__canCreate, Resource(true));


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

    void wait_pmodule(int thread_id)
    {
        context.wait_thread(cast(P_MODULE)thread_id);
        if (thread_id == P_MODULE.fulltext_indexer)
        {
            context.reopen_ro_fulltext_indexer_db();
        }
    }

    void set_trace(int idx, bool state)
    {
        context.set_trace(idx, state);
    }

    void backup()
    {
        ResultCode rc = ResultCode.OK;
        int        recv_worker_id;

        long       res = -1;

        Worker     *worker = allocate_worker();

        std.concurrency.send(getFreeTid(), Command.Execute, Function.Backup, worker.id, std.concurrency.thisTid);
        yield();
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

        std.concurrency.send(worker.tid, Command.Execute, Function.CountIndividuals, worker.id, std.concurrency.thisTid);
        yield();
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

    string[] query(string ticket, string _query, string sort = null, string databases = null, bool reopen = false)
    {
        ResultCode rc;
        int        recv_worker_id;

        string[]   individuals_ids;

        Worker     *worker = allocate_worker();

        std.concurrency.send(worker.tid, Command.Get, Function.IndividualsIdsToQuery, _query, sort, databases, ticket, reopen, worker.id,
                             std.concurrency.thisTid);
        yield();

        std.concurrency.receive((immutable(
                                           string)[] _individuals_ids, ResultCode _rc, int _recv_worker_id) { individuals_ids =
                                                                                                                  cast(string[])_individuals_ids;
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

    Json[] get_individuals(string ticket, string[] uris)
    {
        ResultCode rc;
        int        recv_worker_id;

        Json[]     res;

        Worker     *worker = allocate_worker();

        std.concurrency.send(worker.tid, Command.Get, Function.Individuals, uris.idup, ticket, worker.id, std.concurrency.thisTid);
        yield();
        std.concurrency.receive((immutable(
                                           Json)[] _res, ResultCode _rc, int _recv_worker_id) { res = cast(Json[])_res; rc = _rc; recv_worker_id =
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

    Json get_individual(string _ticket, string uri)
    {
        ResultCode rc;
        int        recv_worker_id;

        if (true)
        {
            Json[] res;

            Worker *worker = allocate_worker();

            std.concurrency.send(worker.tid, Command.Get, Function.Individual, uri, "", _ticket, worker.id, std.concurrency.thisTid);
            yield();
            std.concurrency.receive((immutable(
                                               Json)[] _res, ResultCode _rc, int _recv_worker_id) { res = cast(Json[])_res; rc = _rc;
                                                                                                    recv_worker_id = _recv_worker_id; });

            if (recv_worker_id == worker.id)
            {
                //writeln ("free worker ", worker.id);
                worker.complete = false;
                worker.ready    = true;

                if (rc != ResultCode.OK)
                    throw new HTTPStatusException(rc);
            }
            else
            {
                res = put_another_get_my(recv_worker_id, res, rc, worker);
            }

            if (res.length > 0)
                return res[ 0 ];
            else
                return Json.init;
        }
        else
        {
            Individual res;

            Individual[ string ] onto_individuals =
                context.get_onto_as_map_individuals();

            Individual individual = onto_individuals.get(uri, Individual.init);

            if (individual != Individual.init)
            {
                rc  = ResultCode.OK;
                res = individual;
            }
            else
            {
                Ticket *ticket = context.get_ticket(_ticket);
                rc = ticket.result;
                if (rc == ResultCode.OK)
                {
                    Individual ii = context.get_individual(ticket, uri);
                    if (ii.getStatus() == ResultCode.OK)
                        res = ii;
                    else
                        rc = ii.getStatus();
                }
            }

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);

            Json json = individual_to_json(res);
            return json;
        }
    }

    int put_individual(string _ticket, Json individual_json, bool wait_for_indexing)
    {
        Individual indv    = json_to_individual(individual_json);
        Ticket     *ticket = context.get_ticket(_ticket);

        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            rc = context.put_individual(ticket, indv.uri, indv, wait_for_indexing);
        }

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return rc.to!int;
    }

    int add_to_individual(string _ticket, Json individual_json, bool wait_for_indexing)
    {
        Individual indv    = json_to_individual(individual_json);
        Ticket     *ticket = context.get_ticket(_ticket);

        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            rc = context.add_to_individual(ticket, indv.uri, indv, wait_for_indexing);
        }

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return rc.to!int;
    }

    int set_in_individual(string _ticket, Json individual_json, bool wait_for_indexing)
    {
        Individual indv    = json_to_individual(individual_json);
        Ticket     *ticket = context.get_ticket(_ticket);

        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            rc = context.set_in_individual(ticket, indv.uri, indv, wait_for_indexing);
        }

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return rc.to!int;
    }

    int remove_from_individual(string _ticket, Json individual_json, bool wait_for_indexing)
    {
        Individual indv    = json_to_individual(individual_json);
        Ticket     *ticket = context.get_ticket(_ticket);

        ResultCode rc = ticket.result;

        if (rc == ResultCode.OK)
        {
            rc = context.remove_from_individual(ticket, indv.uri, indv, wait_for_indexing);
        }

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc);

        return rc.to!int;
    }
}
