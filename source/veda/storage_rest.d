module veda.storage_rest;

import vibe.d;
import veda.pacahon_driver;

import std.stdio, std.datetime, std.conv, std.string, std.datetime, std.file;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import type;
import pacahon.context;
import pacahon.know_predicates;
import onto.owl;
import onto.individual;
import onto.resource;
import onto.lang;
import veda.util;

public const string veda_schema__File          = "veda-schema:File";
public const string veda_schema__fileName      = "veda-schema:fileName";
public const string veda_schema__fileSize      = "veda-schema:fileSize";
public const string veda_schema__fileThumbnail = "veda-schema:fileThumbnail";
public const string veda_schema__fileURI       = "veda-schema:fileURI";

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
    string[] query(string ticket, string query);

    @path("get_individuals") @method(HTTPMethod.POST)
    Json[] get_individuals(string ticket, string uris[]);

    @path("get_individual") @method(HTTPMethod.GET)
    Json get_individual(string ticket, string uri);

    @path("put_individuals") @method(HTTPMethod.PUT)
    int put_individuals(string ticket, Json[] individuals);

    @path("put_individual") @method(HTTPMethod.PUT)
    int put_individual(string ticket, Json individual);

    @path("get_property_values") @method(HTTPMethod.GET)
    Json[] get_property_values(string ticket, string uri, string property_uri);

    @path("execute_script") @method(HTTPMethod.POST)
    string[ 2 ] execute_script(string script);
}

class VedaStorageRest : VedaStorageRest_API
{
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

        string ticket = req.cookies.get("ticket", "");

        writeln("@v ticket=", ticket);

        if (uri.length > 3 && ticket !is null)
        {
            Tid my_task = Task.getThis();

            if (my_task is Tid.init)
                return;

            immutable(Individual)[] individual;
            send(io_task, Command.Get, Function.Individual, uri, ticket, my_task);

            ResultCode rc;
            receive((immutable(Individual)[] _individuals, ResultCode _rc) { individual = _individuals; rc = _rc; });

            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);

            if (individual.length == 0)
                return;

            Individual file_info = cast(Individual)individual[ 0 ];

            writeln("@v file_info=", file_info);

            HTTPServerRequestDelegate dg = serveStaticFile(attachments_db_path ~ "/" ~ file_info.getFirstResource(
                                                                                                                  veda_schema__fileURI).get!string);
            dg(req, res);
        }
    }

    override :

    Json get_rights(string ticket, string uri)
    {
        Individual indv_res;
        Tid        my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            indv_res.uri = "_";
            send(io_task, Command.Get, Function.Rights, uri, ticket, my_task);
            ubyte res = receiveOnly!(ubyte);

//	    writeln ("@v res=", res);

            indv_res.addResource(rdf__type, Resource(DataType.Uri, veda_schema__PermissionStatement));

            if ((res & Access.can_read) > 0)
                indv_res.addResource(veda_schema__canRead, Resource(true));

            if ((res & Access.can_update) > 0)
                indv_res.addResource(veda_schema__canUpdate, Resource(true));

            if ((res & Access.can_delete) > 0)
                indv_res.addResource(veda_schema__canDelete, Resource(true));

            if ((res & Access.can_create) > 0)
                indv_res.addResource(veda_schema__canCreate, Resource(true));
        }

//	writeln ("@v indv_res=", indv_res);
        Json json = individual_to_json(indv_res.idup);
        return json;
    }

    Json[] get_rights_origin(string ticket, string uri)
    {
        immutable(Individual)[] individuals;
        Tid                     my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Get, Function.RightsOrigin, uri, ticket, my_task);
            individuals = receiveOnly!(immutable(Individual)[]);
        }

        Json[] json = Json[].init;
        foreach (individual; individuals)
            json ~= individual_to_json(individual);

        return json;
    }

    Ticket authenticate(string login, string password)
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Get, Function.NewTicket, login, password, my_task);
            immutable(Ticket)[] tickets = receiveOnly!(immutable(Ticket)[]);
            if (tickets.length > 0)
            {
                if (tickets[ 0 ].result != ResultCode.OK)
                    throw new HTTPStatusException(tickets[ 0 ].result);
                return tickets[ 0 ];
            }
        }
        return Ticket.init;
    }

    void wait_pmodule(int thread_id)
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Wait, Function.PModule, cast(P_MODULE)thread_id, my_task);
            receiveOnly!(bool);
        }
    }

    void set_trace(int idx, bool state)
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Set, Function.Trace, idx, state, my_task);
            receiveOnly!(bool);
        }
    }

    void backup()
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Execute, Function.Backup, my_task);
            receiveOnly!(bool);
        }
    }

    long count_individuals()
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Execute, Function.CountIndividuals, my_task);
            long res = receiveOnly!(long);
            return res;
        }
        return -1;
    }

    bool is_ticket_valid(string ticket)
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Is, Function.TicketValid, ticket, my_task);
            bool res = receiveOnly!(bool);
            return res;
        }
        return false;
    }

    string[] query(string ticket, string query)
    {
        Tid                 my_task = Task.getThis();

        immutable(string)[] individuals_ids;
        if (my_task !is Tid.init)
        {
            send(io_task, Command.Get, Function.IndividualsIdsToQuery, query, ticket, my_task);

            ResultCode rc;
            receive((immutable(string)[] _individuals_ids, ResultCode _rc) { individuals_ids = _individuals_ids; rc = _rc; });
            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);
        }
        return cast(string[])individuals_ids;
    }

    Json[] get_individuals(string ticket, string uris[])
    {
//	StopWatch sw;
//	sw.start();

        Tid                     my_task = Task.getThis();

        immutable(Individual)[] individuals = Individual[].init;
        if (my_task !is Tid.init)
        {
//        immutable(Individual)[] individual;
            send(io_task, Command.Get, Function.Individuals, uris.idup, ticket, my_task);

            ResultCode rc;
            receive((immutable(Individual)[] _individuals, ResultCode _rc) { individuals = _individuals; rc = _rc; });
            if (rc != ResultCode.OK)
                throw new HTTPStatusException(rc);
        }

//	sw.stop();
//	long t = cast(long) sw.peek().usecs;
//	logInfo("get_individuals (pacahon) execution time:"~text(t)~" usecs");
//	sw.start();

        Json[] json = Json[].init;
        foreach (individual; individuals)
            json ~= individual_to_json(individual);

//	sw.stop();
//	long t1 = cast(long) sw.peek().usecs;
//	logInfo("get_individuals (transform to json) execution time:"~text(t1)~" usecs");

        return json;
    }

    Json get_individual(string ticket, string uri)
    {
        Tid my_task = Task.getThis();

        if (my_task is Tid.init)
            return Json.init;

        immutable(Individual)[] individual;
        send(io_task, Command.Get, Function.Individual, uri, ticket, my_task);
//    individual = receiveOnly!(immutable(Individual)[]);
        ResultCode rc;
        receive((immutable(Individual)[] _individuals, ResultCode _rc) { individual = _individuals; rc = _rc; });

        if (rc != ResultCode.OK)
            throw new HTTPStatusException(rc);


        if (individual.length == 0)
            return Json.init;

        immutable(Individual) result = individual[ 0 ];
        Json json = individual_to_json(result);
        return json;
    }

    int put_individual(string ticket, Json individual_json)
    {
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            immutable(Individual)[] ind;
            Individual              indv = json_to_individual(individual_json);
            ind ~= indv.idup;
            send(io_task, Command.Put, Function.Individual, ticket, indv.uri, ind, my_task);
            ResultCode res = receiveOnly!(ResultCode);
            return res.to!int;
        }
        return ResultCode.Service_Unavailable.to!int;
    }

    int put_individuals(string ticket, Json[] individuals_json)
    {
//    Tid my_task = Task.getThis();
//    if (my_task !is Tid.init) {
//        immutable(Individual)[] ind;
//        ind ~= individual.idup;
//        send(io_task, Command.Put, Function.Individuals, ticket, uri, ind, my_task);
//        ResultCode res = receiveOnly!(ResultCode);
//        return res;
//    }
        return ResultCode.Service_Unavailable.to!int;
    }

    Json[] get_property_values(string ticket, string uri, string property_uri)
    {
//    Tid my_task = Task.getThis();
        string res;

//    if (my_task !is Tid.init) {
//        send(io_task, Command.Get, Function.PropertyOfIndividual, uri, property_uri, lang, my_task);
//        res = receiveOnly!(string);
//    }
//    return res;
        return Json[].init;
    }

    string[ 2 ] execute_script(string script)
    {
        string[ 2 ] res;
        Tid my_task = Task.getThis();

        if (my_task !is Tid.init)
        {
            send(io_task, Command.Execute, Function.Script, script, my_task);

            res = receiveOnly!(string[ 2 ]);
        }

        return res;
    }
}
