import std.conv, std.stdio, std.file, core.runtime, core.thread;
import vibe.d;
import properd;
import veda.onto.individual, veda.onto.resource, veda.core.common.context, veda.core.common.define, veda.core.impl.thread_context;
import veda.frontend.core_rest, veda.frontend.cbor8vjson, veda.frontend.individual8vjson;
import vibe.inet.url, vibe.http.client, vibe.http.server, vibe.http.websockets : WebSocket, handleWebSockets;


// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
veda.common.logger.Logger _log;
veda.common.logger.Logger log()
{
    if (_log is null)
        _log = new veda.common.logger.Logger("veda-core-webserver", "log", "frontend");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

extern (C) void handleTerminationW(int _signal)
{
//    log.trace("!SYS: veda.app: caught signal: %s", text(_signal));
    writefln("!SYS: veda.app: caught signal: %s", text(_signal));

    //veda.core.threads.dcs_manager.close();

    foreach (port, listener; listener_2_port)
    {
        listener.stopListening();
    }

    vibe.core.core.exitEventLoop();

    writeln("!SYS: veda.app: exit");

    thread_term();
    Runtime.terminate();
//    kill(getpid(), SIGKILL);
//    exit(_signal);
}

void shutdown(int _signal)
{
    foreach (port, listener; listener_2_port)
    {
        listener.stopListening();
    }

    vibe.core.core.exitEventLoop();

    writeln("!SYS: veda.app: shutdown");
    kill(getpid(), SIGKILL);
    exit(_signal);
}

void view_error(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error)
{
    res.render!("view_error.dt", req, error);
}

void uploadFile(HTTPServerRequest req, HTTPServerResponse res)
{
    string filename;

    try
    {
        auto pf = "file" in req.files;

        enforce(pf !is null, "No file uploaded!");

        auto pt = "path" in req.form;
        auto nm = "uri" in req.form;
        if (pt !is null && nm !is null)
        {
            string pts = cast(string)*pt;
            filename = cast(string)*nm;

            string[] ptspc = pts.split('/');

            string   np = "./data/files/";
            foreach (it; ptspc)
            {
                np ~= it ~ "/";
                try
                {
                    mkdir(np);
                }
                catch (Exception ex)
                {
                }
            }

            Path path = Path("data/files/" ~ pts ~ "/") ~filename;

            try moveFile(pf.tempPath, path);
            catch (Exception e) {
//                logWarn("Failed to move file to destination folder: %s", e.msg);
//                logInfo("Performing copy+delete instead.");
                copyFile(pf.tempPath, path);
            }

            res.writeBody("File uploaded!", "text/plain");
        }
    }
    catch (Throwable ex)
    {
        log.trace("ERR! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ", " ~ ex.msg ~ ", filename:" ~ filename);
    }
}

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;

HTTPListener[ ushort ] listener_2_port;

shared static this()
{
    import etc.linux.memoryerror;
    static if (is (typeof(registerMemoryErrorHandler)))
        registerMemoryErrorHandler();

    import vibe.core.args;
    Ticket sys_ticket;

    string[ string ] properties;

    try
    {
        properties = readProperties("./veda.properties");
    }
    catch (Exception ex)
    {
    }

    string node_id;
    node_id = properties.as!(string)("node_id");

//    veda.core.common.context.Context core_context;

//    core_context = veda.core.srv.server.init_core(node_id);
//    if (core_context is null)
//    {
//        log.trace("ERR! Veda core has not been initialized");
//        return;
//    }
    veda.core.common.context.Context context;

    context = new PThreadContext(node_id, "frontend", log, "127.0.0.1:8088/ws");

    sys_ticket = context.sys_ticket(false);

    Individual *[ string ] onto_config;

    Individual delegate(Ticket * ticket, string uri) get_individual;

    Individual get_individual_from_local(Ticket *ticket, string uri)
    {
        Individual *ii = onto_config.get(uri, null);

        if (ii !is null)
            return *ii;
        else
            return Individual.init;
    }

    string[] uris = context.get_individuals_ids_via_query(&sys_ticket, "'rdfs:isDefinedBy.isExists' == true", null, null, 0, 100000, 100000).result;

//    long count_individuals = context.count_individuals();
    if (uris.length == 0)
    {
        context.sys_ticket(true);
        onto_config = load_config_onto();

        get_individual = &get_individual_from_local;
    }
    else
    {
        get_individual = &context.get_individual;
    }

    Ticket sticket = *context.get_systicket_from_storage();

//    if (sys_ticket_id !is null)
//    {
//       sticket = *context.get_ticket(sys_ticket_id);
    //set_global_systicket(sticket);
//    }
//    else
//    {
//        sticket = core_context.sys_ticket();
//    }

//    ushort                count_thread = 4;

//    std.concurrency.Tid[] pool;
//    for (int i = 0; i < count_thread; i++)
//    {
//        pool ~= std.concurrency.spawnLinked(&core_thread, node_id);
//        core.thread.Thread.sleep(dur!("msecs")(10));
//    }

    bool is_exist_listener = false;


    Individual node = get_individual(&sticket, node_id);

    //count_thread = cast(ushort)node.getFirstInteger("v-s:count_thread", 4);

    Resources listeners = node.resources.get("v-s:listener", Resources.init);
    foreach (listener_uri; listeners)
    {
        Individual connection = get_individual(&sticket, listener_uri.uri);

        Resource   transport = connection.getFirstResource("v-s:transport");
        if (transport != Resource.init)
        {
            if (transport.data() == "http")
            {
                ushort http_port = cast(ushort)connection.getFirstInteger("v-s:port", 8080);
                is_exist_listener = start_http_listener(context, http_port);
            }
        }
    }
}

bool start_http_listener(Context context, ushort http_port)
{
    try
    {
        VedaStorageRest vsr = new VedaStorageRest(context, &shutdown);

        auto            settings = new HTTPServerSettings;

        settings.port           = http_port;
        settings.maxRequestSize = 1024 * 1024 * 1000;
        //settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
        //settings.bindAddresses = ["127.0.0.1"];
        settings.errorPageHandler = toDelegate(&view_error);
        //settings.options = HTTPServerOption.parseURL|HTTPServerOption.distribute;
        
        HTTPFileServerSettings file_serve_settings = new HTTPFileServerSettings; 
		file_serve_settings.maxAge = dur!"hours"(8);

        auto router = new URLRouter;
        router.get("/files/*", &vsr.fileManager);
        router.get("*", serveStaticFiles("public", file_serve_settings));
        router.get("/", serveStaticFile("public/index.html"));
        router.get("/tests", serveStaticFile("public/tests.html"));
        router.post("/files", &uploadFile);

        registerRestInterface(router, vsr);

        log.trace("=========== ROUTES ============");
        auto routes = router.getAllRoutes();
        log.trace("GET:");
        foreach (route; routes)
        {
            if (route.method == HTTPMethod.GET)
                log.trace(route.pattern);
        }

        log.trace("PUT:");
        foreach (route; routes)
        {
            if (route.method == HTTPMethod.PUT)
                log.trace(route.pattern);
        }

        log.trace("POST:");
        foreach (route; routes)
        {
            if (route.method == HTTPMethod.POST)
                log.trace(route.pattern);
        }

        log.trace("DELETE:");
        foreach (route; routes)
        {
            if (route.method == HTTPMethod.DELETE)
                log.trace(route.pattern);
        }
        log.trace("===============================");

        HTTPListener listener = listenHTTP(settings, router);
        listener_2_port[ http_port ] = listener;

        log.trace("Please open http://127.0.0.1:" ~ text(settings.port) ~ "/ in your browser.");

        router.get("/ws", handleWebSockets(&handleWebSocketConnection));
        settings               = new HTTPServerSettings;
        settings.port          = 8091;
        settings.bindAddresses = [ "127.0.0.1" ];
        listenHTTP(settings, router);
        log.trace("listen /ws %s:%s", text(settings.bindAddresses), text(settings.port));


        //router.get("/ccus", handleWebSockets(&handleWebSocketConnection_CCUS));
        //settings      = new HTTPServerSettings;
        //settings.port = 8088;
        //settings.bindAddresses = [ "127.0.0.1" ];
        //listenHTTP(settings, router);
        //log.trace("listen /ccus %s:%s", text(settings.bindAddresses), text(settings.port));

        return true;
    }
    catch (Exception ex)
    {
        log.trace("start_http_listener# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
    }
    return false;
}

import veda.util.raptor2individual;

Individual *[ string ] load_config_onto()
{
    log.trace("load_config_onto");
    string[ string ] prefixes;

    Individual *[ string ] l_individuals = ttl2individuals(onto_path ~ "/config.ttl", prefixes, prefixes, log);

    log.trace("load_config_onto %s", l_individuals);

    return l_individuals;
}

