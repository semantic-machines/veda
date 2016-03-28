import std.conv, std.stdio, std.file;
import vibe.d;
import properd;
import veda.core_driver, veda.core_rest;
import veda.onto.individual, veda.onto.resource, veda.core.context, veda.core.define;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "frontend");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

extern (C) void handleTermination(int _signal)
{
    log.trace("!SYS: veda.app: caught signal: %s", _signal);
    writefln("!SYS: veda.app: caught signal: %s", _signal);

    foreach (port, listener; listener_2_port)
    {
        listener.stopListening();
    }

    vibe.core.core.exitEventLoop();

    writeln("!SYS: veda.app: exit");
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
    bsd_signal(SIGINT, &handleTermination);

    import etc.linux.memoryerror;
    static if (is (typeof(registerMemoryErrorHandler)))
        registerMemoryErrorHandler();

    import vibe.core.args;
    string role;
    ushort listener_http_port = 8081;
    string write_storage_node = "http://localhost:8080";
    string sys_ticket_id;

    readOption("role", &role, "set role, if role is empty, then ignore params: [listener_http_port] and [write_storage_node]");
    readOption("listener_http_port", &listener_http_port, "default: 8081");
    readOption("write_storage_node", &write_storage_node, "default: http://127.0.0.1:8080");
    readOption("systicket", &sys_ticket_id, "");

    string[ string ] properties;

    try
    {
        properties = readProperties("./veda.properties");
    }
    catch (Exception ex)
    {
    }

    string node_id;

    if (role is null)
    {
        node_id            = properties.as!(string)("node_id");
        listener_http_port = 0;
        write_storage_node = null;
    }
    else
    {
        set_g_process_name(role);
    }

//    http_port    = properties.as!(string)("node_id");
//    count_thread = properties.as!(int)("count_thread");
//    int checktime_onto_files = properties.as!(int)("checktime_onto_files");

//    if (checktime_onto_files < 1)
//        checktime_onto_files = 30;
    veda.core.context.Context core_context;

    if (write_storage_node !is null)
        set_g_external_write_storage_url(write_storage_node);

    core_context = veda.core.server.init_core(node_id, role, listener_http_port, write_storage_node);
    if (core_context is null)
    {
        log.trace("ERR! Veda core has not been initialized");
        return;
    }

    Ticket sticket;
    if (sys_ticket_id !is null)
    {
        sticket = *core_context.get_ticket(sys_ticket_id);
        set_global_systicket(sticket);
    }
    else
    {
        sticket = core_context.sys_ticket();
    }

    ushort                count_thread = 4;

    std.concurrency.Tid[] pool;
    for (int i = 0; i < count_thread; i++)
    {
        pool ~= std.concurrency.spawnLinked(&core_thread, node_id, write_storage_node);
        core.thread.Thread.sleep(dur!("msecs")(10));
    }

    bool is_exist_listener = false;

    if (role !is null)
    {
        is_exist_listener = start_http_listener(core_context, pool, listener_http_port);
//        start_http_listener(core_context, pool, 8555);
    }
    else
    {
        Individual node = core_context.get_individual(&sticket, node_id);

        count_thread = cast(ushort)node.getFirstInteger("v-s:count_thread", 4);

        Resources listeners = node.resources.get("v-s:listener", Resources.init);
        foreach (listener_uri; listeners)
        {
            Individual connection = core_context.get_individual(&sticket, listener_uri.uri);

            Resource   transport = connection.getFirstResource("v-s:transport");
            if (transport != Resource.init)
            {
                if (transport.data() == "http")
                {
                    ushort http_port = cast(ushort)connection.getFirstInteger("v-s:port", 8080);
                    is_exist_listener = start_http_listener(core_context, pool, http_port);
                }
            }
        }
//                    start_http_listener(core_context, pool, 8111);
    }

    if (is_exist_listener == false)
    {
        start_http_listener(core_context, pool, 8080);
    }
}

bool start_http_listener(Context core_context, ref std.concurrency.Tid[] pool, ushort http_port)
{
    try
    {
        VedaStorageRest vsr = new VedaStorageRest(pool, core_context);

        auto            settings = new HTTPServerSettings;

        settings.port           = http_port;
        settings.maxRequestSize = 1024 * 1024 * 1000;
        //settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
        //settings.bindAddresses = ["127.0.0.1"];
        settings.errorPageHandler = toDelegate(&view_error);
        //settings.options = HTTPServerOption.parseURL|HTTPServerOption.distribute;

        auto router = new URLRouter;
        router.get("/files/*", &vsr.fileManager);
        router.get("*", serveStaticFiles("public"));
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

        return true;
    }
    catch (Exception ex)
    {
        log.trace("start_http_listener# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
    }
    return false;
}
