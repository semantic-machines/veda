import std.conv, std.stdio, std.file;
import vibe.d;
import properd;
import veda.pacahon_driver;
import veda.storage_rest;


void view_error(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error)
{
    res.renderCompat!("view_error.dt",
                      HTTPServerRequest, "req",
                      HTTPServerErrorInfo, "error")(req, error);
}

void uploadFile(HTTPServerRequest req, HTTPServerResponse res)
{
    auto pf = "file" in req.files;

    enforce(pf !is null, "No file uploaded!");

    auto pt = "path" in req.form;
    if (pt !is null)
    {
        string   pts = cast(string)*pt;

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

        auto path = Path("data/files/" ~ pts ~ "/") ~pf.filename;

        try moveFile(pf.tempPath, path);
        catch (Exception e) {
//                logWarn("Failed to move file to destination folder: %s", e.msg);
//                logInfo("Performing copy+delete instead.");
            copyFile(pf.tempPath, path);
        }

        res.writeBody("File uploaded!", "text/plain");
    }
}


shared static this()
{
    ushort http_port    = 8080;
    int    count_thread = 10;

    string[ string ] properties;

    try
    {
        properties = readProperties("./veda.properties");
    }
    catch (Exception ex)
    {
    }


    http_port    = properties.as!(ushort)("http_port");
    count_thread = properties.as!(int)("count_thread");


    pacahon.server.init_core();

    pacahon.context.Context context;
    string                  thread_name = "veda" ~ text(std.uuid.randomUUID().toHash())[ 0..5 ];
    core.thread.Thread.getThis().name   = thread_name;
    context = new pacahon.thread_context.PThreadContext(pacahon.server.props_file_path, thread_name);

    std.concurrency.Tid[] pool;

    for (int i = 0; i < count_thread; i++)
    {
        pool ~= std.concurrency.spawn(&core_thread);
        core.thread.Thread.sleep(dur!("msecs")(10));
    }

    VedaStorageRest vsr = new VedaStorageRest(pool, context, properties);

    auto            settings = new HTTPServerSettings;
    settings.port           = http_port;
    settings.maxRequestSize = 1024 * 1024 * 1000;
    //settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
    //settings.bindAddresses = ["127.0.0.1"];
    settings.errorPageHandler = toDelegate(&view_error);

    auto router = new URLRouter;
    router.get("/files/*", &vsr.fileManager);
    router.get("*", serveStaticFiles("public"));
    router.get("/", serveStaticFile("public/index.html"));
    router.get("/tests", serveStaticFile("public/tests.html"));
    router.get("/upload-form", staticTemplate !"upload_form.dt");
    router.post("/upload", &uploadFile);

    registerRestInterface(router, vsr);

    logInfo("============ROUTES=============");
    auto routes = router.getAllRoutes();
    logInfo("GET:");
    foreach (route; routes)
    {
        if (route.method == HTTPMethod.GET)
            logInfo(route.pattern);
    }

    logInfo("PUT:");
    foreach (route; routes)
    {
        if (route.method == HTTPMethod.PUT)
            logInfo(route.pattern);
    }
    logInfo("POST:");
    foreach (route; routes)
    {
        if (route.method == HTTPMethod.POST)
            logInfo(route.pattern);
    }
    logInfo("DELETE:");
    foreach (route; routes)
    {
        if (route.method == HTTPMethod.DELETE)
            logInfo(route.pattern);
    }
    logInfo("===============================");

    listenHTTP(settings, router);
    logInfo("Please open http://127.0.0.1:" ~ text(settings.port) ~ "/ in your browser.");
}
