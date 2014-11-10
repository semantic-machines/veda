import std.conv, std.stdio, std.file;
import vibe.d;
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
    // initialize storage
    auto pacahon = new PacahonDriver();
    pacahon.init();

    VedaStorageRest vsr = new VedaStorageRest();

    auto            settings = new HTTPServerSettings;
    settings.port           = 8081;
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
    logInfo("Please open http://127.0.0.1:8080/ in your browser.");
}