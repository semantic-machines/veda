import std.conv;
import vibe.d;
import veda.storage;
import onto.owl;

void showError(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error)
{
	//res.render!("error.dt", req, error);
	res.renderCompat!("error.dt",
		HTTPServerRequest, "req",
		HTTPServerErrorInfo, "error")(req, error);
}

void showClasses(HTTPServerRequest req, HTTPServerResponse res)
{
	immutable (Class)[] classes = get_all_classes();
	res.renderCompat!("classes.dt",
		HTTPServerRequest, "req",
		immutable (Class)[], "classes")(req, classes);
}

shared static this()
{
	auto settings = new HTTPServerSettings;
	settings.port = 8080;
	settings.bindAddresses = ["::1", "127.0.0.1"];
	settings.errorPageHandler = toDelegate(&showError);

	auto router = new URLRouter;
	router.get("/", staticTemplate!"index.dt");
	router.get("/classes", &showClasses);
	router.get("*", serveStaticFiles("public"));

	listenHTTP(settings, router);
	logInfo("Please open http://127.0.0.1:8080/ in your browser.");

	// initialize storage
	auto vs = new VedaStorage ();
	vs.init();
}