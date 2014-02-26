import std.conv;
import vibe.d;
import veda.storage;
import onto.owl;
import onto.individual;


void show_error(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error)
{
	res.renderCompat!("error.dt",
		HTTPServerRequest, "req",
		HTTPServerErrorInfo, "error")(req, error);
}

void get_classes(HTTPServerRequest req, HTTPServerResponse res)
{
	immutable (Class)[string] classes = get_all_classes();
	string[][string] subClasses;
	foreach(_class; classes.values) {
		auto _superClasses = _class.subClassOf;
		foreach(_superClass; _superClasses) {
			subClasses[_superClass.uri] ~= _class.uri;
		}
	}
	res.renderCompat!("classes.dt",
		HTTPServerRequest, "req",
		immutable (Class)[string], "classes",
		string[][string], "subClasses")(req, classes, subClasses);
}

void get_class(HTTPServerRequest req, HTTPServerResponse res)
{
	get_individual(req, res);
}

void get_individual(HTTPServerRequest req, HTTPServerResponse res)
{
	string uri = req.params["uri"];
	logInfo(uri);
	Individual individual = veda.storage.get_individual(uri);
	logInfo(text(individual));
	res.renderCompat!("individuals.dt",
		HTTPServerRequest, "req",
		Individual, "individual")(req, individual);
}

shared static this()
{
	auto vs = new VedaStorage (); // initialize storage I

	auto settings = new HTTPServerSettings;
	settings.port = 8080; //settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
	settings.errorPageHandler = toDelegate(&show_error);

	auto router = new URLRouter;
	router.get("/", staticTemplate!"index.dt");
	router.get("/classes", &get_classes);
	router.get("/classes/", &get_classes);
	router.get("/classes/:uri", &get_class);
	router.get("/individuals/:uri", &get_individual);
	router.get("*", serveStaticFiles("public"));

	listenHTTP(settings, router);
	logInfo("Please open http://127.0.0.1:8080/ in your browser.");
	
	vs.init(); // initialize storage II
}