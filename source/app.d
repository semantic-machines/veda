import std.conv;
import vibe.d;
import veda.storage;
import onto.owl;
import onto.individual;


void showError(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error)
{
	//res.render!("error.dt", req, error);
	res.renderCompat!("error.dt",
		HTTPServerRequest, "req",
		HTTPServerErrorInfo, "error")(req, error);
}

void getClasses(HTTPServerRequest req, HTTPServerResponse res)
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

void getIndividuals(HTTPServerRequest req, HTTPServerResponse res)
{
	string uri = req.params["uri"];
	Individual individual = get_individual("mondi-data:SemanticMachines");
	res.renderCompat!("individuals.dt",
		HTTPServerRequest, "req",
		Individual, "individual")(req, individual);
}

shared static this()
{
	// initialize storage I
	auto vs = new VedaStorage ();
	auto settings = new HTTPServerSettings;
	settings.port = 8080;
//	settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
	settings.errorPageHandler = toDelegate(&showError);

	auto router = new URLRouter;
	router.get("/", staticTemplate!"index.dt");
	router.get("/classes", &getClasses);
	router.get("/classes/", &getClasses);
	router.get("/classes/:uri", &getClasses);
	router.get("/individuals/:uri", &getIndividuals);
	router.get("*", serveStaticFiles("public"));

	listenHTTP(settings, router);
	logInfo("Please open http://127.0.0.1:8080/ in your browser.");

	// initialize storage II
	vs.init();
}