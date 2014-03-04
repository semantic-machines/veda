import std.conv;
import vibe.d;
import veda.storage;
import onto.owl;
import onto.individual;
import std.datetime;


void show_error(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error) {
	res.renderCompat!("error.dt",
		HTTPServerRequest, "req",
		HTTPServerErrorInfo, "error")(req, error);
}

void get_classes(HTTPServerRequest req, HTTPServerResponse res) {
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

void get_class(HTTPServerRequest req, HTTPServerResponse res) {
	get_individual(req, res);
}

void get_individual(HTTPServerRequest req, HTTPServerResponse res) {
	string uri = req.params["uri"];
	logInfo(uri);
	Individual individual = veda.storage.get_individual(uri);
	res.renderCompat!("individual.dt",
		HTTPServerRequest, "req",
		Individual, "individual")(req, individual);
}

void get_login(HTTPServerRequest req, HTTPServerResponse res) {
	string ticket = req.cookies.get("ticket", "");
	if (!is_ticket_valid(ticket)) {
		res.renderCompat!("login.dt",
			HTTPServerRequest, "req")(req);
	}
}

void login(HTTPServerRequest req, HTTPServerResponse res) {
	string ticket = req.cookies.get("ticket", "");
	if (is_ticket_valid(ticket)) {
		return;
	} else {
		string user;
		string password;
		if (req.method == HTTPMethod.POST) {
			user = req.form.get("user", "");
			password = req.form.get("password","");
		} else if (req.method == HTTPMethod.GET) {
			user = req.query.get("user", "");
			password = req.query.get("password","");
		}
		ticket = get_ticket(user, password);
		if (ticket !is null) {
			res.setCookie("ticket", ticket, "/");
		} else {
			res.setCookie("ticket", null, "/");
			res.renderCompat!("login.dt",
				HTTPServerRequest, "req")(req);
		}
	}
}

void get_search(HTTPServerRequest req, HTTPServerResponse res) {
	//start timer
	StopWatch sw;
	sw.start();
	
	string q = req.query.get("q", "");
	if (q != "") {
		logInfo(q);
		Individual[] individuals = get_individuals_via_query(q, 0);
		
		//stop & log timer & start again
		sw.stop();
		long t = cast(long) sw.peek().msecs;
		logInfo("query execution time:"~text(t)~" msecs");
		sw.reset();
		sw.start();
		
		res.renderCompat!("search_results.dt",
			HTTPServerRequest, "req",
			Individual[], "individuals")(req, individuals);
		
	} else {
		res.renderCompat!("search.dt",
			HTTPServerRequest, "req")(req);
	}

	//stop & log timer
	sw.stop();
	long t = cast(long) sw.peek().msecs;
	logInfo("page rendering time:"~text(t)~" msecs");
}

string get_ticket(string user, string password) {
	return veda.storage.get_ticket(user, password);
}

bool is_ticket_valid(string ticket) {
	return veda.storage.is_ticket_valid(ticket);
}

bool check_credentials(string user, string password) {
	return user == "admin" && password == "admin";
}

shared static this()
{
	auto vs = new VedaStorage (); // initialize storage I

	auto settings = new HTTPServerSettings;
	settings.port = 8080; //settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
	settings.errorPageHandler = toDelegate(&show_error);

	auto router = new URLRouter;
//	router.any("*", performBasicAuth("veda system", toDelegate(&check_credentials)));
	router.get("*", serveStaticFiles("public"));
	router.any("*", &login);
	router.get("/", staticTemplate!"index.dt");
	router.get("/classes", &get_classes);
	router.get("/classes/", &get_classes);
	router.get("/classes/:uri", &get_class);
	router.get("/individuals/:uri", &get_individual);
	router.get("/search", &get_search);
	router.get("/search/", &get_search);
	listenHTTP(settings, router);
	logInfo("Please open http://127.0.0.1:8080/ in your browser.");
	
	vs.init(); // initialize storage II
}