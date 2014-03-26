import std.conv;
import vibe.d;
import veda.pacahon_driver;
import veda.storage_rest;
import onto.owl;
import onto.lang;
import onto.individual;
import pacahon.context;
import std.datetime;

void view_error(HTTPServerRequest req, HTTPServerResponse res, HTTPServerErrorInfo error) {
	res.renderCompat!("view_error.dt",
		HTTPServerRequest, "req",
		HTTPServerErrorInfo, "error")(req, error);
}

void view_classes(HTTPServerRequest req, HTTPServerResponse res) {
	auto storage = new VedaStorageRest();
	immutable (Class)[string] classes = storage.get_classes();
	string[][string] subClasses;
	foreach(_class; classes.values) {
		auto _superClasses = _class.subClassOf;
		foreach(_superClass; _superClasses) {
			subClasses[_superClass.uri] ~= _class.uri;
		}
	}
	res.renderCompat!("view_classes.dt",
		HTTPServerRequest, "req",
		immutable (Class)[string], "classes",
		string[][string], "subClasses")(req, classes, subClasses);
}

void view_class(HTTPServerRequest req, HTTPServerResponse res) {
	view_individual(req, res);
}

void view_individual(HTTPServerRequest req, HTTPServerResponse res) {
	auto storage = new VedaStorageRest();
	string uri = req.params.get("uri", "");
	string ticket = req.cookies.get("ticket", "");
	Individual individual = storage.get_individual(ticket, uri, 0);
	res.renderCompat!("view_individual.dt",
		HTTPServerRequest, "req",
		Individual, "individual",
		string, "ticket")(req, individual, ticket);
}

void view_individual2(HTTPServerRequest req, HTTPServerResponse res) {
	string uri = req.params.get("uri", "");
	string ticket = req.cookies.get("ticket", "");
	res.renderCompat!("view_individual2.dt",
		HTTPServerRequest, "req",
		string, "uri",
		string, "ticket")(req, uri, ticket);
}

void view_popover(HTTPServerRequest req, HTTPServerResponse res) {
	string uri = req.params["uri"];
	string ticket = req.cookies.get("ticket", "");
	auto storage = new VedaStorageRest();
	Individual individual = storage.get_individual(ticket, uri, 0);
	res.renderCompat!("view_popover.dt",
		HTTPServerRequest, "req",
		Individual, "individual",
		string, "ticket")(req, individual, ticket);
}

void index(HTTPServerRequest req, HTTPServerResponse res) {
	string ticket = req.cookies.get("ticket", "");
	res.renderCompat!("index.dt",
		HTTPServerRequest, "req",
		string, "ticket")(req, ticket);
}

void logout(HTTPServerRequest req, HTTPServerResponse res) {
	res.setCookie("ticket", null, "/");
	res.setCookie("password", null, "/");
	res.redirect("/");
}

string to_rfc822(SysTime time) {
	//Example: Tue, 15-Jan-2013 21:47:38 GMT
	string[] weekdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
	string  Www = weekdays[time.dayOfWeek], 
			dd  = text(time.day), 
			Mmm = text(time.month), 
			yyyy= text(time.year), 
			hh  = text(time.hour), 
			mm  = text(time.minute), 
			ss  = text(time.second), 
			tz  = time.timezone.stdName;
	return Www~", "~dd~"-"~Mmm~"-"~yyyy~" "~hh~":"~mm~":"~ss~" "~tz;
}

void login(HTTPServerRequest req, HTTPServerResponse res) {
	auto storage = new VedaStorageRest();
	string ticket_string = req.cookies.get("ticket", "");
	if (storage.is_ticket_valid(ticket_string)) return;
	else {
		string login = req.cookies.get("login", "");
		string password = req.cookies.get("password", "");
		Ticket ticket = storage.authenticate(login, password);
		if (ticket != Ticket.init) {
			Cookie ticket_cookie = res.setCookie("ticket", ticket.id, "/");
			//ticket_cookie.expires = to_rfc822(SysTime(ticket.end_time, TimeZone.getTimeZone("UTC")));
			res.setCookie("password", null, "/");
		} else {
			res.setCookie("ticket", null, "/");
			res.setCookie("password", null, "/");
			res.renderCompat!("login.dt",
				HTTPServerRequest, "req")(req);
		}
	}
}

void search(HTTPServerRequest req, HTTPServerResponse res) {
	auto storage = new VedaStorageRest();
	//start timer
	StopWatch sw;
	sw.start();
	string ticket = req.cookies.get("ticket", "");
	string query = req.query.get("query", "");

	if (query != "") {
		logInfo(query);
		Individual[] individuals = storage.query(ticket, query, 0);
		
		//stop & log timer & start again
		sw.stop();
		long t = cast(long) sw.peek().msecs;
		logInfo("query execution time:"~text(t)~" msecs");
		sw.reset();
		sw.start();
		
		res.renderCompat!("search.dt",
			HTTPServerRequest, "req",
			Individual[], "individuals",
			string, "ticket")(req, individuals, ticket);
		
	} else {
		Individual[] individuals;
		res.renderCompat!("search.dt",
			HTTPServerRequest, "req",
			Individual[], "individuals",
			string, "ticket")(req, individuals, ticket);
	}

	//stop & log timer
	sw.stop();
	long t = cast(long) sw.peek().msecs;
	logInfo("page rendering time:"~text(t)~" msecs");
}

shared static this()
{
	// initialize storage
	auto pacahon = new PacahonDriver ();
	pacahon.init(); 

	auto settings = new HTTPServerSettings;
	settings.port = 8080; //settings.bindAddresses = ["::1", "127.0.0.1", "172.17.35.148"];
	settings.bindAddresses = ["127.0.0.1"];
	settings.errorPageHandler = toDelegate(&view_error);

	auto router = new URLRouter;
	router.get("*", serveStaticFiles("public"));
	router.get("/logout", &logout);

	registerRestInterface(router, new VedaStorageRest());

	router.any("*", &login);
	//router.get("/", &index);
	router.get("/", serveStaticFile("public/index.html"));
	router.get("/view_classes", &view_classes);
	router.get("/view_classes/", &view_classes);
	router.get("/view_class/:uri", &view_class);
	router.get("/view_individual/:uri", &view_individual);
	router.get("/view_individual2/:uri", &view_individual2);
	router.get("/view_popover/:uri", &view_popover);
	router.get("/search", &search);
	router.get("/search/", &search);


	logInfo("============ROUTES=============");
	auto routes = router.getAllRoutes();
	logInfo("GET:");
	foreach(key, value; routes[HTTPMethod.GET]) {
		logInfo(text(key) ~ ": " ~ text(value));
	}
	logInfo("PUT:");
	foreach(key, value; routes[HTTPMethod.PUT]) {
		logInfo(text(key) ~ ": " ~ text(value));
	}
	logInfo("POST:");
	foreach(key, value; routes[HTTPMethod.POST]) {
		logInfo(text(key) ~ ": " ~ text(value));
	}
	logInfo("DELETE:");
	foreach(key, value; routes[HTTPMethod.DELETE]) {
		logInfo(text(key) ~ ": " ~ text(value));
	}
	logInfo("===============================");

	listenHTTP(settings, router);
	logInfo("Please open http://127.0.0.1:8080/ in your browser.");
	
/*	setTimer(dur!"seconds"(1), {
		scope(exit)
			exitEventLoop(true);
		StopWatch sw;
		sw.start();
		logInfo("PERFORMANCE TEST:");
		int count = 1000000;
		for(int i=0; i<count; i++) {
			//Individual[] individuals = veda.storage.query("53e5cc22-7750-48fe-a772-9b155ceb2b16", "rdf", 0);
			//auto storage = new RestInterfaceClient!VedaStorageRest_API("http://127.0.0.1:8080/");
			auto storage = new VedaStorageRest();
			Individual[] individuals = storage.query("53e5cc22-7750-48fe-a772-9b155ceb2b16", "rdf", 0);
		}
		sw.stop();
		long t = cast(long) sw.peek().msecs;
		logInfo("test execution time:"~text(t)~" msecs");
		logInfo("median time per query:"~text(cast(double) t/count)~" msecs");
	});*/
}