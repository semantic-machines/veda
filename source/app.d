import std.conv;
import vibe.d;
import veda.storage;
import onto.owl;
import onto.lang;
import onto.individual;
import pacahon.context;
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
	string ticket_string = req.cookies.get("ticket", "");
	if (is_ticket_valid(ticket_string)) {
		return;
	} else {
		string login = req.cookies.get("login", "");
		string password = req.cookies.get("password", "");
		Ticket ticket = authenticate(login, password);
		if (ticket != Ticket.init) {
			Cookie ticket_cookie = res.setCookie("ticket", ticket.id, "/");
			ticket_cookie.expires = to_rfc822(SysTime(ticket.end_time, TimeZone.getTimeZone("UTC")));
			//res.setCookie("login", null, "/");
			res.setCookie("password", null, "/");
		} else {
			res.setCookie("ticket", null, "/");
			//res.setCookie("login", null, "/");
			res.setCookie("password", null, "/");
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

bool check_credentials(string login, string password) {
	return login == "admin" && password == "admin";
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