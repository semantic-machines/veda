module veda.storage_rest;

import vibe.d;
import veda.pacahon_driver;

import std.stdio, std.datetime, std.conv, std.string;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.context;
import onto.owl;
import onto.individual;
import onto.resource;
import onto.lang;

static LANG[string] Lang;
static ResourceType[string] Resource_type;

static this() {
	Lang = [ 
		"NONE": LANG.NONE,	"none" : LANG.NONE,
		"RU"  : LANG.RU,	"ru"   : LANG.RU,
		"EN"  : LANG.EN,	"en"   : LANG.EN
	];

	Resource_type = [ 
		"Uri" 		: ResourceType.Uri,
		"String" 	: ResourceType.String,
		"Integer" 	: ResourceType.Integer,
		"Datetime" 	: ResourceType.Datetime,
		"Float" 	: ResourceType.Float,
		"Boolean" 	: ResourceType.Boolean
	];
}

Json individual_to_json(immutable(Individual) individual) {
	Json json = Json.emptyObject;
	json["@"] = individual.uri;
	foreach (property_name, property_values; individual.resources) {
		Json resources_json = Json.emptyArray;
		foreach (property_value; property_values) resources_json ~= resource_to_json(property_value);
		json[property_name] = resources_json;
	}
	return json;
}

Individual json_to_individual(const Json individual_json) {
	
	Individual individual = Individual.init;

	foreach (string property_name, ref const property_values; individual_json) {
		if (property_name == "@") individual.uri = to!string(individual_json[property_name]);
		Resource[] resources = Resource[].init;
		foreach (property_value; property_values) resources ~= json_to_resource(property_value);
		individual.resources[property_name] = resources;
	}
	return individual;
}

Json resource_to_json(Resource resource) {
	Json resource_json = Json.emptyObject;
	resource_json["type"] = text(resource.type);
	if (resource.type == ResourceType.Uri || resource.type == ResourceType.Datetime ) resource_json["data"] = resource.data;
	else if (resource.type == ResourceType.String) 
	{ 
		resource_json["data"] = resource.data; 
		if (resource.lang != LANG.NONE) resource_json["lang"] = text(resource.lang);
	}
	else if (resource.type == ResourceType.Integer) resource_json["data"] = parse!int(resource.data);
	else if (resource.type == ResourceType.Float) resource_json["data"] = parse!double(resource.data);
	else if (resource.type == ResourceType.Float) resource_json["data"] = parse!bool(resource.data);
	else resource_json["data"] = Json.undefined;
	return resource_json;
}

Resource json_to_resource(const Json resource_json) {
	Resource resource = Resource.init;
	resource.type = Resource_type.get(resource_json["type"].get!string, ResourceType.String);
	resource.lang = Lang.get(resource_json["lang"].get!string, LANG.NONE);
	resource.data = resource_json["data"].get!string;
	return resource;
}

//////////////////////////////////////////////////// Rest API /////////////////////////////////////////////////////////////////

interface VedaStorageRest_API {

	@path("authenticate") @method(HTTPMethod.GET)
	Ticket authenticate(string login, string password);

	@path("is_ticket_valid") @method(HTTPMethod.GET)
	bool is_ticket_valid(string ticket);

	@path("query") @method(HTTPMethod.GET)
	string[] query(string ticket, string query);

	@path("query_") @method(HTTPMethod.GET)
	Individuals query_(string ticket, string query);

	@path("get_individuals") @method(HTTPMethod.POST)
	Json[] get_individuals(string ticket, string[] uris);

	@path("get_individual_") @method(HTTPMethod.GET)
	Individual get_individual_(string ticket, string uri);
	
	@path("put_individuals") @method(HTTPMethod.PUT)
	ResultCode put_individuals(string ticket, Json[] individuals_json);
	
	@path("get_property_values") @method(HTTPMethod.GET)
	Json[] get_property_values(string ticket, string uri, string property_uri);

	@path("execute_script") @method(HTTPMethod.POST)
	string[2] execute_script(string script);
	
	@path("get_classes") @method(HTTPMethod.GET)
	immutable(Class)[ string ] get_classes();

	@path("get_class") @method(HTTPMethod.GET)
	Class get_class(string uri);
}

class VedaStorageRest : VedaStorageRest_API {
override:
Ticket authenticate(string login, string password) {

    Tid my_task = Task.getThis();
    if (my_task !is Tid.init) {
        send(io_task, Command.Get, Function.NewTicket, login, password, my_task);
        immutable(Ticket)[] tickets = receiveOnly!(immutable(Ticket)[]);
        if (tickets.length > 0) {
        	return tickets[ 0 ];
        }
    }
    return Ticket.init;
}

bool is_ticket_valid(string ticket) {
    Tid my_task = Task.getThis();
    if (my_task !is Tid.init) {
        send(io_task, Command.Is, Function.TicketValid, ticket, my_task);
        bool res = receiveOnly!(bool);
        return res;
    }
    return false;
}

string[] query(string ticket, string query) {
    Tid my_task = Task.getThis();
    string[] individuals_ids;
    if (my_task !is Tid.init) {
        send(io_task, Command.Get, Function.IndividualsIdsToQuery, query, ticket, my_task);
        individuals_ids = receiveOnly!(string[]);
    }
    return individuals_ids;
}

Individuals query_(string ticket, string query) {
    Tid my_task = Task.getThis();
    immutable(Individual)[] individuals;
    if (my_task !is Tid.init) {
        send(io_task, Command.Get, Function.IndividualsToQuery, query, ticket, my_task);
        individuals = receiveOnly!(immutable(Individual)[]);
    }
    return cast(Individual[])individuals;
}

Json[] get_individuals(string ticket, string[] uris) {
    Tid my_task = Task.getThis();
    immutable(Individual)[] individuals = Individual[].init;
    if (my_task !is Tid.init) {
        immutable(Individual)[] individual;
        send(io_task, Command.Get, Function.Individuals, uris.idup, ticket, my_task);
        individuals = receiveOnly!(immutable(Individual)[]);
    }
    Json[] json = Json[].init;
    foreach (individual; individuals) json ~= individual_to_json(individual);
    return json;
}

Individual get_individual_(string ticket, string uri) {
    Tid my_task = Task.getThis();
    Individual result = Individual.init;
    if (my_task !is Tid.init) {
        immutable(Individual)[] individual;
        send(io_task, Command.Get, Function.Individual, uri, ticket, my_task);
        individual = receiveOnly!(immutable(Individual)[]);
        if (individual.length > 0) {
            result = cast(Individual)individual[ 0 ];
		}
    }
    return result;
}

ResultCode put_individuals(string ticket, Json[] individuals_json) {
//    Tid my_task = Task.getThis();
//    if (my_task !is Tid.init) {
//        immutable(Individual)[] ind;
//        ind ~= individual.idup;
//        send(io_task, Command.Put, Function.Individuals, ticket, uri, ind, my_task);
//        ResultCode res = receiveOnly!(ResultCode);
//        return res;
//    }
    return ResultCode.Service_Unavailable;
}

Json[] get_property_values(string ticket, string uri, string property_uri) {
//    Tid my_task = Task.getThis();
    string res;
//    if (my_task !is Tid.init) {
//        send(io_task, Command.Get, Function.PropertyOfIndividual, uri, property_uri, lang, my_task);
//        res = receiveOnly!(string);
//    }
//    return res;
return Json[].init;
}

string[2] execute_script(string script) {
    string[2] res;
    Tid my_task = Task.getThis();

    if (my_task !is Tid.init) 
    {
        send(io_task, Command.Execute, Function.Script, script, my_task);

        res = receiveOnly!(string[2]);
    }

    return res;
}

immutable(Class)[ string ] get_classes() {
    Tid my_task = Task.getThis();
    immutable(Class)[ string ] res;
    immutable(Class)[] classes;
    if (my_task !is Tid.init) {
        send(io_task, Command.Get, Function.AllClasses, "", my_task);
        classes = receiveOnly!(immutable(Class)[]);
        foreach (clasz; classes) {
            res[ clasz.uri ] = clasz;
        }
        res.rehash();
    }
    return res;
}

Class get_class(string uri) {
    Tid my_task = Task.getThis();
    immutable(Class)[] classes;
    if (my_task !is Tid.init) {
        send(io_task, Command.Get, Function.Class, uri, my_task);
        classes = receiveOnly!(immutable(Class)[]);
    }
    if (classes.length > 0) {
    return cast(Class)classes[ 0 ];
    }
    return Class.init;
}

}