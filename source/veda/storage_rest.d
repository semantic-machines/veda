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

//////////////////////////////////////////////////// Rest API /////////////////////////////////////////////////////////////////

interface VedaStorageRest_API {

	@path("authenticate") @method(HTTPMethod.GET)
	Ticket authenticate(string login, string password);

	@path("is_ticket_valid") @method(HTTPMethod.GET)
	bool is_ticket_valid(string ticket);

	@path("query") @method(HTTPMethod.GET)
	Individual[] query(string ticket, string query = "rdf", byte level = 0);

	@path("individual") @method(HTTPMethod.GET)
	Individual get_individual(string ticket, string uri, byte level = 0);
	
	@path("individual") @method(HTTPMethod.PUT)
	ResultCode put_individual(string ticket, string uri, Individual indv);
	
	@path("individual") @method(HTTPMethod.GET)
	string get_property_value(string ticket, string uri, string property_uri, LANG lang);

	@path("classes") @method(HTTPMethod.GET)
	immutable(Class)[ string ] get_classes();

	@path("classes") @method(HTTPMethod.GET)
	Class get_class(string uri);

	@path("execute_script") @method(HTTPMethod.POST)
	string[2] execute_script(string script);
}

class VedaStorageRest : VedaStorageRest_API {
override:
Ticket authenticate(string login, string password) {
    Tid my_task = Task.getThis();
    if (my_task !is null) {
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
    if (my_task !is null) {
        send(io_task, Command.Is, Function.TicketValid, ticket, my_task);
        bool res = receiveOnly!(bool);
        return res;
    }
    return false;
}

Individual[] query(string ticket, string query, byte level = 0) {
    Tid my_task = Task.getThis();
    immutable(Individual)[] individuals;
    if (my_task !is null) {
        send(io_task, Command.Get, Function.IndividualsToQuery, query, level, ticket, my_task);
        individuals = receiveOnly!(immutable(Individual)[]);
    }
    return cast(Individual[])individuals;
}

Individual get_individual(string ticket, string uri, byte level = 0) {
    Tid my_task = Task.getThis();
    if (my_task !is null) {
        immutable(Individual)[] individual;
        send(io_task, Command.Get, Function.Individual, uri, level, ticket, my_task);
        individual = receiveOnly!(immutable(Individual)[]);
        if (individual.length > 0) {
            return cast(Individual)individual[ 0 ];
		}
    }
    return Individual.init;
}

ResultCode put_individual(string ticket, string uri, Individual individual) {
    Tid my_task = Task.getThis();
    if (my_task !is null) {
        immutable(Individual)[] ind;
        ind ~= individual.idup;
        send(io_task, Command.Put, Function.Individual, ticket, uri, ind, my_task);
        ResultCode res = receiveOnly!(ResultCode);
        return res;
    }
    return ResultCode.Service_Unavailable;
}

string get_property_value(string ticket, string uri, string property_uri, LANG lang) {
    Tid my_task = Task.getThis();
    string res;
    if (my_task !is null) {
        send(io_task, Command.Get, Function.PropertyOfIndividual, uri, property_uri, lang, my_task);
        res = receiveOnly!(string);
    }
    return res;
}

immutable(Class)[ string ] get_classes() {
    Tid my_task = Task.getThis();
    immutable(Class)[ string ] res;
    immutable(Class)[] classes;
    if (my_task !is null) {
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
    if (my_task !is null) {
        send(io_task, Command.Get, Function.Class, uri, my_task);
        classes = receiveOnly!(immutable(Class)[]);
    }
    if (classes.length > 0) {
    	return cast(Class)classes[ 0 ];
    }
    return Class.init;
}

string[2] execute_script(string script) {
	string[2] res;
	logInfo(script);
    Tid my_task = Task.getThis();

    if (my_task !is null) 
    {
        send(io_task, Command.Execute, Function.Script, script, my_task);
        res[0] = receiveOnly!(string);
    }

    return res;
}

}