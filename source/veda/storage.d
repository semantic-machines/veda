module veda.storage;

import veda.pacahondriver;

import std.stdio, std.datetime, std.conv, std.string;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.context;
import onto.owl;
import onto.individual;
import onto.resource;
import onto.lang;

//////////////////////////////////////////////////// Client API /////////////////////////////////////////////////////////////////

public static Ticket authenticate(string login, string password) {
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

public static bool is_ticket_valid(string ticket) {
    Tid my_task = Task.getThis();
    if (my_task !is null) {
        send(io_task, Command.Is, Function.TicketValid, ticket, my_task);
        bool res = receiveOnly!(bool);
        return res;
    }
    return false;
}

public static Individual[] query(string ticket, string query, byte level = 0) {
    Tid my_task = Task.getThis();
    immutable(Individual)[] individuals;
    if (my_task !is null) {
        send(io_task, Command.Get, Function.IndividualsToQuery, query, level, ticket, my_task);
        individuals = receiveOnly!(immutable(Individual)[]);
    }
    return cast(Individual[])individuals;
}

public static Individual get_individual(string ticket, string uri, byte level = 0) {
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

public static ResultCode put_individual(string ticket, string uri, Individual indv) {
    Tid my_task = Task.getThis();
    if (my_task !is null) {
        immutable(Individual)[] individual;
        individual ~= indv.idup;
        send(io_task, Command.Put, Function.Individual, ticket, uri, individual, my_task);
        ResultCode res = receiveOnly!(ResultCode);
        return res;
    }
    return ResultCode.Service_Unavailable;
}

public static string get_property_value(string ticket, string uri, string property_uri, LANG lang) {
    Tid my_task = Task.getThis();
    string res;
    if (my_task !is null) {
        send(io_task, Command.Get, Function.PropertyOfIndividual, uri, property_uri, lang, my_task);
        res = receiveOnly!(string);
    }
    return res;
}

public static immutable(Class)[ string ] get_classes() {
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

public static Class get_class(string uri) {
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