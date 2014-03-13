module veda.storage;

import std.stdio, std.datetime, std.conv, std.string;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;
import onto.individual;
import onto.resource;
import onto.lang;
import util.lmultidigraph;

enum Command
{
    Get,
    Is
}

enum Function
{
    AllClasses,
    Class,
    Individual,
    PropertyOfIndividual,
    IndividualsToQuery,
    NewTicket,
    TicketValid
}

Task io_task;

class VedaStorage
{
    Context context;

    this()
    {
        init_core();
        context = new ThreadContext(props_file_path, "vibe.app");
    }

    void init()
    {
        writeln("START VEDA STORAGE FIBER LISTENER");

        io_task = runTask({
                              immutable(Individual) _empty_iIndividual = (immutable(Individual)).init;
                              immutable(Class) _empty_iClass = (immutable(Class)).init;
                              Resources _empty_Resources = Resources.init;

                              while (true)
                              {
                                  receive(
                                          (Command cmd, Function fn, string arg1, byte arg2, string ticket, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.Individual)
                                                  {
                                                      immutable(Individual)[ string ] onto_individuals =
                                                          context.get_onto_as_map_individuals();
                                                      immutable(Individual)[] individuals;

                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);

                                                      if (individual != _empty_iIndividual)
                                                          individuals ~= individual;
                                                      else
                                                          individuals ~= context.get_individual(arg1, ticket, arg2).idup;

                                                      send(tid, individuals);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.IndividualsToQuery)
                                                  {
//        StopWatch sw;
//        sw.start();
                                                      immutable(Individual)[] individuals =
                                                          context.get_individuals_via_query(arg1, ticket, arg2);

//                sw.stop();
//                long t = cast(long) sw.peek().msecs;
//                logInfo("@L2 query execution time:"~text(t)~" msecs");

                                                      send(tid, individuals);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string args, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.AllClasses)
                                                  {
                                                      send(tid, context.get_owl_classes().values);
                                                  }
                                                  else if (cmd == Command.Is && fn == Function.TicketValid)
                                                  {
                                                      bool res = context.is_ticket_valid(args);
                                                      send(tid, res);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.Class)
                                                  {
                                                      immutable(Class)[] classes;
                                                      Ticket ticket;

                                                      immutable(Class) classz = context.get_owl_classes().get(args, _empty_iClass);

                                                      if (classz != _empty_iClass)
                                                          classes ~= classz;

                                                      send(tid, classes);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string arg1, string arg2, Tid tid) {
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.NewTicket)
                                                  {
                                                      immutable(Ticket)[] tickets;
                                                      Ticket ticket = context.authenticate(arg1, arg2);
                                                      tickets ~= ticket;

                                                      send(tid, tickets.idup);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string arg1, string arg2, string ticket, LANG lang, Tid tid) {
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.PropertyOfIndividual)
                                                  {
                                                      string res1;
                                                      immutable(Individual)[ string ] onto_individuals =
                                                          context.get_onto_as_map_individuals();
                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);
                                                      if (individual == _empty_iIndividual)
                                                      {
                                                          immutable(Individual) individual1 =
                                                              context.get_individual(arg1, ticket).idup;
                                                          immutable Resources res = individual1.resources.get(arg2, Resources.init);

                                                          foreach (rr; res)
                                                          {
                                                              if (rr.lang == lang)
                                                              {
                                                                  res1 = rr.data;
                                                                  break;
                                                              }
                                                          }
                                                      }
                                                      else
                                                      {
                                                          immutable Resources res = individual.resources.get(arg2, Resources.init);

                                                          foreach (rr; res)
                                                          {
                                                              if (rr.lang == lang)
                                                              {
                                                                  res1 = rr.data;
                                                                  break;
                                                              }
                                                          }
                                                      }

                                                      send(tid, res1);
                                                  }
                                              }
                                          },
                                          (int msg, Tid tid) {
                                              logInfo("Received int message: %s", msg);
                                          });
                              }
                          });
    }
}

//////////////////////////////////////////////////// Client API /////////////////////////////////////////////////////////////////

public static Ticket authenticate(string login, string password)
{
    Tid my_task = Task.getThis();

    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.NewTicket, login, password, my_task);
        immutable(Ticket)[] tickets = receiveOnly!(immutable(Ticket)[]);
        if (tickets.length > 0)
            return tickets[ 0 ];
    }

    return Ticket.init;
}

public static bool is_ticket_valid(string ticket)
{
    Tid my_task = Task.getThis();

    if (my_task !is null)
    {
        send(io_task, Command.Is, Function.TicketValid, ticket, my_task);
        bool res = receiveOnly!(bool);
        return res;
    }

    return false;
}

public static Individual[] get_individuals_via_query(string query, string ticket, byte level = 0)
{
    Tid                     my_task = Task.getThis();

    immutable(Individual)[] individuals;
    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.IndividualsToQuery, query, level, ticket, my_task);
        individuals = receiveOnly!(immutable(Individual)[]);
    }

    return cast(Individual[])individuals;
}

public static Individual get_individual(string uri, string ticket, byte level = 0)
{
    Tid                     my_task = Task.getThis();

    immutable(Individual)[] individual;
    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.Individual, uri, level, ticket, my_task);
        individual = receiveOnly!(immutable(Individual)[]);
        if (individual.length > 0)
            return cast(Individual)individual[ 0 ];
    }

    return Individual.init;
}

public static string get_single_property_value_of_individual(string uri, string property_uri, LANG lang)
{
    Tid    my_task = Task.getThis();

    string res;

    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.PropertyOfIndividual, uri, property_uri, lang, my_task);
        res = receiveOnly!(string);
    }

    return res;
}

public static immutable(Class)[ string ] get_all_classes()
{
    Tid my_task = Task.getThis();

    immutable(Class)[ string ] res;

    immutable(Class)[] classes;

    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.AllClasses, "", my_task);
        classes = receiveOnly!(immutable(Class)[]);
        foreach (clasz; classes)
            res[ clasz.uri ] = clasz;

        res.rehash();
    }

    return res;
}

public static Class get_class(string uri)
{
    Tid                my_task = Task.getThis();

    immutable(Class)[] classes;

    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.Class, uri, my_task);
        classes = receiveOnly!(immutable(Class)[]);
    }

    if (classes.length > 0)
        return cast(Class)classes[ 0 ];

    return Class.init;
}
