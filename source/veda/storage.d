module veda.storage;

import std.stdio, std.datetime, std.conv;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;
import onto.individual;
import onto.resource;
import util.lmultidigraph;

enum Command
{
    Get = 1
}

enum Function
{
    AllClasses           = 1,
    Class                = 2,
    Individual           = 3,
    PropertyOfIndividual = 4,
    IndividualsToQuery   = 5
}

Task io_task;

class VedaStorage
{
    immutable(Class)[ string ] owl_classes;
    immutable(Individual)[ string ] onto_individuals;
    Context       context;
    Individual_IO individual_io;

    this()
    {
        init_core();
//        core.thread.Thread.sleep(dur!("msecs")(0));
        context          = new ThreadContext(props_file_path, "vibe.app");
        individual_io    = new Individual_IO(context);
        onto_individuals = context.get_onto_as_map_individuals();

        foreach (cl; context.owl_classes)
        {
            immutable Class iic = cl.idup;
            owl_classes[ iic.uri ] = iic;
        }
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
                                          (Command cmd, Function fn, string arg1, byte arg2, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.Individual)
                                                  {
                                                      immutable(Individual)[] individuals;
                                                      Ticket ticket;

                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);

                                                      if (individual != _empty_iIndividual)
                                                          individuals ~= individual;
                                                      else
                                                          individuals ~= individual_io.getIndividual(arg1, ticket, arg2).idup;

                                                      send(tid, individuals);
                                                  }
						  else if (cmd == Command.Get && fn == Function.IndividualsToQuery)
						  {
                                                      immutable(Individual)[] individuals;
                                                      Ticket ticket;
						    
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
                                                      send(tid, owl_classes.values);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.Class)
                                                  {
                                                      immutable(Class)[] classes;
                                                      Ticket ticket;

                                                      immutable(Class) classz = owl_classes.get(args, _empty_iClass);

                                                      if (classz != _empty_iClass)
                                                          classes ~= classz;

                                                      send(tid, classes);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string arg1, string arg2, LANG lang, Tid tid) {
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.PropertyOfIndividual)
                                                  {
                                                      Ticket ticket;

                                                      string res1;
                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);
                                                      if (individual == _empty_iIndividual)
                                                      {
                                                          immutable(Individual) individual1 =
                                                              individual_io.getIndividual(arg1, ticket).idup;
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
public static immutable(Individual)[] get_individuals_to_query(string query, byte level = 0)
{
    Tid                     my_task = Task.getThis();

    immutable(Individual)[] individuals;
    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.IndividualsToQuery, query, level, my_task);
        individuals = receiveOnly!(immutable(Individual)[]);
    }

    return individuals;
}

public static Individual get_individual(string uri, byte level = 0)
{
    Tid                     my_task = Task.getThis();

    immutable(Individual)[] individual;
    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.Individual, uri, level, my_task);
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
