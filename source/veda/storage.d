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
    Individual           = 2,
    PropertyOfIndividual = 3
}

Task io_task;

class VedaStorage
{
    immutable(Class)[] owl_classes;
    immutable(Individual)[ string ] onto_individuals;
    Context            context;
    Individual_IO      individual_io;

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
            owl_classes ~= iic;
        }
    }

    void init()
    {
        writeln("START VEDA STORAGE FIBER LISTENER");


        io_task = runTask({
                              immutable(Individual) _empty_Individual = (immutable(Individual)).init;
                              Resources _empty_Resources = Resources.init;
                              while (true)
                              {
                                  receive(
                                          (Command cmd, Function fn, string args, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is null)
                                              {
                                                  if (cmd == Command.Get && fn == Function.AllClasses)
                                                  {
                                                      send(tid, owl_classes);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.Individual)
                                                  {
                                                      immutable(Individual)[] individuals;
                                                      Ticket ticket;

                                                      immutable(Individual) individual = onto_individuals.get(args, _empty_Individual);

                                                      if (individual != _empty_Individual)
                                                          individuals ~= individual;
                                                      else
                                                          individuals ~= individual_io.getIndividual(args, ticket).idup;

                                                      send(tid, individuals);
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
                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_Individual);
                                                      if (individual == _empty_Individual)
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


public static Individual get_individual(string uri)
{
    Tid                     my_task = Task.getThis();

    immutable(Individual)[] individual;
    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.Individual, uri, my_task);
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
