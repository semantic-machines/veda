module veda.storage;

import std.stdio, std.datetime, std.conv;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;
import onto.individual;
import util.lmultidigraph;

enum Command
{
    Get = 1
}

enum Function
{
    AllClasses = 1,
    Individual
}

Task io_task;

class VedaStorage
{
    immutable(Class)[] owl_classes;
    Context            context;
    Individual_IO      individual_io;

    this()
    {
        init_core();
//        core.thread.Thread.sleep(dur!("msecs")(0));
        context = new ThreadContext(props_file_path, "vibe.app");

        foreach (cl; context.owl_classes)
        {
            immutable Class iic = cl.idup;
            owl_classes ~= iic;
            writeln(iic);
        }

        individual_io = new Individual_IO(context);
    }

    void init()
    {
        writeln("START VEDA STORAGE FIBER LISTENER");

        io_task = runTask({
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
                                                      individuals ~= individual_io.getIndividual(args, ticket).idup;
                                                      send(tid, individuals);
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
