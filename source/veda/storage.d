module veda.storage;

import std.stdio, std.datetime, std.conv;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;
import util.lmultidigraph;

enum Command
{
    Get = 1
}

enum Function
{
    AllClasses = 1
}

Task io_task;

class VedaStorage
{
    immutable(Class)[] owl_classes;
    Context            context;

    this()
    {
        init_core();
        core.thread.Thread.sleep(dur!("msecs")(50));
        context = new ThreadContext(props_file_path, "vibe.app");

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
                              while (true)
                              {
                                  receive(
                                          (Command cmd, Function fn, string args, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
//                                              if (tid !is null)
					      {	
//						if (cmd == Command.Get && fn == Function.AllClasses)	
                                                  send(tid, owl_classes);
					      }	
                                          },
                                          (int msg, Tid tid) {
                                              logInfo("Received int message: %s", msg);
                                          });
                              }
                          });

    }
}

public static immutable(Class)[] get_all_classes()
{
    immutable(Class)[] classes;

    Tid my_task = Task.getThis();

    if (my_task !is null)
    {
        send(io_task, Command.Get, Function.AllClasses, "", my_task);
        classes = receiveOnly!(immutable(Class)[]);
    }

    return classes;
}
