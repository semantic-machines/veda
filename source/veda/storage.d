module veda.storage;

import std.stdio, std.datetime, std.conv;
import vibe.core.concurrency;
import vibe.core.core;
import vibe.core.log;
import vibe.core.task;

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
    immutable(Class[]) owl_classes;
    Context context;

	this ()
	{
		init_core ();
		core.thread.Thread.sleep(dur!("msecs")(50));
		context = new ThreadContext(props_file_path, "vibe.app");    

		foreach (cl ; context.owl_classes)
		{
				immutable Class iic = cl.idup;
				owl_classes ~= iic;
		}
	}

    void init ()
    {	
	writeln ("start veda storage listener");

    	io_task = runTask({
                while (true) {
			writeln ("recvieve 1");
                        receive(
                                (Command cmd, Function fn, string args, Tid tid) {
					writeln ("Tid=", cast(void*)tid);    
                                        logInfo("Received string message: %s", args);
					if (tid !is null)
					    send (tid, owl_classes);    
                                },
                                (int msg, Tid tid) {
                                        logInfo("Received int message: %s", msg);
                                });
                }
        });

	get_all_classes ();
    }

}

    public static immutable(Class[]) get_all_classes ()
    {
	Tid my_task = Task.getThis();
	writeln ("@@@@my tid=", cast(void*)my_task);
	if (my_task !is null)
	{
	    send (io_task, Command.Get, Function.AllClasses, "all", my_task);
	}

//	foreach (cl ; owl_classes)
//	{
//		writeln ("*** CL.label=", cl.label);	    
//	}
	return (immutable Class[]).init;
    }
