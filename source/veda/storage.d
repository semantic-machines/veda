module veda.storage;

import std.stdio, std.datetime, std.conv;
import vibe.core.concurrency;
import vibe.core.core;
import vibe.core.log;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;
import util.lmultidigraph;

class VedaStorage 
{
    immutable(Class)[] owl_classes;
    Context context;
    Task io_task;

    this ()
    {
    }    

    void init ()
    {	
	init_core ();
	core.thread.Thread.sleep(dur!("msecs")(50));
	context = new ThreadContext(props_file_path, "vibe.app");    

	writeln ("*** INIT");	    
	foreach (cl ; context.owl_classes)
	{
		immutable Class iic = cl.idup;
		owl_classes ~= iic;
		writeln ("*** CL.label=", iic.label);	    
	}


	writeln ("start io_task");

    	io_task = runTask({
                while (true) {
			writeln ("recvieve 1");
                        logDebug("receive1");
                        receive(
                                (string msg, Tid tid) {
                                        logInfo("Received string message: %s", msg);
					send (tid, owl_classes);    
                                },
                                (int msg, Tid tid) {
                                        logInfo("Received int message: %s", msg);
                                });
                }
        });

    }
}