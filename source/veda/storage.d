module veda.storage;

import std.stdio;
import vibe.core.concurrency;
import vibe.core.core;
import vibe.core.log;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;

class VedaStorage 
{
	immutable(Class)[] owl_classes;
	Context context;
	Task io_task;

    this ()
    {
	init_core ();
//	context = new ThreadContext(null, "vibe.app");    
    }    

    /*Taskinit ()
    {	
		foreach (cl ; context.owl_classes)
		{
	    	writeln ("*** CL=", cl);	    
		}


		writeln ("start io_task");

        io_task = runTask({
                while (true) {
			writeln ("recvieve 1");
                        logDebug("receive1");
                        receive(
                                (string msg) {
                                        logInfo("Received string message: %s", msg);
                                },
                                (int msg) {
                                        logInfo("Received int message: %s", msg);
                                });
                }
        });

	return io_task; 
    }*/
}