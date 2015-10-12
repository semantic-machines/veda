/**
 * сервер
 */
module veda.core.server;

private
{
    import core.thread, std.stdio, std.string, std.c.string, std.outbuffer, std.datetime, std.conv, std.concurrency;
    version (linux) import std.c.linux.linux, core.stdc.stdlib;
    import io.mq_client, io.rabbitmq_client, io.file_reader;
    import util.logger, util.utils, util.load_info;
    import veda.core.scripts, veda.core.context, veda.core.know_predicates, veda.core.log_msg, veda.core.thread_context; 
    import veda.core.define, veda.core.interthread_signals;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import type, az.acl, storage.storage_thread, search.xapian_indexer, veda.onto.individual, veda.onto.resource;
}

logger log;
logger io_msg;

// Called upon a signal from Linux
extern (C) public void sighandler0(int sig) nothrow @system
{
    try
    {
        log.trace_log_and_console("signal %d caught...\n", sig);
        system(cast(char *)("kill -kill " ~ text(getpid()) ~ "\0"));
        //Runtime.terminate();
    }
    catch (Exception ex)
    {
    }
}

extern (C) public void sighandler1(int sig) nothrow @system
{
    try
    {
        printPrettyTrace(stderr);

        string err;
        if (sig == SIGBUS)
            err = "SIGBUS";
        else if (sig == SIGSEGV)
            err = "SIGSEGV";

        log.trace_log_and_console("signal %s caught...\n", err);
        system(cast(char *)("kill -kill " ~ text(getpid()) ~ "\0"));
        //Runtime.terminate();
    }
    catch (Exception ex)
    {
    }
}

static this()
{
    log    = new logger("pacahon", "log", "server");
    io_msg = new logger("pacahon", "io", "server");
}

version (executable)
{
    void main(char[][] args)
    {
        init_core();
        while (true)
            core.thread.Thread.sleep(dur!("seconds")(1000));
    }
}

void commiter(string thread_name, Tid tid, Tid tid_subject_manager, Tid tid_acl_manager)
{
    core.thread.Thread.getThis().name = thread_name;
    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    while (true)
    {
        core.thread.Thread.sleep(dur!("seconds")(10));
        send(tid, CMD.COMMIT, "");
        core.thread.Thread.sleep(dur!("seconds")(1));
        send(tid_subject_manager, CMD.COMMIT);
        core.thread.Thread.sleep(dur!("seconds")(1));
        send(tid_acl_manager, CMD.COMMIT);
    }
}

bool wait_starting_thread(P_MODULE tid_idx, ref Tid[ P_MODULE ] tids)
{
    bool res;
    Tid  tid = tids[ tid_idx ];

    if (tid == Tid.init)
        throw new Exception("wait_starting_thread: Tid=" ~ text(tid_idx) ~ " not found", __FILE__, __LINE__);

    log.trace("START THREAD... : %s", text(tid_idx));
    send(tid, thisTid);
    receive((bool isReady)
            {
                res = isReady;
                //if (trace_msg[ 50 ] == 1)
                log.trace("START THREAD IS SUCCESS: %s", text(tid_idx));
                if (res == false)
                    log.trace("FAIL START THREAD: %s", text(tid_idx));
            });
    return res;
}
//		import io.zmq_io;


Context init_core(string node_id)
{
    if (node_id is null || node_id.length < 2)
        node_id = "v-a:standart_node";

    Backtrace.install(stderr);

    log    = new logger("pacahon", "log", "server");
    io_msg = new logger("pacahon", "io", "server");
    Tid[ P_MODULE ] tids;

    try
    {
//        log.trace_log_and_console("\nPACAHON %s.%s.%s\nSOURCE: commit=%s date=%s\n", veda.core.myversion.major, veda.core.myversion.minor,
//                                  veda.core.myversion.patch, veda.core.myversion.hash, veda.core.myversion.date);
		Individual node;
		Resources roles;

        Context core_context = new PThreadContext(node_id, "core_context", P_MODULE.nop);
        node = core_context.get_individual(null, node_id);
		if (node.getStatus() == ResultCode.OK)
		{
        	log.trace_log_and_console ("VEDA NODE CONFIGURATION: [%s]", node);
   	        roles = node.resources.get("vsrv:role", Resources.init);        	
        }
                        
        tids[ P_MODULE.interthread_signals ] = spawn(&interthread_signals_thread, text(P_MODULE.interthread_signals));
        wait_starting_thread(P_MODULE.interthread_signals, tids);

        tids[ P_MODULE.fulltext_indexer ] =
            spawn(&xapian_indexer, text(P_MODULE.fulltext_indexer), node_id);
        if (wait_starting_thread(P_MODULE.fulltext_indexer, tids) == false)
            return null;

        tids[ P_MODULE.subject_manager ] = spawn(&individuals_manager, text(P_MODULE.subject_manager), individuals_db_path, node_id);
        wait_starting_thread(P_MODULE.subject_manager, tids);

        tids[ P_MODULE.ticket_manager ] = spawn(&individuals_manager, text(P_MODULE.ticket_manager), tickets_db_path, node_id);
        wait_starting_thread(P_MODULE.ticket_manager, tids);

        tids[ P_MODULE.acl_manager ] = spawn(&acl_manager, text(P_MODULE.acl_manager), acl_indexes_db_path);
        wait_starting_thread(P_MODULE.acl_manager, tids);

        tids[ P_MODULE.xapian_thread_context ] = spawn(&xapian_thread_context, text(P_MODULE.xapian_thread_context));
        wait_starting_thread(P_MODULE.xapian_thread_context, tids);

        send(tids[ P_MODULE.fulltext_indexer ], CMD.SET, P_MODULE.subject_manager, tids[ P_MODULE.subject_manager ]);
        send(tids[ P_MODULE.fulltext_indexer ], CMD.SET, P_MODULE.acl_manager, tids[ P_MODULE.acl_manager ]);
        send(tids[ P_MODULE.fulltext_indexer ], CMD.SET, P_MODULE.xapian_thread_context, tids[ P_MODULE.xapian_thread_context ]);

        tids[ P_MODULE.commiter ] =
            spawn(&commiter, text(P_MODULE.commiter), tids[ P_MODULE.fulltext_indexer ], tids[ P_MODULE.subject_manager ],
                  tids[ P_MODULE.acl_manager ]);
        wait_starting_thread(P_MODULE.commiter, tids);

        tids[ P_MODULE.statistic_data_accumulator ] = spawn(&statistic_data_accumulator, text(P_MODULE.statistic_data_accumulator));
        wait_starting_thread(P_MODULE.statistic_data_accumulator, tids);

        tids[ P_MODULE.print_statistic ] = spawn(&print_statistic, text(P_MODULE.print_statistic),
                                                 tids[ P_MODULE.statistic_data_accumulator ]);
        wait_starting_thread(P_MODULE.print_statistic, tids);

        tids[ P_MODULE.fanout ] = spawn(&veda.core.fanout.fanout_thread, text(P_MODULE.fanout), node_id);
        wait_starting_thread(P_MODULE.fanout, tids);

        foreach (key, value; tids)
            register(text(key), value);

        tids[ P_MODULE.condition ] = spawn(&condition_thread, text(P_MODULE.condition));
        wait_starting_thread(P_MODULE.condition, tids);

        register(text(P_MODULE.condition), tids[ P_MODULE.condition ]);
        Tid tid_condition = locate(text(P_MODULE.condition));

		//spawn (&zmq_thread, "");
        //Context core_context = new PThreadContext(node_id, "core_context", P_MODULE.nop);

        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        tids[ P_MODULE.file_reader ] = spawn(&io.file_reader.file_reader_thread, P_MODULE.file_reader, node_id, 5);
        wait_starting_thread(P_MODULE.file_reader, tids);
        
//        io.file_reader.processed(core_context);
		if (node.getStatus() != ResultCode.OK)
		{
        	core_context.reopen_ro_subject_storage_db();
        	core_context.reopen_ro_acl_storage_db();
        	node = core_context.get_individual(null, node_id);
        	
        	log.trace_log_and_console ("VEDA NODE CONFIGURATION:[%s]", node);
        }

        Resources  listeners = node.resources.get("vsrv:listener", Resources.init);
        foreach (listener; listeners)
        {
            Individual connection = core_context.get_individual(null, listener.uri);

            Resource transport = connection.getFirstResource("vsrv:transport");
            if (transport != Resource.init)
            {
                if (transport.data() == "rabbitmq")
                {
                    mq_client rabbitmq_connection = null;

                    // прием данных по каналу rabbitmq
                    log.trace_log_and_console("LISTENER: connect to rabbitmq");

                    try
                    {
                        rabbitmq_connection = new rabbitmq_client();
                        rabbitmq_connection.connect_as_listener(getAsSimpleMapWithoutPrefix(connection));

                        if (rabbitmq_connection.is_success() == true)
                        {
                            //rabbitmq_connection.set_callback(&get_message);

                            ServerThread thread_listener_for_rabbitmq = new ServerThread(&rabbitmq_connection.listener, node_id, "RABBITMQ");

                            thread_listener_for_rabbitmq.start();

//								LoadInfoThread load_info_thread1 = new LoadInfoThread(&thread_listener_for_rabbitmq.getStatistic);
//								load_info_thread1.start();
                        }
                        else
                        {
                            writeln(rabbitmq_connection.get_fail_msg);
                        }
                    } catch (Exception ex)
                    {
                    }
                }
            }
        }

        return core_context;
    } catch (Exception ex)
    {
        writeln("Exception: ", ex.msg);
        return null;
    }
}

class ServerThread : core.thread.Thread
{
    PThreadContext resource;

    this(void delegate() _dd, string node_id, string context_name)
    {
        super(_dd);
        resource = new PThreadContext(node_id, context_name, P_MODULE.nop);

//		resource.sw.start();
    }
}
