module veda.vmodule.vmodule;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;
    import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, core.thread, std.algorithm : remove;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue, veda.util.container;
    import veda.common.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
    import veda.core.common.context, veda.util.tools, veda.onto.onto, veda.util.module_info;
    import kaleidic.nanomsg.nano;
}

bool f_listen_exit = false;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;

// ////// ////// ///////////////////////////////////////////

extern (C) void handleTermination(int _signal)
{
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));
    
    if (_log !is null)
	    _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    //_log.close();

    writeln("!SYS: ", process_name, ": preparation for the exit.");

    f_listen_exit = true;
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}


class VedaModule // : WSLink
{
    long   last_committed_op_id;
    long   last_check_time;

    int    sock;
    string notify_channel_url     = "tcp://127.0.0.1:9111\0";
    bool   already_notify_channel = false;

    Cache!(string, string) cache_of_indv;

    ModuleInfoFile module_info;

    long           op_id           = 0;
    long           committed_op_id = 0;

    long           count_signal           = 0;
    long           count_readed           = 0;
    long           count_success_prepared = 0;
    Queue          queue;
    Consumer       cs;

    Context        context;
    Onto           onto;

    Individual     node;

    ushort         port;
    string         host;
    string         queue_name      = "individuals-flow";
    string         main_module_url = "tcp://127.0.0.1:9112\0";
    Ticket         sticket;
    P_MODULE       module_name;

    Logger log;

    this(P_MODULE _module_name, string _host, ushort _port, Logger in_log)
    {
        process_name = text(_module_name);
        module_name  = _module_name;
        port         = _port;
        host         = _host;
        _log         = in_log;
        log 		 = _log;		
    }

    ~this()
    {
        delete module_info;
    }

    void run()
    {
        module_info = new ModuleInfoFile(process_name, _log, OPEN_MODE.WRITER);
		if (!module_info.is_ready)
		{
			log.trace ("%s terminated", process_name);
			return;
		}
    	
        context = create_context();

        if (context is null)
            context = new PThreadContext("cfg:standart_node", process_name, module_name, log, main_module_url);

        if (node == Individual.init)
        {
            node = context.getConfiguration();
        }

        cache_of_indv = new Cache!(string, string)(1000, "individuals");

        if (configure() == false)
        {
            log.trace("[%s] configure is fail, terminate", process_name);
            return;
        }

        ubyte[] buffer = new ubyte[ 1024 ];

        queue = new Queue(queue_name, Mode.R, log);
        queue.open();

        while (!queue.isReady)
        {
            log.trace("queue [%s] not ready, sleep and repeate...", queue_name);
            Thread.sleep(dur!("seconds")(10));
            queue.open();
        }

        cs = new Consumer(queue, process_name, log);
        cs.open();

        //if (count_signal == 0)
        //    prepare_queue();

        load_systicket();

        sock = nn_socket(AF_SP, NN_SUB);
        if (sock >= 0)
        {
            int to = 1000;
            if (nn_setsockopt(sock, NN_SOL_SOCKET, NN.RCVTIMEO, &to, to.sizeof) < 0)
                log.trace("ERR! cannot set scoket options NN_RCVTIMEO");

            if (nn_setsockopt(sock, NN_SUB, NN_SUB_SUBSCRIBE, "".toStringz, 0) < 0)
                log.trace("ERR! cannot set scoket options NN_SUB_SUBSCRIBE");

            if (nn_connect(sock, cast(char *)notify_channel_url) >= 0)
            {
                already_notify_channel = true;
                log.trace("success connect %s", notify_channel_url);
            }
            else
                log.trace("ERR! cannot connect socket to %s", notify_channel_url);


            //listen(&ev_LWS_CALLBACK_GET_THREAD_ID, &ev_LWS_CALLBACK_CLIENT_RECEIVE);
            if (already_notify_channel)
            {
                while (f_listen_exit != true)
                {
                    ev_CALLBACK_GET_THREAD_ID();
                    thread_id();
                }
            }
        }
        
        if (_log !is null)
	        _log.close();
	        
	    module_info.close();    
    }

///////////////////////////////////////////////////

    // if return [false] then, no commit prepared message, and repeate
    abstract ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id,
                                long op_id);

    abstract bool configure();

    abstract Context create_context();

    abstract void thread_id();

    abstract void receive_msg(string msg);

    private void prepare_queue()
    {
        queue.close();
        queue.open();
        while (true)
        {
            if (f_listen_exit == true)
                break;

            string data = cs.pop();

            if (data is null)
            {
                break;
            }

            count_readed++;

            Individual imm;

            if (data !is null && cbor2individual(&imm, data) < 0)
            {
                log.trace("ERR! invalid individual:[%s]", data);
                continue;
            }

            string  new_bin = imm.getFirstLiteral("new_state");
            //writeln ("@read from queue, new_bin=", new_bin);
            string  prev_bin = imm.getFirstLiteral("prev_state");
            //writeln ("@read from queue, prev_bin=", prev_bin);
            string  user_uri = imm.getFirstLiteral("user_uri");
            //writeln ("@read from queue, user_uri=", user_uri);
            string  event_id = imm.getFirstLiteral("event_id");
            INDV_OP cmd      = cast(INDV_OP)imm.getFirstInteger("cmd");
            op_id = imm.getFirstInteger("op_id");

            Individual prev_indv, new_indv;
            if (new_bin !is null && cbor2individual(&new_indv, new_bin) < 0)
            {
                log.trace("ERR! invalid individual:[%s]", new_bin);
            }
            else
            {
//                log.trace("@read from queue new_indv.uri=%s, op_id=%s", new_indv.uri, op_id);

                if (prev_bin !is null && cbor2individual(&prev_indv, prev_bin) < 0)
                {
                    log.trace("ERR! invalid individual:[%s]", prev_bin);
                }
            }

            count_success_prepared++;

            //writeln ("%1 prev_bin=[", prev_bin, "], \nnew_bin=[", new_bin, "]");
            if (onto is null)
                onto = context.get_onto();

            onto.update_class_in_hierarchy(new_indv, true);

            cache_of_indv.put(new_indv.uri, new_bin);

            try
            {
                ResultCode res = prepare(cmd, user_uri, prev_bin, prev_indv, new_bin, new_indv, event_id, op_id);

                if (res == ResultCode.OK)
                {
                    cs.commit();
                    module_info.put_info(op_id, committed_op_id);
                }
                else if (res == ResultCode.Connect_Error || res == ResultCode.Internal_Server_Error || res == ResultCode.Not_Ready ||
                         res == ResultCode.Service_Unavailable || res == ResultCode.Too_Many_Requests)
                {
                    log.trace("WARN: message fail prepared, sleep and repeate...");
                    Thread.sleep(dur!("seconds")(10));
                }
                else
                {
                    cs.commit();
                    module_info.put_info(op_id, committed_op_id);
                    log.trace("ERR! message fail prepared, skip.");
                }
            }
            catch (Throwable ex)
            {
                log.trace("ERR! ex=%s", ex.msg);
            }
        }
        if (count_readed != count_success_prepared)
            log.trace("WARN! : readed=%d, success_prepared=%d", count_readed, count_success_prepared);
    }

    void load_systicket()
    {
        sticket = *context.get_systicket_from_storage();

        if (sticket is Ticket.init || sticket.result != ResultCode.OK)
        {
            writeln("SYS TICKET, systicket=", sticket);

            bool is_superadmin = false;

            void trace(string resource_group, string subject_group, string right)
            {
                if (subject_group == "cfg:SuperUser")
                    is_superadmin = true;
            }

            while (is_superadmin == false)
            {
                context.get_rights_origin(&sticket, "cfg:SuperUser", &trace);

                writeln("@@ child_process is_superadmin=", is_superadmin);
                Thread.sleep(dur!("seconds")(1));
            }
        }

        set_global_systicket(sticket);
    }

    void ev_CALLBACK_GET_THREAD_ID()
    {
        //g_child_process.thread_id();
        if (last_committed_op_id < committed_op_id)
        {
            last_committed_op_id = committed_op_id;
            module_info.put_info(op_id, committed_op_id);
        }

        long now = Clock.currTime().stdTime();

        if (now - last_check_time > 1_000_000)
        {
            last_check_time = now;
            prepare_queue();
        }

        if (already_notify_channel)
        {
            char *buf  = cast(char *)0;
            int  bytes = nn_recv(sock, &buf, NN_MSG, 0, /*NN_DONTWAIT*/);

            if (bytes > 0)
            {
                string msg = buf[ 0..bytes - 1 ].dup;
                nn_freemsg(buf);

                //log.trace("CLIENT (%s): RECEIVED %s", process_name, msg);

                if (msg.length > 4 && msg.indexOf("CMD:") >= 0)
                {
                    receive_msg(msg[4..$]);
                }
                else
                {
                    prepare_queue();
                }
            }
        }
    }
}


