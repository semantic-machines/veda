module veda.vmodule.vmodule;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime, core.thread, core.memory;
    import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, core.thread, std.uuid, std.algorithm : remove;
    import kaleidic.nanomsg.nano;
    import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue, veda.util.container;
    import veda.common.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
    import veda.core.common.context, veda.util.tools, veda.onto.onto, veda.util.module_info, veda.common.logger;
}

bool   f_listen_exit = false;
Logger _log;

extern (C) void handleTermination(int _signal)
{
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));

    if (_log !is null)
        _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    //_log.close();

    writeln("!SYS: ", process_name, ": preparation for the exit.");

    f_listen_exit = true;

    thread_term();
    Runtime.terminate();
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}

class VedaModule
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

    string         main_queue_name = "individuals-flow";
    Queue          main_queue;
    Consumer       main_cs;
    Consumer       main_cs_prefetch;

    string         prepareall_queue_name;
    Queue          prepareall_queue;
    Consumer       prepareall_cs;

    Context        context;
    Onto           onto;

    Individual     node;

    string         main_module_url = "tcp://127.0.0.1:9112\0";
    Ticket         sticket;
    string         message_header;
    string         module_id;

    bool[ string ]   subsrc;

    Logger log;

    this(string _module_id, Logger in_log)
    {
        module_id             = _module_id.replace("-", "_");
        process_name          = text(module_id);
        prepareall_queue_name = process_name ~ "_prepare_all";
        message_header        = "MSG:" ~ module_id ~ ":";
        _log                  = in_log;
        log                   = _log;
    }

    ~this()
    {
        delete module_info;
    }

    private void open_perapareall_queue()
    {
        // attempt open [prepareall] queue
        prepareall_queue = new Queue(prepareall_queue_name, Mode.R, log);
        prepareall_queue.open();

        if (prepareall_queue.isReady)
        {
            prepareall_cs = new Consumer(prepareall_queue, process_name, log);
            prepareall_cs.open();
        }
    }

    void run()
    {
        module_info = new ModuleInfoFile(process_name, _log, OPEN_MODE.WRITER);
        if (!module_info.is_ready)
        {
            log.trace("%s terminated", process_name);
            return;
        }

        context = create_context();

        if (context is null)
            context = new PThreadContext("cfg:standart_node", process_name, log, main_module_url);

        if (node == Individual.init)
            node = context.get_configuration();

        cache_of_indv = new Cache!(string, string)(1000, "individuals");

        open();
        if (configure() == false)
        {
            log.trace("[%s] configure is fail, terminate", process_name);
            return;
        }

        ubyte[] buffer = new ubyte[ 1024 ];

        main_queue = new Queue(main_queue_name, Mode.R, log);
        main_queue.open();

        while (!main_queue.isReady)
        {
            log.trace("queue [%s] not ready, sleep and repeate...", main_queue_name);
            Thread.sleep(dur!("seconds")(10));
            main_queue.open();
        }

        main_cs = new Consumer(main_queue, process_name, log);
        main_cs.open();

        main_cs_prefetch = new Consumer(main_queue, process_name ~ "_prefetch", log);
        main_cs_prefetch.open();

        // attempt open [prepareall] queue
        open_perapareall_queue();
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
    abstract bool close();
    abstract bool open();

    abstract Context create_context();

    abstract void thread_id();

    abstract void receive_msg(string msg);

    public void subscribe_on_prefetch(string uri)
    {
        subsrc[ uri.idup ] = true;
    }

    public void unsubscribe_on_prefetch(string uri)
    {
        subsrc.remove(uri.dup);
    }

    abstract void event_of_change(string uri);

    public void prepare_all()
    {
        long  total_count    = context.get_subject_storage_db().count_entries();
        long  count_prepared = 0;

        long  count;
        Queue queue = new Queue(prepareall_queue_name, Mode.RW, log);

        if (queue.open(Mode.RW) != true)
        {
            log.trace("fail on create queue [%s]", prepareall_queue_name);
            return;
        }

        bool add_to_queue(string key, string value)
        {
            queue.push(key);
            count++;
            return true;
        }

        log.trace_console("start prepare_all");
        context.freeze();
        log.trace_console("start create queue");
        context.get_subject_storage_db().get_of_cursor(&add_to_queue);
        log.trace_console("end create queue, count: %d", queue.count_pushed);
        queue.close();
        context.unfreeze();

        open_perapareall_queue();
    }

    private void configuration_found_in_queue()
    {
        string data;

        while (true)
        {
            data = main_cs_prefetch.pop();
            if (data is null)
            {
                //log.trace("PREFETCH: pop return null");
                break;
            }

            Individual imm;
            if (data !is null && cbor2individual(&imm, data) < 0)
            {
                log.trace("ERR! invalid individual:[%s]", data);
                continue;
            }
            string uri = imm.getFirstLiteral("uri");
            //log.trace("PREFETCH %s", uri);

            if (context.get_config_uri() == uri)
            {
                log.trace("prefetch: found change in config [%s]", uri);
                string new_bin = imm.getFirstLiteral("new_state");
                if (new_bin !is null && cbor2individual(&node, new_bin) < 0)
                {
                    log.trace("ERR! invalid individual:[%s]", new_bin);
                }
                else
                {
                    log.trace("prefetch: reconfigure, use [%s]", node);
                    close();
                    open();
                    context.get_onto();
                    configure();
                }
            }
            else if ((uri in subsrc) !is null)
            {
                event_of_change(uri);
            }

            //Thread.sleep(dur!("seconds")(1));
            main_cs_prefetch.commit_and_next(false);
        }
        main_cs_prefetch.sync();
    }

    private void prepare_queue(string msg)
    {
        main_queue.close();
        main_queue.open();

        while (true)
        {
            if (f_listen_exit == true)
                break;

            configuration_found_in_queue();

            string data = main_cs.pop();

            if (data is null)
            {
                if (prepareall_cs !is null)
                {
                    data = prepareall_cs.pop();
                    if (data is null)
                    {
                        log.trace("[%s] queue is empty, remove it.", prepareall_queue_name);
                        prepareall_cs.remove();
                        prepareall_queue.remove();
                        prepareall_cs = null;
                        break;
                    }

                    Individual indv = context.get_individual(&sticket, data);

                    ResultCode rc = ResultCode.Internal_Server_Error;

                    if (indv !is Individual.init)
                    {
                        Individual prev_indv;

                        try
                        {
                            rc = prepare(INDV_OP.PUT, sticket.user_uri, null, prev_indv, data, indv, "", -1);
                        }
                        catch (Throwable tr)
                        {
                            log.trace("ERR! indv.uri=%s, err=%s", indv.uri, tr.msg);
                        }
                    }

                    if (rc != ResultCode.Connect_Error)
                        prepareall_cs.commit_and_next(true);

                    continue;
                }
                else
                {
                    break;
                }
            }

            count_readed++;

            Individual imm;
            if (data !is null && cbor2individual(&imm, data) < 0)
            {
                log.trace("ERR! invalid individual:[%s]", data);
                continue;
            }

            string  new_bin  = imm.getFirstLiteral("new_state");
            string  prev_bin = imm.getFirstLiteral("prev_state");
            string  user_uri = imm.getFirstLiteral("user_uri");
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

            onto.update_onto_hierarchy(new_indv, true);

            cache_of_indv.put(new_indv.uri, new_bin);

            try
            {
                ResultCode res = prepare(cmd, user_uri, prev_bin, prev_indv, new_bin, new_indv, event_id, op_id);

                if (res == ResultCode.OK)
                {
                    main_cs.commit_and_next(true);
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
                    main_cs.commit_and_next(true);
                    module_info.put_info(op_id, committed_op_id);
                    log.trace("ERR! message fail prepared (res=%s), skip.  count=%d", text(res), count_success_prepared);
                }
            }
            catch (Throwable ex)
            {
                log.trace("ERR! ex=%s", ex.msg);
            }

            //if (count_success_prepared % 1000 == 0)
            //{
            //    log.trace("reopen db's and gc collect");
            //    context.reopen_ro_subject_storage_db();
            //    context.reopen_ro_acl_storage_db();
            //    context.reopen_ro_ticket_manager_db();
            //    GC.collect();
            //}
        }
        if (count_readed != count_success_prepared)
            log.trace("WARN! : readed=%d, success_prepared=%d", count_readed, count_success_prepared);
    }

    void load_systicket()
    {
        sticket = *context.get_systicket_from_storage();

        if (sticket is Ticket.init || sticket.result != ResultCode.OK)
        {
            log.trace("load_systicket: fail systicket=%s", text(sticket));

            bool is_superadmin = false;

            void trace_acl(string resource_group, string subject_group, string right)
            {
                if (subject_group == "cfg:SuperUser")
                    is_superadmin = true;
            }

            while (is_superadmin == false)
            {
                context.get_rights_origin_from_acl(&sticket, "cfg:SuperUser", &trace_acl);

                log.trace("child_process is_superadmin=%s", text(is_superadmin));
                Thread.sleep(dur!("seconds")(1));
            }
        }

        set_global_systicket(sticket);
        log.trace("load_systicket: systicket=%s", text(sticket));
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
            prepare_queue(null);
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

                if (msg.length > message_header.length + 1 && msg.indexOf(message_header) >= 0)
                    receive_msg(msg[ (message_header.length)..$ ]);
                else
                    prepare_queue(msg[ 1..$ ]);
            }
        }
    }
}
