module veda.vmodule.vmodule;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;
    import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, std.algorithm : remove;
    import requests.http, requests.streams;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
    import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
    import veda.core.common.context, veda.util.tools, veda.onto.onto;
    import veda.bind.libwebsocketd, veda.vmodule.wslink;
    import veda.util.container;
}

bool   f_listen_exit = false;

logger _log;

extern (C) void handleTermination(int _signal)
{
    _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));
    //_log.close();

    writeln("!SYS: ", process_name, ": preparation for the exit.");

    f_listen_exit = true;
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}

VedaModule g_child_process;

class VedaModule : WSLink
{
    Cache!(string, string) cache_of_indv;

    string     fn_module_info_w  = null;
    File       *ff_module_info_w = null;

    long       op_id           = 0;
    long       committed_op_id = 0;

    long       count_signal           = 0;
    long       count_readed           = 0;
    long       count_success_prepared = 0;
    Queue      queue;
    Consumer   cs;

    Context    context;
    Onto       onto;

    Individual node;

    ushort     port;
    string     host;
    string     queue_name = "individuals-flow";
    string     parent_url = "http://127.0.0.1:8080";
    Ticket     sticket;
    P_MODULE   module_name;

    override logger log()
    {
        if (_log is null)
            _log = new logger("veda-core-" ~ process_name, "log", process_name);
        return _log;
    }

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        g_child_process  = this;
        process_name     = text(_module_name);
        module_name      = _module_name;
        port             = _port;
        host             = _host;
        _log             = new logger("veda-core-" ~ process_name, "log", "PROCESS");
        fn_module_info_w = module_info_path ~ "/" ~ process_name ~ "_info";
        super(host, port);
    }

    ~this()
    {
        ff_module_info_w.flush();
        ff_module_info_w.close();
    }

    void run()
    {
        if (exists(fn_module_info_w) == false)
            ff_module_info_w = new File(fn_module_info_w, "w");
        else
            ff_module_info_w = new File(fn_module_info_w, "r+");

        context = create_context();

        if (context is null)
            context = new PThreadContext("cfg:standart_node", process_name, module_name, parent_url);

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

        queue = new Queue(queue_name, Mode.R);
        queue.open();

        while (!queue.isReady)
        {
            log.trace("queue [%s] not ready, sleep and repeate...", queue_name);
            core.thread.Thread.sleep(dur!("seconds")(10));
            queue.open();
        }

        cs = new Consumer(queue, process_name);
        cs.open();

        //if (count_signal == 0)
        //    prepare_queue();

        load_systicket();

        listen(&ev_LWS_CALLBACK_GET_THREAD_ID, &ev_LWS_CALLBACK_CLIENT_RECEIVE);

        _log.close();
    }

///////////////////////////////////////////////////

    // if return [false] then, no commit prepared message, and repeate
    abstract ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id,
                                long op_id);

    abstract bool configure();

    abstract Context create_context();

    abstract void thread_id();

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
                break;

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
                //writeln ("@read from queue new_indv.uri=", new_indv.uri, ", op_id=", op_id);

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
                    put_info();
                }
                else if (res == ResultCode.Connect_Error || res == ResultCode.Internal_Server_Error || res == ResultCode.Not_Ready ||
                         res == ResultCode.Service_Unavailable || res == ResultCode.Too_Many_Requests)
                {
                    log.trace("WARN: message fail prepared, sleep and repeate...");
                    core.thread.Thread.sleep(dur!("seconds")(10));
                }
                else
                {
                    cs.commit();
                    put_info();
                    log.trace("ERR: message fail prepared, skip.");
                }
            }
            catch (Throwable ex)
            {
                log.trace("EX! ex=%s", ex.msg);
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
                core.thread.Thread.sleep(dur!("seconds")(1));
            }
        }

        set_global_systicket(sticket);
    }

    bool put_info()
    {
        try
        {
            ff_module_info_w.seek(0);
            ff_module_info_w.writefln("%s;%d;%d", process_name, op_id, committed_op_id);
            ff_module_info_w.flush();
            return true;
        }
        catch (Throwable tr)
        {
            log.trace("module:put_info [%s;%d;%d] %s", process_name, op_id, committed_op_id, tr.msg);
            return false;
        }
    }
}

void ev_LWS_CALLBACK_GET_THREAD_ID()
{
    g_child_process.thread_id();
    if (last_committed_op_id < g_child_process.committed_op_id)
    {
        last_committed_op_id = g_child_process.committed_op_id;
        g_child_process.put_info();
    }

    long now = Clock.currTime().stdTime();

    if (now - last_check_time > 1_000_000)
    {
        last_check_time = now;
        g_child_process.prepare_queue();
    }
}

void ev_LWS_CALLBACK_CLIENT_RECEIVE(lws *wsi, char[] msg)
{
    string res;

    if (msg == "get_opid")
    {
        writeln("[CP] Client recieved:", msg);
        long prepared_op_id;

        if (g_child_process.committed_op_id != 0)
            prepared_op_id = g_child_process.committed_op_id;
        else
            prepared_op_id = g_child_process.op_id;

        //writeln ("@module[", process_name, "] get_op_id, return op_id=", prepared_op_id);

        res = text(prepared_op_id);
    }
    else
        res = "Ok";

    websocket_write_back(wsi, res);     // ~ text(msg_count));

    if (msg != "get_opid")
        g_child_process.prepare_queue();
}


long last_committed_op_id;
long last_check_time;

