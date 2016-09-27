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
    import veda.core.bind.libwebsocketd;
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

int        connection_flag = 0;
int        destroy_flag    = 0;
int        writeable_flag  = 0;

VedaModule g_child_process;

class VedaModule
{
    Cache!(string, string) cache_of_indv;

    string                    fn_module_info_w  = null;
    File                      *ff_module_info_w = null;

    long                      op_id           = 0;
    long                      committed_op_id = 0;

    long                      count_signal           = 0;
    long                      count_readed           = 0;
    long                      count_success_prepared = 0;
    Queue                     queue;
    Consumer                  cs;

    lws_context               *ws_context;
    lws_context_creation_info info;
    lws                       *wsi;
    lws_protocols[]           protocol = new lws_protocols[ 2 ];

    Context                   context;
    Onto                      onto;

    Individual                node;

    ushort                    port;
    string                    host;
    string                    queue_name = "individuals-flow";
    string                    parent_url = "http://127.0.0.1:8080";
    Ticket                    sticket;
    P_MODULE                  module_name;

    logger log()
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
    }

    ~this()
    {
        ff_module_info_w.flush();
        ff_module_info_w.close();
    }

    private void init_chanel()
    {
        protocol[ 0 ].name                  = "veda-module-protocol\0";
        protocol[ 0 ].callback              = &ws_service_callback;
        protocol[ 0 ].per_session_data_size = 16;
        protocol[ 0 ].rx_buffer_size        = 0;
        protocol[ 0 ].id                    = 0;

        info.port      = CONTEXT_PORT_NO_LISTEN;
        info.protocols = &protocol[ 0 ];
//    info.extensions = lws_get_internal_extensions();
        info.gid     = -1;
        info.uid     = -1;
        info.options = 0;

        ws_context = lws_create_context(&info);

        //writeln("[Main] context created.");

        if (ws_context is null)
        {
            log.trace("init_chanel: ws_context is NULL");
            return;
        }

        lws_client_connect_info i;

        i.host    = cast(char *)(host ~ "\0");
        i.origin  = i.host;
        i.address = i.host;
        i.port    = port;
        i.context = ws_context;
        i.path    = "/ws\0";

        wsi = lws_client_connect_via_info(&i);

        if (wsi is null)
        {
            log.trace("init_chanel: wsi create error.");
            return;
        }

        destroy_flag = 0;
        log.trace("init_chanel: %s, is Ok", process_name);
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

        try
        {
            while (true)
            {
                if (f_listen_exit == true)
                {
                    log.trace("EXIT");
                    break;
                }

                init_chanel();

                bool f1 = false;
                while (!destroy_flag)
                {
                    lws_service(ws_context, 50);

                    // send module name
                    if (connection_flag && f1 == false)
                    {
                        websocket_write_back(wsi, "module-name=" ~ process_name);
                        lws_callback_on_writable(wsi);
                        f1 = true;
                    }
                }

                log.trace("DISCONNECT");
                lws_context_destroy(ws_context);

                core.thread.Thread.sleep(dur!("seconds")(1));
            }
        }
        catch (Throwable tr)
        {
            log.trace("MAIN LOOP EXIT %s", tr.msg);
        }

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

private int websocket_write_back(lws *wsi_in, string str)
{
    if (str is null)
        return -1;

    int     pre = LWS_SEND_BUFFER_PRE_PADDING;
    int     n;
    int     len = cast(int)str.length;

    ubyte[] _out  = new ubyte[ pre + len ];
    ubyte[] frame = _out[ pre..pre + str.length ];

    frame[ 0..$ ] = cast(ubyte[])str[ 0..$ ];

    //* write out*/
    n = lws_write(wsi_in, cast(ubyte *)frame, len, lws_write_protocol.LWS_WRITE_TEXT);

    //writeln("[websocket_write_back]", str, ", n=", n);

    return n;
}

long last_committed_op_id;
long last_check_time;


extern (C) static int ws_service_callback(lws *wsi, lws_callback_reasons reason, void *user, void *_in, size_t len)
{
    //writeln ("@@reason=", reason);

    switch (reason)
    {
    case lws_callback_reasons.LWS_CALLBACK_GET_THREAD_ID:

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

        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_ESTABLISHED:
        //writeln("[CP]Connect with server success.");
        connection_flag = 1;
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
        _log.trace("[CP] Connect with server error.");
        destroy_flag    = 1;
        connection_flag = 0;
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLOSED:
        //writeln("[CP] LWS_CALLBACK_CLOSED");
        destroy_flag    = 1;
        connection_flag = 0;
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_RECEIVE:
        char[] msg = fromStringz(cast(char *)_in);

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

        websocket_write_back(wsi, res); // ~ text(msg_count));

        if (msg != "get_opid")
            g_child_process.prepare_queue();


//        if (writeable_flag)
//            destroy_flag = 1;

        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_WRITEABLE:
        //writeln("[CP] On writeable is called.");
        //websocket_write_back(wsi, "test msg-count=" ~ text(msg_count));
        //msg_count++;
        writeable_flag = 1;
        break;

    default:
    }

    return 0;
}
