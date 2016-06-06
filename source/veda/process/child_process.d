module veda.process.child_process;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;
    import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, std.algorithm : remove;
    import requests.http, requests.streams;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
    import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
    import veda.core.common.context, veda.util.tools, veda.onto.onto;
    import veda.core.bind.libwebsocketd;
}

bool   f_listen_exit = false;

logger _log;

extern (C) void handleTermination(int _signal)
{
    f_listen_exit = true;

    _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));
    _log.close();

    writeln("!SYS: ", process_name, ": exit");
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}

int          connection_flag = 0;
int          destroy_flag    = 0;
int          writeable_flag  = 0;

ChildProcess g_child_process;

class ChildProcess
{
    long                      op_id;

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

    logger log()
    {
        if (_log is null)
            _log = new logger("veda-core-" ~ process_name, "log", process_name);
        return _log;
    }

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        g_child_process = this;
        process_name    = text(_module_name);
        port            = _port;
        host            = _host;
        _log            = new logger("veda-core-" ~ process_name, "log", "PROCESS");
        context         = new PThreadContext("cfg:standart_node", process_name, _module_name, parent_url);

        if (node == Individual.init)
        {
            node = context.getConfiguration();
            configure();
        }
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
            //writeln("[Main] context is NULL.");
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

        //writeln("[Main] wsi create success.");
    }

    void run()
    {
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

        init_chanel();

        load_systicket();

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

        lws_context_destroy(ws_context);

        log.trace("EXIT");
    }

///////////////////////////////////////////////////

    // if return [false] then, no commit prepared message, and repeate
    abstract bool prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id);

    abstract void configure();


    private void prepare_queue()
    {
        queue.close();
        queue.open();
        while (true)
        {
            if (f_listen_exit == true)
                break;

            string data = cs.pop();

            if (data !is null)
            {
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

                try
                {
                    bool res = prepare(cmd, user_uri, prev_bin, prev_indv, new_bin, new_indv, event_id, op_id);

                    if (res == true)
                        cs.commit();
                    else
                    {
                        log.trace("message fail prepared, sleep and repeate...");
                        core.thread.Thread.sleep(dur!("seconds")(10));
                    }
                }
                catch (Throwable ex)
                {
                    log.trace("EX! ex=%s", ex.msg);
                }
            }
            else
                break;
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
}

int websocket_write_back(lws *wsi_in, string str)
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

extern (C) static int ws_service_callback(lws *wsi, lws_callback_reasons reason, void *user, void *_in, size_t len)
{
    //writeln ("@@reason=", reason,  ", msg_count=", msg_count);

    switch (reason)
    {
    case lws_callback_reasons.LWS_CALLBACK_CLIENT_ESTABLISHED:
        //writeln("[CP]Connect with server success.");
        connection_flag = 1;
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
        //writeln("[CP] Connect with server error.");
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
        //writeln("[CP] Client recieved:", msg);

        string res;
        if (msg == "get_opid")
            res = text(g_child_process.op_id);
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
