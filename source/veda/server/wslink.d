module veda.server.wslink;

private
{
    import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;
    import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, std.algorithm : remove;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
    import veda.common.logger, veda.core.storage.lmdb_storage;
    import veda.core.common.context, veda.util.tools, veda.onto.onto;
    import veda.bind.libwebsocketd;
    import veda.util.container;
    alias core.thread.Thread core_thread;
}

bool   f_listen_exit   = false;
int    connection_flag = 0;
int    destroy_flag    = 0;
int    writeable_flag  = 0;

Logger _log;

long   max_size_packet = 1024 * 64;
extern (C) void handleTermination1(int _signal)
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
    bsd_signal(SIGINT, &handleTermination1);
}

class WSClient
{
    lws_context               *ws_context;
    lws_context_creation_info info;
    lws                       *wsi;
    lws_protocols[]           protocol = new lws_protocols[ 2 ];
    string                    ws_path;

    ushort                    port;
    string                    host;
    Logger                    log;
    string                    handshake;

    this(string _host, ushort _port, string _ws_path, string _handshake, Logger in_log)
    {
        handshake = _handshake;
        port      = _port;
        host      = _host;
        _log      = in_log;
        log       = in_log;
        ws_path   = _ws_path;
    }

    ~this()
    {
        //wsi
    }

    private void init_channel()
    {
        protocol[ 0 ].name                  = "veda-module-protocol\0";
        protocol[ 0 ].callback              = &ws_service_callback;
        protocol[ 0 ].per_session_data_size = 16;
        protocol[ 0 ].rx_buffer_size        = max_size_packet;
        protocol[ 0 ].id                    = 0;

        info.port      = 8091; //CONTEXT_PORT_NO_LISTEN;
        info.protocols = &protocol[ 0 ];
//    info.extensions = lws_get_internal_extensions();
        info.gid     = -1;
        info.uid     = -1;
        info.options = 0;

        ws_context = lws_create_context(&info);

        if (ws_context is null)
        {
            log.trace("init_channel: ws_context is NULL");
            return;
        }

        lws_client_connect_info i;

        i.host    = cast(char *)(host ~ "\0");
        i.origin  = i.host;
        i.address = i.host;
        i.port    = port;
        i.context = ws_context;
        i.path    = cast(char *)(ws_path ~ "\0"); //"/ws\0";

        wsi = lws_client_connect_via_info(&i);

        if (wsi is null)
        {
            log.trace("init_channel: wsi create error.");
            return;
        }

        destroy_flag = 0;
        log.trace("init_channel: %s:%d%s, is Ok", host, port, ws_path);
    }

    void listen(void function(lws * wsi) _ev_LWS_CALLBACK_GET_THREAD_ID, void function(lws * wsi) _ev_LWS_CALLBACK_CLIENT_WRITEABLE, void function(
                                                                                                                                                   lws
                                                                                                                                                   *
                                                                                                                                                   wsi,
                                                                                                                                                   char
                                                                                                                                                   []
                                                                                                                                                   msg,
                                                                                                                                                   ResultCode
                                                                                                                                                   rc) _ev_LWS_CALLBACK_CLIENT_RECEIVE)
    {
        ev_LWS_CALLBACK_GET_THREAD_ID    = _ev_LWS_CALLBACK_GET_THREAD_ID;
        ev_LWS_CALLBACK_CLIENT_RECEIVE   = _ev_LWS_CALLBACK_CLIENT_RECEIVE;
        ev_LWS_CALLBACK_CLIENT_WRITEABLE = _ev_LWS_CALLBACK_CLIENT_WRITEABLE;

        try
        {
            while (true)
            {
                if (f_listen_exit == true)
                {
                    log.trace("EXIT");
                    break;
                }

                init_channel();

                bool f1 = false;
                while (!destroy_flag)
                {
                    lws_service(ws_context, 50);

                    // send module name
                    //if (connection_flag && f1 == false)
                    //{
                    //websocket_write(wsi, handshake);
                    //lws_callback_on_writable(wsi);
                    //f1 = true;
                    //}
                    ev_LWS_CALLBACK_GET_THREAD_ID(wsi);
                }

                log.trace("DISCONNECT");
                lws_context_destroy(ws_context);

                core_thread.sleep(dur!("seconds")(1));
            }
        }
        catch (Throwable tr)
        {
            log.trace("MAIN LOOP EXIT: %s", tr.info);
        }

        _log.close();
    }
}

void function(lws *wsi) ev_LWS_CALLBACK_GET_THREAD_ID;
void function(lws *wsi) ev_LWS_CALLBACK_CLIENT_WRITEABLE;
void function(lws *wsi, char[] msg, ResultCode rc) ev_LWS_CALLBACK_CLIENT_RECEIVE;

public int websocket_write(lws *wsi_in, string str)
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

    //writeln("[websocket_write]", str, ", n=", n);

    return n;
}

long last_committed_op_id;
long last_check_time;


extern (C) static int ws_service_callback(lws *wsi, lws_callback_reasons reason, void *user, void *_in, size_t len)
{
    //if (reason != lws_callback_reasons.LWS_CALLBACK_GET_THREAD_ID)
    //    writeln ("@@reason=", reason);

    switch (reason)
    {
    case lws_callback_reasons.LWS_CALLBACK_GET_THREAD_ID:
        //ev_LWS_CALLBACK_GET_THREAD_ID(wsi);
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_ESTABLISHED:
        writeln("[CP] LWS_CALLBACK_CLIENT_ESTABLISHED");
        connection_flag = 1;
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
        _log.trace("[CP] LWS_CALLBACK_CLIENT_CONNECTION_ERROR");
        //destroy_flag    = 1;
        connection_flag = 0;
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLOSED:
        writeln("[CP] LWS_CALLBACK_CLOSED");
        //destroy_flag    = 1;
        connection_flag = 0;
        break;

    case lws_callback_reasons.LWS_CALLBACK_RECEIVE:
        writeln("[CP] LWS_CALLBACK_RECEIVE");
        ResultCode rc;

        if (len >= max_size_packet)
        {
            rc = ResultCode.Size_too_large;
            ev_LWS_CALLBACK_CLIENT_RECEIVE(wsi, null, rc);
        }
        else
        {
            byte[] bmsg = (cast(byte *)_in)[ 0..len ];
            char[] msg  = cast(char[])bmsg;
            //writefln ("msg[%d]=%s", len, msg);
            rc = ResultCode.OK;
            ev_LWS_CALLBACK_CLIENT_RECEIVE(wsi, msg, rc);
        }
        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_RECEIVE:
        //writeln("[CP] LWS_CALLBACK_CLIENT_RECEIVE");

        ResultCode rc;

        if (len >= max_size_packet)
        {
            rc = ResultCode.Size_too_large;
            ev_LWS_CALLBACK_CLIENT_RECEIVE(wsi, null, rc);
        }
        else
        {
            byte[] bmsg = (cast(byte *)_in)[ 0..len ];
            char[] msg  = cast(char[])bmsg;
            //writefln ("msg[%d]=%s", len, msg);
            rc = ResultCode.OK;
            ev_LWS_CALLBACK_CLIENT_RECEIVE(wsi, msg, rc);
        }

        break;

    case lws_callback_reasons.LWS_CALLBACK_CLIENT_WRITEABLE:
        writeln("[CP] LWS_CALLBACK_CLIENT_WRITEABLE");
        //writefln("[%s:%d%s] On writeable is called.", host, port, ws_path);
        //websocket_write(wsi, "test msg-count=" ~ text(msg_count));
        //msg_count++;
        ev_LWS_CALLBACK_CLIENT_WRITEABLE(wsi);

        writeable_flag = 1;
        break;

    default:
    }

    return 0;
}
