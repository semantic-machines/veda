module veda.server.signal_to_ccus;

import core.thread, std.stdio, std.format, std.datetime, std.concurrency, std.conv, std.outbuffer, std.string, std.uuid, std.path, std.json;
import veda.core.common.context, veda.core.util.utils, veda.util.tools, veda.onto.onto, veda.core.impl.thread_context;
import veda.bind.libwebsocketd, veda.server.wslink;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-server", "log", "CCUS");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

long     recv_wait_dur = 100_000_000;
WSClient ccus_channel;

void ev_LWS_CALLBACK_CLIENT_WRITEABLE(lws *wsi)
{
}

void ev_LWS_CALLBACK_GET_THREAD_ID(lws *wsi)
{
    receiveTimeout(msecs(recv_wait_dur),
                   (string signal)
                   {
                       //writefln ("signal_to_ccus: recv:%s", signal);
                       int n = websocket_write(wsi, signal);

                       log.trace ("signal_to_ccus: send:%s, n=%d", signal, n);
                       //lws_callback_on_writable(wsi);
                   },
                   (Variant v) { log.trace("signal_to_ccus::Received some other type.", v); });
}

void ev_LWS_CALLBACK_CLIENT_RECEIVE(lws *wsi, char[] msg, ResultCode rc)
{
}

void signal_to_ccus_channel(string thread_name)
{
    ccus_channel = new WSClient("127.0.0.1", 8088, "/ccus", "ccus=server2ccus", log);

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    ccus_channel.listen(&ev_LWS_CALLBACK_GET_THREAD_ID, &ev_LWS_CALLBACK_CLIENT_WRITEABLE, &ev_LWS_CALLBACK_CLIENT_RECEIVE);
    
    log.trace ("signal_to_ccus_channel: exit");    
}