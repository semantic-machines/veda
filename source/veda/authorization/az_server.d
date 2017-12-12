module veda.authorization.az_server;
/**
 * authorization module as service
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime, core.thread, core.atomic;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer, std.json, std.string;
import veda.common.logger, veda.storage.authorization, veda.storage.lmdb.lmdb_acl, veda.storage.common, veda.common.type;

static this()
{
    bsd_signal(SIGINT, &handleTermination3);
}

bool f_listen_exit = false;

extern (C) void handleTermination3(int _signal)
{
    stderr.writefln("!SYS: caught signal: %s", text(_signal));

    f_listen_exit = true;
}

const byte     TRACE_ACL   = 0;
const byte     TRACE_GROUP = 1;
const byte     TRACE_INFO  = 2;

private char[] az_prepare(string request, Authorization acl_indexes)
{
    OutBuffer trace_acl;
    OutBuffer trace_group;
    OutBuffer trace_info;

    long      response_offset = 0;

    char[]    response = new char[ request.length ];
    byte[ 3 ] order_trace;

    string user_uri;
    stderr.writefln("request=|%s| len=%d", request, request.length);

    JSONValue jsn;

    try
    {
        jsn = parseJSON(request);
    }
    catch (Throwable tr)
    {
        stderr.writefln("ERR! fail parse request=%s, err=%s", request, tr.msg);
    }

    response[ response_offset++ ] = '[';

    if (jsn.type == JSON_TYPE.ARRAY)
    {
        foreach (idx, el; jsn.array)
        {
            string uri;
            ubyte  request_access;

            if (idx == 0)
            {
                if (el.type != JSON_TYPE.STRING)
                {
                    break;
                }
                user_uri = el.str;
            }
            else
            {
                if (idx > 1)
                    response[ response_offset++ ] = ',';
                response[ response_offset++ ] = '"';
                if (el.type == JSON_TYPE.ARRAY)
                {
                    if (el.array.length >= 2 && el.array.length <= 5)
                    {
                        uri = el.array[ 0 ].str;

                        string s_access = el.array[ 1 ].str;
                        ubyte  access;

                        if (el.array.length > 2)
                        {
                            for (int ii = 2; ii < el.array.length; ii++)
                            {
                                if (el.array[ ii ].str == "TRACE-ACL")
                                {
                                    order_trace[ TRACE_ACL ] = cast(byte)ii;
                                    trace_acl                = new OutBuffer();
                                }
                                else if (el.array[ ii ].str == "TRACE-GROUP")
                                {
                                    order_trace[ TRACE_GROUP ] = cast(byte)ii;
                                    trace_group                = new OutBuffer();
                                }
                                else if (el.array[ ii ].str == "TRACE-INFO")
                                {
                                    order_trace[ TRACE_INFO ] = cast(byte)ii;
                                    trace_info                = new OutBuffer();
                                }
                            }
                        }

                        ubyte res = acl_indexes.authorize(uri, user_uri, access_from_pretty_string(s_access), true, trace_acl, trace_group, trace_info);

                        stderr.writefln("uri=%s user_uri=%s response_access=%s", uri, user_uri, access_to_pretty_string(res));

                        if (res & Access.can_create)
                            response[ response_offset++ ] = 'C';
                        if (res & Access.can_read)
                            response[ response_offset++ ] = 'R';
                        if (res & Access.can_update)
                            response[ response_offset++ ] = 'U';
                        if (res & Access.can_delete)
                            response[ response_offset++ ] = 'D';
                    }
                    else
                    {
                    }
                }
                response[ response_offset++ ] = '"';
            }
        }
    }
    else
        stderr.writefln("ERR! bad request: unknown json");

    //stderr.writefln ("res_trace=%s", res_trace);

    foreach (oo; order_trace)
    {
        if (oo > 0)
        {
            //stderr.writefln ("%s", res_trace[oo-2]);
        }
    }

    response[ response_offset++ ] = ']';
    response[ response_offset++ ] = 0;
    return response[ 0..response_offset ];
}


long   count;

Logger log;
import kaleidic.nanomsg.nano;
void main()
{
    int    sock;
    string url = "tcp://127.0.0.1:22000\0";

    sock = nn_socket(AF_SP, NN_REP);
    if (sock < 0)
    {
        stderr.writefln("ERR! cannot create socket");
        return;
    }
    if (nn_bind(sock, cast(char *)url) < 0)
    {
        stderr.writefln("ERR! cannot bind to socket, url=%s", url);
        return;
    }
    stderr.writefln("success bind to %s", url);

    log = new Logger("veda-authorization", "log", "");
    Authorization acl_indexes;
    if (acl_indexes is null)
        acl_indexes = new LmdbAuthorization(DBMode.R, "acl", log);

    while (!f_listen_exit)
    {
        try
        {
            count++;

            char *buf  = cast(char *)0;
            int  bytes = nn_recv(sock, &buf, NN_MSG, 0);
            if (bytes >= 0)
            {
                string req = cast(string)buf[ 0..bytes ];
                stderr.writefln("RECEIVED [%d](%s) cont=%d", bytes, req, count);

                char[] rep = az_prepare(req, acl_indexes);

                nn_freemsg(buf);

                bytes = nn_send(sock, cast(char *)rep, rep.length, 0);
                stderr.writefln("SENDING (%s) %d bytes", rep, bytes);
            }
        }
        catch (Throwable tr)
        {
            log.trace("ERR! MAIN LOOP", tr.info);
        }
    }
}
