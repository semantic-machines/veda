module veda.authorization.az_server;
/**
 * authorization module as service
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime, core.thread, core.atomic;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer, std.json, std.string;
import kaleidic.nanomsg.nano, commando, veda.util.properd;
import veda.common.logger, veda.authorization.authorization, veda.storage.common, veda.common.type;
import veda.storage.tarantool.tarantool_acl, veda.storage.lmdb.lmdb_acl, veda.storage.mdbx.mdbx_acl;

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

const byte TRACE_ACL   = 1;
const byte TRACE_GROUP = 2;
const byte TRACE_INFO  = 3;

private nothrow string az_prepare(string request, Authorization athrz, Logger log)
{
    try
    {
        OutBuffer trace_acl;
        OutBuffer trace_group;
        OutBuffer trace_info;

        long      response_offset = 0;

        char[]    response = new char[ request.length ];
        byte[ 3 ] order_trace;

        string user_uri;
        //stderr.writefln("request=|%s| len=%d", request, request.length);

        JSONValue jsn;

        try { jsn = parseJSON(request); }
        catch (Throwable tr)
        {
            log.trace("ERR! az_server: fail parse request=%s, err=%s", request, tr.msg);
            return "[\"err:invalid request\"]";
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
                                        order_trace[ ii - 2 ] = TRACE_ACL;
                                        trace_acl             = new OutBuffer();
                                    }
                                    else if (el.array[ ii ].str == "TRACE-GROUP")
                                    {
                                        order_trace[ ii - 2 ] = TRACE_GROUP;
                                        trace_group           = new OutBuffer();
                                    }
                                    else if (el.array[ ii ].str == "TRACE-INFO")
                                    {
                                        order_trace[ ii - 2 ] = TRACE_INFO;
                                        trace_info            = new OutBuffer();
                                    }
                                }
                            }

                            ubyte res = athrz.authorize(uri, user_uri, access_from_pretty_string(
                                                                                                 s_access), true, trace_acl, trace_group,
                                                        trace_info);

                            //stderr.writefln("uri=%s user_uri=%s response_access=%s", uri, user_uri, access_to_pretty_string(res));

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
        {
            log.trace("ERR! bad request: unknown json");
            return "[\"err:unknown json\"]";
        }

        //stderr.writefln ("res_trace=%s", res_trace);

        if (trace_group !is null || trace_acl !is null || trace_info !is null)
        {
            string[] all_res;

            all_res ~= cast(string)response[ 2..response_offset - 1 ];

            foreach (oo; order_trace)
            {
                if (oo > 0)
                {
                    if (oo == TRACE_ACL)
                        all_res ~= trace_acl.toString();
                    else if (oo == TRACE_GROUP)
                        all_res ~= trace_group.toString();
                    else if (oo == TRACE_INFO)
                        all_res ~= trace_info.toString();
                }
            }
            JSONValue jout = JSONValue(all_res);
            string    sout = jout.toString();
            return sout;
        }

        response[ response_offset++ ] = ']';
        response[ response_offset++ ] = 0;
        return cast(string)response[ 0..response_offset ];
    }
    catch (Throwable tr)
    {
        try{ log.trace("ERR! az_prepare %s", tr.msg); } catch (Throwable tr) {}
        return "[\"err:exception:" ~ tr.msg ~ "\"]";
    }
}

private long   count;
private Logger log;
void main(string[] args)
{
    string bind_url = "tcp://127.0.0.1:22000";

    try
    {
        ArgumentParser.parse(args, (ArgumentSyntax syntax)
                             {
                                 syntax.config.caseSensitive = commando.CaseSensitive.yes;
                                 syntax.option('b', "bind", &bind_url, Required.no,
                                               "Set binding url, example: --bind=tcp://127.0.0.1:22000");
                             });
    }
    catch (ArgumentParserException ex)
    {
        stderr.writefln(ex.msg);
        return;
    }

    int sock;

    log = new Logger("veda-core-authorization", "log", "");

    sock = nn_socket(AF_SP, NN_REP);
    if (sock < 0)
    {
        log.trace("ERR! cannot create socket");
        return;
    }
    if (nn_bind(sock, cast(char *)(bind_url ~ "\0")) < 0)
    {
        log.trace("ERR! cannot bind to socket, url=%s", bind_url);
        return;
    }
    log.trace("success bind to %s", bind_url);

    Authorization athrz;

    string[ string ] properties;
    properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    if (tarantool_url !is null)
    {
        athrz = new TarantoolAuthorization(log);
    }
    else
    {
        string authorization_db_type = properties.as!(string)("authorization_db_type");

        if (authorization_db_type == "mdbx")
            athrz = new MdbxAuthorization(DBMode.R, "acl", log);
        else
            athrz = new LmdbAuthorization(DBMode.R, "acl", log);
    }

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
                //stderr.writefln("RECEIVED [%d](%s) cont=%d", bytes, req, count);

                string rep = az_prepare(req, athrz, log);

                nn_freemsg(buf);

                bytes = nn_send(sock, cast(char *)rep, rep.length, 0);
                //stderr.writefln("SENDING (%s) %d bytes", rep, bytes);
            }
        }
        catch (Throwable tr)
        {
            log.trace("ERR! MAIN LOOP", tr.info);
        }
    }
}
