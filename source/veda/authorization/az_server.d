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
    listener.close();

    thread_term();
    Runtime.terminate();
}

class HandlerThread : Thread
{
    private Socket        socket;
    private Authorization acl_indexes;
    private Logger        log;

public:
    this(Socket socket)
    {
        this.socket = socket;
        super(&run);
    }

private:
    void run()
    {
        stderr.writefln("accept connection from %s", socket.remoteAddress());

        log = new Logger("veda-authorization", "log", "");

        if (acl_indexes is null)
            acl_indexes = new LmdbAuthorization(DBMode.R, "acl", this.log);

        try
        {
            while (true)
            {
                char[] request = _recv(socket);

                long   response_offset = 2;
                char[] response        = request;

                if (request == "close")
                    break;

                //correct len
                for (long i = request.length; i > 0; i--)
                {
                    if (request[ i ] == ']')
                    {
                        request = request[ 0..i + 1 ];
                        break;
                    }
                }

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

                        if (idx > 1)
                            response[ response_offset++ ] = ',';
                        response[ response_offset++ ] = '"';
                        if (el.type == JSON_TYPE.ARRAY)
                        {
                            if (el.array.length == 2)
                            {
                                uri = el.array[ 0 ].str;

                                string s_access = el.array[ 1 ].str.toLower();
                                ubyte  access;

                                ubyte  res = acl_indexes.authorize(uri, user_uri, access_from_pretty_string(s_access), true, null, null, null);

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
                            response[ response_offset++ ] = '"';
                        }
                    }
                }
                else
                    stderr.writefln("ERR! bad request: unknown json");

                response[ response_offset++ ] = ']';

                _send(socket, response[ 0..response_offset ]);
            }

            socket.close();
            acl_indexes.close();
        }
        catch (Exception ex)
        {
            //printPrettyTrace(stderr);
            stderr.writefln("@ERR QUERY HANDLER %s", ex.msg);
            socket.close();
        }
    }
}

const byte     HEADER_SIZE = 16;

private char[] _recv(Socket socket)
{
    ubyte[]   buf          = new ubyte[ HEADER_SIZE ];
    ulong     request_size = 0;
    ubyte[]   request;

    ptrdiff_t res = socket.receive(buf);
    stderr.writefln("res= [%d] buf |%s|", res, cast(string)buf);

    if (res > 0)
    {
        if (buf[ 0 ] == 'T' && buf[ 1 ] == 'P' && buf[ 2 ] == 'S' && buf[ 3 ] == '=')
        {
            int idx = 3;
            for (; idx < HEADER_SIZE; idx++)
            {
                if (buf[ idx ] == ' ')
                    break;
            }

            if (idx < HEADER_SIZE)
            {
                string ssize = cast(string)buf[ 4..idx ];
                stderr.writefln("ssize=[%s], idx=%d", ssize, idx);

                request_size = to!int (ssize);

                //stderr.writefln("request_size=%d", request_size);

                request = new ubyte[ request_size + (HEADER_SIZE - idx) ];
                ubyte[] trq = request[ (HEADER_SIZE - idx)..$ ];
                socket.receive(trq);
                stderr.writefln("request=|%s|", cast(string)request);

                for (int i = idx, q = 0; i < HEADER_SIZE; i++, q++)
                    request[ q ] = buf[ i ];
            }
            return cast(char[])request;
        }
    }

    return cast(char[])"close";
}

private void _send(Socket socket, char[] data)
{
    ubyte[] buf           = new ubyte[ 4 ];
    long    response_size = data.length;
    stderr.writefln("RESP %s", data);
    buf                    = new ubyte[ 4 + response_size ];
    buf[ 0 ]               = cast(byte)((response_size >> 24) & 0xFF);
    buf[ 1 ]               = cast(byte)((response_size >> 16) & 0xFF);
    buf[ 2 ]               = cast(byte)((response_size >> 8) & 0xFF);
    buf[ 3 ]               = cast(byte)(response_size & 0xFF);
    buf[ 4 .. buf.length ] = cast(ubyte[])data;
    socket.send(buf);
}


void handle_request()
{
}

Logger    log;

TcpSocket listener;

void main()
{
    string host = "127.0.0.1";
    ushort port = 22000;

    listener = new TcpSocket();

    listener.bind(getAddress(host, port)[ 0 ]);
    listener.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, 1);
    listener.listen(65535);

    log = new Logger("veda-authorization", "log", "");

    stderr.writefln("listen %s:%d", host, port);

    try
    {
        while (!f_listen_exit)
        {
            Socket socket = listener.accept();
            auto   ht     = new HandlerThread(socket);
            ht.start();
        }
    }
    catch (Exception ex)
    {
        //printPrettyTrace(stderr);
        stderr.writefln("@ERR IN AUTHORIZATION %s", ex.msg);
        listener.close();
    }
}
