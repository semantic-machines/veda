module veda.authorization.auth;
/**
 * authorization module
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime, core.thread, core.atomic;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer;
import veda.common.logger, veda.storage.authorization, veda.storage.lmdb.lmdb_acl, veda.storage.common;

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
            string   request = _recv(socket);

            string[] els = request.split('�');
            if (els.length == 2)
            {
                //context.get_logger.trace ("query: %s", els);

                string _user_uri = els[ 0 ];
                string _uri      = els[ 1 ];
            }

            string response = "F";
            _send(socket, response);

            socket.close();
        }
        catch (Exception ex)
        {
            //printPrettyTrace(stderr);
            stderr.writefln("@ERR QUERY HANDLER %s", ex.msg);
            socket.close();
        }
    }
}

private string _recv(Socket socket)
{
    ubyte[] buf          = new ubyte[ 7 ];
    ulong   request_size = 0;
    socket.receive(buf);
    ubyte[] request;

    if (buf[ 0 ] == 'T' && buf[ 1 ] == 'P' && buf[ 2 ] == 'S')
    {
        for (int i = 3; i < 7; i++)
            request_size = (request_size << 8) + buf[ i ];

        request = new ubyte[ request_size ];
        socket.receive(request);
        stderr.writefln("@REQ [%s]", cast(string)request);
    }

    return cast(string)request;
}

private void _send(Socket socket, string data)
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
    ushort port = 11113;

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
