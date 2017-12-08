module veda.authorization.auth;
/**
 * authorization module
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer;
import core.thread, core.atomic;
import veda.common.logger;

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
    Socket                           socket;

public:
    this(Socket socket)
    {
        this.socket  = socket;
        super(&run);
    }

private:
    void run()
    {
        try
        {
            string       request = _recv(socket);

            string[]     els = request.split('�');
            if (els.length == 8)
            {
                //context.get_logger.trace ("query: %s", els);

                string _user_uri    = els[ 0 ];
                string _uri         = els[ 1 ];
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
    ubyte[] buf          = new ubyte[ 4 ];
    long    request_size = 0;
    socket.receive(buf);
    for (int i = 0; i < 4; i++)
        request_size = (request_size << 8) + buf[ i ];

    ubyte[] request = new ubyte[ request_size ];
    socket.receive(request);
    stderr.writefln("@REQ [%s]", cast(string)request);

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

Logger log;

TcpSocket          listener;

void main()
{
    listener = new TcpSocket();

    listener.bind(getAddress("localhost", 11112)[ 0 ]);
    listener.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, 1);
    listener.listen(65535);

    log = new Logger("veda-authorization", "log", "");

    try
    {
        while (!f_listen_exit)
        {
            Socket socket = listener.accept();
            auto   ht = new HandlerThread(socket);
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
