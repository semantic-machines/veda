/**
 * filltext query module
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer;
import core.thread, core.atomic;
import veda.common.logger, veda.core.common.context, veda.core.impl.thread_context, veda.common.type, veda.core.common.define;

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
    veda.core.common.context.Context context;

public:
    this(Socket socket, Context context)
    {
        this.socket  = socket;
        this.context = context;
        super(&run);
    }

private:
    void run()
    {
        try
        {
            SearchResult res;
            string       request = _recv(socket);

            string[]     els = request.split('�');
            if (els.length == 8)
            {
                //context.get_logger.trace ("query: %s", els);

                string _ticket    = els[ 0 ];
                string _query     = els[ 1 ];
                string _sort      = els[ 2 ];
                string _databases = els[ 3 ];
                bool   _reopen    = false;
                int    _top       = 10;
                int    _limit     = 100;
                int    _from      = 0;
                //
                if (els[ 4 ].length > 0)
                    _reopen = to!bool(els[ 4 ]);

                if (els[ 5 ].length > 0)
                    _top = to!int (els[ 5 ]);

                if (els[ 6 ].length > 0)
                    _limit = to!int (els[ 6 ]);

                if (els[ 7 ].length > 0)
                    _from = to!int (els[ 7 ]);

                Ticket *ticket;
                ticket = context.get_storage().get_ticket(_ticket, false);

                if (ticket !is null)
                {
                    try
                    {
                        res = context.get_individuals_ids_via_query(ticket, _query, _sort, _databases, _from, _top, _limit, null, OptAuthorize.YES, false);
                        //context.get_logger.trace("res=%s", res);
                    }
                    catch (Throwable tr)
                    {
                        context.get_logger.trace("ERR! get_individuals_ids_via_query, %s", tr.msg);
                    }
                }
                else
                {
                    context.get_logger.trace("ERR! ticket is null: ticket_id = %s", _ticket);
                }
            }

            string response = to_json_str(res);
            _send(socket, response);

            socket.close();

            ctx_pool.free_context(context);
        }
        catch (Exception ex)
        {
            //printPrettyTrace(stderr);
            stderr.writefln("@ERR QUERY HANDLER %s", ex.msg);
            socket.close();
        }
    }
}


private string to_json_str(SearchResult res)
{
    OutBuffer bb = new OutBuffer();

    bb.write("{\"result\":[");

    foreach (idx, rr; res.result)
    {
        if (idx > 0)
            bb.write(',');

        bb.write('"');
        bb.write(rr);
        bb.write('"');
    }

    bb.writef("], \"count\":%d,\"estimated\":%d,\"processed\":%d,\"cursor\":%d,\"result_code\":%d}", res.count, res.estimated, res.processed,
              res.cursor,
              res.result_code);
    return bb.toString();
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

private Logger log;

class ContextPool
{
	string main_module_url = null;
    private shared bool[ Context ] pool;

    synchronized Context allocate_context()
    {
        foreach (ctx, state; pool)
        {
            if (state == false)
            {
                pool[ ctx ] = true;
                stderr.writefln("return exists context %X", &ctx);
                return ctx;
            }
        }
        Ticket  systicket;
        Context new_ctx = PThreadContext.create_new("cfg:standart_node", "ft-query", log, main_module_url);
        
        stderr.writefln("create new context %X", &new_ctx);
        pool[ new_ctx ] = true;
        return new_ctx;
    }

    synchronized void free_context(Context ctx)
    {
        pool[ ctx ] = false;
    }
}

shared ContextPool ctx_pool;
TcpSocket          listener;

void main()
{
    listener = new TcpSocket();

    listener.bind(getAddress("localhost", 11112)[ 0 ]);
    listener.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, 1);
    listener.listen(65535);

    log = new Logger("veda-core-ft-query", "log", "");

    ctx_pool = new ContextPool();
    Context context = ctx_pool.allocate_context();
    ctx_pool.free_context(context);
    try
    {
        while (!f_listen_exit)
        {
            Socket socket = listener.accept();
            context = ctx_pool.allocate_context();
            auto   ht = new HandlerThread(socket, context);
            ht.start();
        }
    }
    catch (Exception ex)
    {
        //printPrettyTrace(stderr);
        stderr.writefln("@ERR IN FT_QUERY %s", ex.msg);
        listener.close();
    }
}

