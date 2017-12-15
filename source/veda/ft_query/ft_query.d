/**
 * filltext query module
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer;
import kaleidic.nanomsg.nano, commando;
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
}

private nothrow string req_prepare(string request, Context context)
{
    try
    {
        SearchResult res;

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
                    res = context.get_individuals_ids_via_query(ticket.user_uri, _query, _sort, _databases, _from, _top, _limit, null, OptAuthorize.YES, false);
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
        return response;
    }
    catch (Throwable tr)
    {
        //printPrettyTrace(stderr);
        try { stderr.writefln("ERR! ft_query request prepare %s", tr.msg); } catch (Throwable tr) {}
        return "ERR";
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


private long   count;
private Logger log;

void main(string[] args)
{
    string bind_url = "tcp://127.0.0.1:23000";

    try
    {
        ArgumentParser.parse(args, (ArgumentSyntax syntax)
                             {
                                 syntax.config.caseSensitive = commando.CaseSensitive.yes;
                                 syntax.option('b', "bind", &bind_url, Required.no,
                                               "Set binding url, example: --bind=tcp://127.0.0.1:23000");
                             });
    }
    catch (ArgumentParserException ex)
    {
        stderr.writefln(ex.msg);
        return;
    }

    int sock;

    sock = nn_socket(AF_SP, NN_REP);
    if (sock < 0)
    {
        stderr.writefln("ERR! cannot create socket");
        return;
    }
    if (nn_bind(sock, cast(char *)(bind_url ~ "\0")) < 0)
    {
        stderr.writefln("ERR! cannot bind to socket, url=%s", bind_url);
        return;
    }
    stderr.writefln("success bind to %s", bind_url);

    log = new Logger("veda-ft-query", "log", "");
    Ticket  systicket;
    Context ctx = PThreadContext.create_new("cfg:standart_node", "ft-query", log, null);

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

                string rep = req_prepare(req, ctx);

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

