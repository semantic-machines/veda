/**
 * filltext query module
 */

import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd, core.runtime;
import std.stdio, std.socket, std.conv, std.array, std.outbuffer, std.json;
import kaleidic.nanomsg.nano, commando;
import core.thread, core.atomic;
import veda.common.logger, veda.core.common.context, veda.core.impl.thread_context, veda.common.type, veda.core.common.define, veda.search.vql, veda.search.isearch;

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
        Logger       log = context.get_logger();
        JSONValue    jsn;

        try { jsn = parseJSON(request); }
        catch (Throwable tr)
        {
            log.trace("ERR! ft_query: fail parse request=%s, err=%s", request, tr.msg);
            return "[\"err:invalid request\"]";
        }

        if (jsn.type == JSON_TYPE.ARRAY)
        {
            if (jsn.array.length == 8)
            {
                string _ticket    = jsn.array[ 0 ].str;
                string _query     = jsn.array[ 1 ].str;
                string _sort      = jsn.array[ 2 ].str;
                string _databases = jsn.array[ 3 ].str;
                bool   _reopen    = false;
                if (jsn.array[ 4 ].type == JSON_TYPE.TRUE)
                    _reopen = true;

                int    _top   = cast(int)jsn.array[ 5 ].integer;
                int    _limit = cast(int)jsn.array[ 6 ].integer;
                int    _from  = cast(int)jsn.array[ 7 ].integer;

                Ticket *ticket;
                ticket = context.get_storage().get_ticket(_ticket, false);

                if (ticket !is null)
                {
                    if (ticket.user_uri is null || ticket.user_uri.length == 0)
                    {
                        context.get_logger.trace("ERR! user not found in ticket object, ticket_id=%s, ticket=%s", _ticket, ticket);
                    }
                    else
                    {
                        try
                        {
                            res = context.get_individuals_ids_via_query(ticket.user_uri, _query, _sort, _databases, _from, _top, _limit, null, OptAuthorize.YES, false);
                        }
                        catch (Throwable tr)
                        {
                            context.get_logger.trace("ERR! get_individuals_ids_via_query, %s", tr.msg);
                            context.get_logger.trace("REQUEST: user=%s, query=%s, sort=%s, databases=%s, from=%d, top=%d, limit=%d", ticket.user_uri, _query, _sort,
                                                     _databases, _from, _top,
                                                     _limit);
                        }
                    }
                }
                else
                {
                    context.get_logger.trace("ERR! ticket not fount: ticket_id = %s", _ticket);
                }

                //context.get_logger.trace("REQUEST: user=%s, query=%s, sort=%s, databases=%s, from=%d, top=%d, limit=%d", ticket.user_uri, _query, _sort, _databases, _from, _top, _limit);
            }
        }

        string response = to_json_str(res);

        //context.get_logger.trace("RESPONCE: %s", response);

        return response;
    }
    catch (Throwable tr)
    {
        try { log.trace("ERR! ft_query request prepare %s", tr.msg); } catch (Throwable tr) {}
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

    string[] tpcs      = bind_url.split(":");
    string   log_sufix = "";
    if (tpcs.length == 3)
    {
        log_sufix = tpcs[ 2 ];
    }

    int     sock;
    log = new Logger("veda-core-ft-query-" ~ log_sufix, "log", "");
    Ticket  systicket;
    Context ctx = PThreadContext.create_new("cfg:standart_node", "ft-query", null, log);
    ctx.set_vql (new XapianSearch(ctx));

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

