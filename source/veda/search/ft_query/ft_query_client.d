module veda.search.ft_query.ft_query_client;

import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, std.outbuffer;
import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.common.logger;
import kaleidic.nanomsg.nano, veda.util.properd;
import veda.search.common.isearch, veda.search.common.vel;
import veda.core.common.context;

private int sock_ft_query = -1;

class FTQueryClient : Search
{
    private Context context;
    private Logger  log;
    private string  ft_query_url;
    private bool    is_ready  = false;
    private bool    is_reopen = true;

    this(Context _context)
    {
        context = _context;
        log     = context.get_logger();
    }

    private void connect()
    {
        try
        {
            string[ string ] properties;
            properties   = readProperties("./veda.properties");
            ft_query_url = properties.as!(string)("ft_query_service_url") ~ "\0";

            if (get_sock_2_ft_query() >= 0)
                is_ready = true;
        }
        catch (Throwable ex)
        {
            log.trace("ERR! unable read ./veda.properties");
        }
    }

    public void reopen_db()
    {
    }

    public bool close_db()
    {
        return true;
    }

    public int query(string user_uri, string filter, string sort, string db_names, int top, int limit,
                     ref Individual[] individuals, OptAuthorize op_auth, bool trace)
    {
        if (is_ready == false)
            connect();

        SearchResult res;
        int          from = 0;

        reqres(res, user_uri, filter, sort, db_names, from, top, limit, op_auth, trace);

        foreach (uri; res.result)
        {
            Individual individual = Individual();

            context.get_storage().get_obj_from_individual_storage(uri, individual);

            if (individual.getStatus() == ResultCode.Not_Found)
            {
                log.trace("ERR! FT:get Unable to find the object [%s] it should be, query=[%s]", uri, filter);
            }
            else if (individual.getStatus() == ResultCode.OK)
            {
                individuals ~= individual;
            }
            else
            {
                log.trace("ERR!: FT:get invalid individual=%s, status=%s, query=%s", uri, individual.getStatus(), filter);
            }
        }

        return cast(int)individuals.length;
    }


    public SearchResult query(string user_uri, string filter, string sort, string db_names, int from, int top, int limit,
                              OptAuthorize op_auth, bool trace)
    {
        if (is_ready == false)
            connect();

        SearchResult res;

        reqres(res, user_uri, filter, sort, db_names, from, top, limit, op_auth, trace);

        return res;
    }


    private void reqres(ref SearchResult sr_res, string user_uri, string filter, string sort, string db_names, int from, int top, int limit,
                        OptAuthorize op_auth, bool trace)
    {
        JSONValue req;
        string    rep;

        req.array =
        [
            JSONValue("UU=" ~ user_uri), JSONValue(filter), JSONValue(sort), JSONValue(db_names), JSONValue(is_reopen), JSONValue(top),
            JSONValue(limit),
            JSONValue(from)
        ];

        try
        {
            int sock = get_sock_2_ft_query();

            if (sock >= 0)
            {
                char   *buf = cast(char *)0;

                string sreq = req.toString();
                int    res  = nn_send(sock, cast(char *)sreq, sreq.length, 0);

                if (res < 0)
                {
                    log.trace("ERR! N_CHANNEL: send: err=%s", fromStringz(nn_strerror(nn_errno())));
                    log.trace("N_CHANNEL send (%s)", req);
                }


                for (int attempt = 0; attempt < 10; attempt++)
                {
                    res = nn_recv(sock, &buf, NN_MSG, 0);

                    if (res < 0)
                    {
                        log.trace("ERR! N_CHANNEL: recv: err=%s", fromStringz(nn_strerror(nn_errno())));
                    }

                    if (res > 0 || res == -1 && nn_errno() != 4)
                        break;

                    log.trace("ERR! N_CHANNEL: repeat recv, attempt=%d", attempt + 1);
                }


                if (res >= 0)
                {
                    int bytes = res;

                    rep = to!string(buf);
                    //log.trace("N_CHANNEL recv (%s)", rep);

                    JSONValue jres;

                    try { jres = parseJSON(rep); }
                    catch (Throwable tr)
                    {
                        nn_freemsg(buf);
                        log.trace("ERR! ft_query_client: fail parse response=%s, err=%s", rep, tr.msg);
                        return;
                    }

                    JSONValue[] ids_json = jres[ "result" ].array;
                    sr_res.result = new string[ ids_json.length ];
                    foreach (idx, id; ids_json)
                    {
                        sr_res.result[ idx ] = id.str;
                    }

                    sr_res.count       = cast(int)jres[ "count" ].integer;
                    sr_res.estimated   = cast(int)jres[ "estimated" ].integer;
                    sr_res.processed   = cast(int)jres[ "processed" ].integer;
                    sr_res.cursor      = cast(int)jres[ "cursor" ].integer;
                    sr_res.result_code = cast(ResultCode)jres[ "result_code" ].integer;

                    nn_freemsg(buf);

                    is_reopen = false;
                }
            }
            else
            {
                log.trace("ERR! N_CHANNEL: invalid socket");
            }

            return;
        }
        catch (Throwable tr)
        {
            log.trace("ERR! reqrep_json_2_ft_query, %s", tr.info);
            log.trace("req: %s", req);
            log.trace("rep: %s", rep);

            return;
        }
    }

    private int get_sock_2_ft_query()
    {
        if (sock_ft_query >= 0)
            return sock_ft_query;

        sock_ft_query = nn_socket(AF_SP, NN_REQ);
        if (sock_ft_query < 0)
        {
            log.trace("ERR! cannot create socket");
            return -1;
        }
        else if (nn_connect(sock_ft_query, cast(char *)ft_query_url) < 0)
        {
            log.trace("ERR! cannot connect socket to %s", ft_query_url);
            return -1;
        }
        else
        {
            log.trace("success connect %s", ft_query_url);
            return sock_ft_query;
        }
    }
}
