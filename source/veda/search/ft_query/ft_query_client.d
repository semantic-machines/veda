module veda.search.ft_query.ft_query_client;

import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, core.thread, std.uuid, std.outbuffer, std.algorithm : remove;
import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.common.logger;
import kaleidic.nanomsg.nano, veda.util.properd;
import veda.search.common.isearch, veda.search.common.vel;

private int sock_ft_query = -1;

class FTQueryClient : Search
{
    private Logger log;
    private string ft_query_url;
    private bool   is_ready = false;

    private void connect()
    {
        try
        {
            string[ string ] properties;
            properties   = readProperties("./veda.properties");
            ft_query_url = properties.as!(string)("ft_query_service_urll") ~ "\0";

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

    public int query(string user_uri, string filter, string freturn, string sort, int top, int limit,
                     ref Individual[] individuals, OptAuthorize op_auth, bool trace)
    {
        if (is_ready == false)
            connect();

        return -1;
    }


    public SearchResult query(string user_uri, string filter, string freturn, string sort, int from, int top, int limit,
                              void delegate(string uri) prepare_element_event,
                              OptAuthorize op_auth, bool trace)
    {
        if (is_ready == false)
            connect();

        return SearchResult.init;
    }

    public int query(string user_uri, string query_str, ref Individual[] res, OptAuthorize op_auth, bool trace)
    {
        if (is_ready == false)
            connect();

        return -1;
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

    private OpResult[] reqrep_json_2_ft_query(ref JSONValue jreq)
    {
        string req = jreq.toString();

        return reqrep_binobj_2_ft_query(req);
    }

    private OpResult[] reqrep_binobj_2_ft_query(string req)
    {
        string     rep;
        int        res;

        OpResult[] ress;

        try
        {
            int sock = get_sock_2_ft_query();

            if (sock >= 0)
            {
                char *buf = cast(char *)0;

                res = nn_send(sock, cast(char *)req, req.length, 0);

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

                    JSONValue jres = parseJSON(rep);

                    if (jres[ "type" ].str == "OpResult")
                    {
                        if ("data" in jres)
                        {
                            JSONValue data = jres[ "data" ];
                            if (data !is JSONValue.init)
                            {
                                foreach (ii; data.array)
                                {
                                    OpResult ores;

                                    ores.op_id  = ii[ "op_id" ].integer;
                                    ores.result = cast(ResultCode)ii[ "result" ].integer;
                                    ress ~= ores;
                                }
                            }
                        }
                        else
                        {
                            OpResult ores;
                            ores.op_id  = jres[ "op_id" ].integer;
                            ores.result = cast(ResultCode)jres[ "result" ].integer;
                            ress ~= ores;
                        }
                    }

                    nn_freemsg(buf);
                }
            }
            else
            {
                log.trace("ERR! N_CHANNEL: invalid socket");
            }

            if (ress.length == 0)
            {
                log.trace("ERR! reqrep_json_2_ft_query, empty result, sock=%d", sock);
                log.trace("req: (%s)", req);
                log.trace("rep: (%s)", rep);
                OpResult ores;
                ores.op_id  = -1;
                ores.result = ResultCode.Internal_Server_Error;
                return [ ores ];
            }

            return ress;
        }
        catch (Throwable tr)
        {
            log.trace("ERR! reqrep_json_2_ft_query, %s", tr.info);
            log.trace("req: %s", req);
            log.trace("rep: %s", rep);

            if (ress.length == 0)
            {
                OpResult ores;
                ores.op_id  = -1;
                ores.result = ResultCode.Internal_Server_Error;
                return [ ores ];
            }

            return ress;
        }
    }
}
