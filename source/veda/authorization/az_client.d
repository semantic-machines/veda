module veda.authorization.az_client;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.string, core.time, std.outbuffer, std.json;
    import url, kaleidic.nanomsg.nano;
    import veda.common.type, veda.core.common.define, veda.authorization.authorization;
    import veda.common.logger, veda.util.module_info;
}

class ClientAuthorization : Authorization
{
    private Logger log;
    private string acl_service_url;
    private int    sock = -1;

    this(string _url, Logger _log)
    {
        acl_service_url = _url;
        log             = _log;
    }

    ubyte authorize(string _uri, string user_uri, ubyte _request_access, bool is_check_for_reload, OutBuffer _trace_acl, OutBuffer _trace_group,
                    OutBuffer _trace_info)
    {
        ubyte res;

        bool  is_open = false;

        while (is_open == false)
        {
            is_open = open();

            if (is_open == false)
                core.thread.Thread.sleep(dur!("seconds")(1));
        }

        OutBuffer buff = new OutBuffer();

        buff.write("[\"");
        buff.write(user_uri);
        buff.write("\",[\"");
        buff.write(_uri);
        buff.write("\",\"");
        buff.write(access_to_short_string(_request_access));
        if (_trace_acl !is null)
            buff.write("\",\"TRACE-ACL");
        if (_trace_group !is null)
            buff.write("\",\"TRACE-GROUP");
        if (_trace_info !is null)
            buff.write("\",\"TRACE-INFO");
        buff.write("\"]]");

        if (sock >= 0)
        {
            char   *buf = cast(char *)0;
            int    bytes;

            string req = buff.toString();

            bytes = nn_send(sock, cast(char *)req, req.length + 1, 0);
            //stderr.writefln("AZCL send [%d](%s)", req.length, req);
            bytes = nn_recv(sock, &buf, NN_MSG, 0);
            if (bytes > 0)
            {
                string rep = to!string(buf);
                //stderr.writefln("AZCL recv (%s)", rep);

                if (rep.length > 4 && rep[ 0 ] == '[' && rep[ 1 ] == '"')
                {
                    //stderr.writefln("AZCL rep (%s)", rep);

                    if (_trace_acl !is null || _trace_group !is null || _trace_info !is null)
                    {
                        JSONValue jsn;

                        try
                        {
                            jsn = parseJSON(rep);
                        }
                        catch (Throwable tr)
                        {
                            stderr.writefln("ERR! az_client: fail parse rep=%s, err=%s", rep, tr.msg);
                        }

                        if (jsn.type == JSON_TYPE.ARRAY)
                        {
                            int idx = 1;
                            rep = jsn.array[ 0 ].str;

                            if (_trace_acl !is null)
                                _trace_acl.write(jsn.array[ idx++ ].str);

                            if (_trace_group !is null)
                                _trace_group.write(jsn.array[ idx++ ].str);

                            if (_trace_info !is null)
                                _trace_info.write(jsn.array[ idx++ ].str);
                        }
                    }
                    else
                    {
                        rep = rep[ 2..$ - 2 ];
                    }

                    res = access_from_pretty_string(rep);
                }

                nn_freemsg(buf);
            }
            //nn_close(sock);
            //sock = -1;
        }
        //stderr.writefln("AZCL _request_access (%d), res (%d)", _request_access, res);
        return res;
    }

    void reopen()
    {
    }

    bool open()
    {
        int prev_sock = sock;

        if (sock >= 0)
            return true;

        sock = nn_socket(AF_SP, NN_REQ);
        if (sock < 0)
        {
            log.trace("ERR! cannot create socket");
            return false;
        }
        else if (nn_connect(sock, cast(char *)(acl_service_url ~ "\0")) < 0)
        {
            log.trace("ERR! cannot connect socket to %s", acl_service_url);
            sock = -1;
            return false;
        }
        else
        {
            log.trace("success connection %s, sock_id=%d, %d", acl_service_url, sock, prev_sock);
            return true;
        }
    }

    void close()
    {
        log.trace("close connection %s, sock_id=%d", acl_service_url, sock);
        nn_close(sock);
        sock = -1;
    }
}
