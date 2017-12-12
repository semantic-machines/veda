module veda.authorization.az_client;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.string, core.time, std.outbuffer;
    import url;
    import veda.common.type, veda.core.common.define, veda.storage.authorization;
    import veda.common.logger, veda.util.module_info;
}
import kaleidic.nanomsg.nano;
version (VibeDefaultMain)
{
    import vibe.core.net;
}

static this()
{
}

class ClientAuthorization : Authorization
{
    Logger log;
    string host;
    ushort port;

    this(string _url, Logger _log)
    {
        auto prsurl = parseURL(_url);

        host = prsurl.host;
        port = prsurl.port;

        log = _log;
    }

    ubyte authorize(string _uri, string user_uri, ubyte _request_access, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                                 string subject_group,
                                                                                                                 string right)
                    _trace_acl,
                    void delegate(string resource_group) _trace_group, void delegate(string log) _trace_info
                    )
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
            buff.write("\"]]");

            if (sock >= 0)
            {
                char   *buf = cast(char *)0;
                int    bytes;

                string req = buff.toString();

                bytes = nn_send(sock, cast(char *)req, req.length + 1, 0);
                stderr.writefln("AZCL send [%d](%s)", req.length, req);
                bytes = nn_recv(sock, &buf, NN_MSG, 0);
                if (bytes > 0)
                {
                    string rep = to!string(buf);
                    //stderr.writefln("AZCL recv (%s)", rep);

                    if (rep.length > 4 && rep[ 0 ] == '[' && rep[ 1 ] == '"')
                    {
                        rep = rep[ 2..$ - 2 ];
                        stderr.writefln("AZCL rep (%s)", rep);

                        res = access_from_pretty_string(rep);
                    }

                    nn_freemsg(buf);
                }
                nn_close(sock);
                sock = -1;
            }
        //stderr.writefln("AZCL _request_access (%d), res (%d)", _request_access, res);
        return res;
    }

    void reopen()
    {
    }

    private int    sock            = -1;
    private string main_module_url = "tcp://127.0.0.1:22000\0";


    bool open()
    {
        if (sock >= 0)
            return true;

        sock = nn_socket(AF_SP, NN_REQ);
        if (sock < 0)
        {
            log.trace("ERR! cannot create socket");
            return false;
        }
        else if (nn_connect(sock, cast(char *)main_module_url) < 0)
        {
            log.trace("ERR! cannot connect socket to %s", main_module_url);
            return false;
        }
        else
        {
            log.trace("success connect %s", main_module_url);
            return true;
        }
        //return false;
    }

    void close()
    {
    }
}
