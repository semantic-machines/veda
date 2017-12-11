module veda.authorization.az_client;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.string, core.time;
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
        version (VibeDefaultMain)
        {
            //if (az_conn is null || az_conn.connected == false)

            for (int i = 0; i < 100000; i++)
            {
                bool is_open = false;
                while (is_open == false)
                {
                    is_open = open();

                    if (is_open == false)
                        core.thread.Thread.sleep(dur!("seconds")(1));
                }

                string req = "[\"td:RomanKarpov\",[\"td:UserGroup_1\",\"crud\"],[\":net4-tr1-r1\",\"ru\"]]";

                if (sock >= 0)
                {
                    char *buf = cast(char *)0;
                    int  bytes;

                    bytes = nn_send(sock, cast(char *)req, req.length + 1, 0);
                    stderr.writefln("AZCL send [%d](%s) i=%d", req.length, req, i);
                    bytes = nn_recv(sock, &buf, NN_MSG, 0);
                    if (bytes > 0)
                    {
                        string rep = to!string(buf);
                        stderr.writefln("AZCL recv (%s)", rep);

                        nn_freemsg(buf);
                    }
                    nn_close(sock);
                    sock = -1;
                }
            }
        }
        return 0;
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
