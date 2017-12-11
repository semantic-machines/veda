module veda.authorization.az_client;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.string;
    import url;
    import veda.common.type, veda.core.common.define, veda.storage.authorization;
    import veda.common.logger, veda.util.module_info;
}

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

    version (VibeDefaultMain)
    {
        TCPConnection az_conn;
    }

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
            if (az_conn.connected == false)
	            open ();
        	
        	if (az_conn.connected == true)
        	{
        		az_conn.write ("TPS=67 [\"td:RomanKarpov\",[\"td:UserGroup_1\",\"crud\"],[\":net4-tr1-r1\",\"ru\"]]");
        		
        		ubyte recv = az_conn.read ();
        		stderr.writefln ("@AUTHORIZE: recv=%s", cast(string)recv);        		
        	}
        }
    	
        return 0;
    }

    void reopen()
    {
    }

    bool open()
    {
        version (VibeDefaultMain)
        {
            az_conn = connectTCP(host, port);

            if (az_conn.connected == true)
            {
            	az_conn.readTimeout = dur!("msec")(3000);
                return true;
            }    
        }

        return false;
    }

    void close()
    {
    }
}
