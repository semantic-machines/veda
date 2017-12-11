module veda.authorization.az_client;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.string;
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

	version (VibeDefaultMain)
	{
		TCPConnection az_conn;
	}

    this(string _host, ushort _port, Logger _log)
    {
        log = _log;
    }

    ubyte authorize(string _uri, string user_uri, ubyte _request_access, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                                 string subject_group,
                                                                                                                 string right)
                    _trace_acl,
                    void delegate(string resource_group) _trace_group, void delegate(string log) _trace_info
                    )
    {
        return 0;
    }

    void reopen()
    {
    }

    bool open()
    {

		version (VibeDefaultMain)
		{
	    	az_conn = connectTCP("127.0.0.1", 22000);
	    	
	    	if (az_conn.connected == true)
		    	return true;
		}
			
        return false;
    }

    void close()
    {
    }
}
