/**
   кэш авторизации
 */

module veda.core.az.cache;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.type, veda.core.define;
}

bool put_in_cache (string ss_as_cbor)
{
	return false;
}



void cache_manager (string thread_name)
{
	    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });
    while (true)
    {
        receive(
                (CMD cmd, CNAME cname, string _key2slot_str)
                {
                    if (cmd == CMD.PUT)
                    {
                    }
                },
                (CMD cmd, CNAME cname, Tid tid_sender)
                {
                    if (cmd == CMD.GET)
                    {
                    }
                }, (Variant v) { writeln(thread_name, "::xapian_thread_context::Received some other type.", v); });
    }
}
