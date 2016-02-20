/**
   кэш авторизации
 */

module veda.core.az.cache;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.type, veda.core.define, veda.core.az.right_set;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "ACL");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

string[ string ] cache;

bool put_in_cache(string ss_as_cbor)
{
    return false;
}



void cache_manager(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });
    while (true)
    {
        receive(
                (CMD cmd, string arg1, string arg2)
                {
                    if (cmd == CMD.PUT)
                    {
                    }
                },
                (CMD cmd, string arg1, Tid tid_sender)
                {
                    if (cmd == CMD.GET)
                    {
                    }
                }, (Variant v) { writeln(thread_name, "::acl_cache_manager::Received some other type.", v); });
    }
}

Right *[] _get_resource_groups(string uri, ubyte access, ref bool[ string ] prepared_uris, int level = 0)
{
    Right *[] res;

    //writeln ("~10 level=", level, ", uri=", uri);

    try
    {
        string groups_str;
        //if (cache !is null)
        //	res = cache.get (uri);

        if (res is null)
        {
            groups_str = cache.get(uri, null);

            if (groups_str !is null)
            {
                rights_from_string(groups_str, res);
                //if (cache !is null)
                //	cache.put (uri, res);
            }
        }

        long res_lenght = res.length;

        for (int idx = 0; idx < res_lenght; idx++)
        {
            Right *group = res[ idx ];

            if (prepared_uris.get(group.id, false) == true)
                continue;

            string group_key = membership_prefix ~ group.id;
            group.access = group.access & access;
            //res ~= group;
            prepared_uris[ group.id ] = true;

            if (uri == group_key)
                continue;

            Right *[] up_restrictions = _get_resource_groups(group_key, group.access & access, prepared_uris, level + 1);
            foreach (restriction; up_restrictions)
            {
                res ~= restriction;
            }
        }
    }
    catch (Throwable ex)
    {
        log.trace("ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]", level, ex.line, ex.file, ex.info);
    }
    return res;
}

RightSet get_resource_groups(string uri, ubyte access)
{
    bool[ string ] prepared_uris;
    return new RightSet(_get_resource_groups(uri, access, prepared_uris, 0));
}



