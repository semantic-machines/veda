/**
 * реализация хранилища, используя немодифицированный tarantool
 */
module veda.storage.tarantool.tarantool_driver;

import std.conv, std.stdio, std.string;
import veda.common.logger, veda.common.type;
import veda.storage.common;
import veda.core.common.transaction, veda.onto.individual, veda.onto.resource;
import veda.util.properd;
import veda.bind.tarantool.tnt_stream, veda.bind.tarantool.tnt_net, veda.bind.tarantool.tnt_opt, veda.bind.tarantool.tnt_ping, veda.bind.tarantool.tnt_reply;

public class TarantoolDriver : KeyValueDB
{
    Logger     log;
    string     uri;
    tnt_stream *tnt;

    this(Logger _log)
    {
        string[ string ] properties;
        properties = readProperties("./veda.properties");
        uri        = properties.as!(string)("tarantool_url") ~ "\0";
        tnt_stream *tnt = tnt_net(null);
        tnt_set(tnt, tnt_opt_type.TNT_OPT_URI, uri.ptr);
        tnt_set(tnt, tnt_opt_type.TNT_OPT_SEND_BUF, 0);
        tnt_set(tnt, tnt_opt_type.TNT_OPT_RECV_BUF, 0);
        tnt_connect(tnt);
        tnt_ping(tnt);
        tnt_reply_ *reply = tnt_reply_init(null);
        tnt.read_reply(tnt, reply);
        tnt_reply_free(reply);
    }

    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        return null;
    }

    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id)
    {
        return ResultCode.Not_Implemented;
    }

    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        return ResultCode.Not_Implemented;
    }

    public long get_last_op_id()
    {
        return -1;
    }

    public int update_or_create(string uri, string content, long op_id, out string new_hash)
    {
        return -1;
    }

    public void open()
    {
    }

    public void reopen()
    {
    }

    public void close()
    {
    }

    public long count_entries()
    {
        return -1;
    }

    public void flush(int force)
    {
    }
}
