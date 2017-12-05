/**
 * реализация хранилища, используя tarantool
 */
module veda.storage.tarantool.tarantool_storage;

import std.conv, std.stdio, std.string;
import veda.common.logger, veda.common.type;
import veda.storage.common;
import veda.core.common.transaction, veda.onto.individual, veda.onto.resource;
import veda.util.properd;
import veda.bind.tarantool.tnt_stream, veda.bind.tarantool.tnt_net, veda.bind.tarantool.tnt_opt, veda.bind.tarantool.tnt_ping, veda.bind.tarantool.tnt_reply;

public class TarantoolStorage
{
    Logger                    log;
    string                    uri;
    tnt_stream* tnt;

    this(Logger _log)
    {
        string[ string ] properties;
        properties = readProperties("./veda.properties");
        uri = properties.as!(string)("tarantool_url") ~ "\0";
        tnt_stream* tnt = tnt_net(null);
	    tnt_set(tnt, tnt_opt_type.TNT_OPT_URI, uri.ptr); 
	    tnt_set(tnt, tnt_opt_type.TNT_OPT_SEND_BUF, 0);
	    tnt_set(tnt, tnt_opt_type.TNT_OPT_RECV_BUF, 0);
	    tnt_connect(tnt); 
	    tnt_ping(tnt); 
	    tnt_reply_ * reply = tnt_reply_init(null);
	    tnt.read_reply(tnt, reply); 
	    tnt_reply_free(reply);        
    }

    public OpResult put(OptAuthorize op_auth, TransactionItem ti)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }

    public OpResult put(OptAuthorize op_auth, immutable TransactionItem ti)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }

    public OpResult[] put(OptAuthorize op_auth, TransactionItem[] items)
    {
        return [OpResult(ResultCode.Not_Implemented, -1)];
    }

    public OpResult[] put(OptAuthorize op_auth, immutable(TransactionItem)[] items)
    {
        return [OpResult(ResultCode.Not_Implemented, -1)];
    }

    public OpResult remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }

    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        return null;
    }

    public string find_ticket(string ticket_id)
    {
        return null;
    }

    public ubyte authorize(string user_uri, string uri, bool trace)
    {
        return 0;
    }

    public void reopen()
    {
    }

    public void open()
    {
    }

    public void close()
    {
    }

    long count_entries()
    {
        return -1;
    }
}
