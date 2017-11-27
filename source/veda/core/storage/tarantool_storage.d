/**
 * реализация хранилища, используя tarantool
 */
module veda.core.storage.tarantool_storage;

import std.conv, std.stdio;
import veda.core.common.context, veda.common.logger, veda.common.type;
import veda.connector.storage_connector, veda.connector.requestresponse;

public class TarantoolStorage
{
    string                  host;
    ushort                  port;
    Logger                  log;
    public StorageConnector connector;

    this(string _host, ushort _port, Logger _log)
    {
        host      = _host;
        port      = _port;
        log       = _log;
        connector = new StorageConnector(log);
        connector.connect(this.host, this.port);
        log.trace("create TarantoolStorage connector");
    }

    public ResultCode put(OptAuthorize op_auth, string user_uri, string in_key, string in_value, long op_id)
    {
        RequestResponse rr = connector.put(op_auth, user_uri, [ in_value ]);

        if (rr !is null)
            return rr.common_rc;

        return ResultCode.Fail_Store;
    }

    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        RequestResponse rr = connector.remove(op_auth, user_uri, [ in_key ], false);

        if (rr !is null)
            return rr.common_rc;

        return ResultCode.Fail_Store;
    }

    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        // stderr.writefln("@FIND [%s] [%s]", user_uri, uri);
        RequestResponse rr = connector.get(op_auth, user_uri, [ uri ], false);

        // stderr.writefln("@FIND RETURN FROM CONNECTOR");
        if (rr !is null && rr.binobjs.length > 0)
        {
            return rr.binobjs[ 0 ];
        }

        return null;
    }

    public string find_ticket(string ticket_id)
    {
        RequestResponse rr = connector.get_ticket([ ticket_id ], false);

        if (rr !is null && rr.binobjs.length > 0)
            return rr.binobjs[ 0 ];

        return null;
    }

    public ubyte authorize(string user_uri, string uri, bool trace)
    {
        // stderr.writefln("@AUTH [%s] [%s]", user_uri, uri);
        RequestResponse rr = connector.authorize(user_uri, [ uri ], trace);

        //log.trace ("authorize.common_rc = %s", rr.common_rc);

        if (rr.common_rc == ResultCode.OK)
        {
            // log.trace ("authorize.right=%s", access_to_pretty_string (rr.rights[0]));

            if (rr !is null && rr.rights.length > 0)
                return rr.rights[ 0 ];
        }

        return 0;
    }

    public void unload_to_queue(string path, string queue_id, bool only_ids)
    {
    }

    public int get_of_cursor(bool delegate(string key, string value) prepare, bool only_ids)
    {
        log.trace("ERR! get_of_cursor not implemented");
        throw new Exception("not implemented");
    }

    public long count_entries()
    {
        log.trace("ERR! count_entries not implemented");
        throw new Exception("not implemented");
    }

    public void reopen_db()
    {
        //throw new Exception ("not implemented");
    }

    public void close_db()
    {
        connector.close();
        //throw new Exception ("not implemented");
    }

    public long dump_to_binlog()
    {
        log.trace("ERR! dump_to_binlog not implemented");
        throw new Exception("not implemented");
    }
}

string access_to_pretty_string(const ubyte src)
{
    string res = "";

    if (src & Access.can_create)
        res ~= "C ";
    if (src & Access.can_read)
        res ~= "R ";
    if (src & Access.can_update)
        res ~= "U ";
    if (src & Access.can_delete)
        res ~= "D ";
    if (src & Access.cant_create)
        res ~= "!C ";
    if (src & Access.cant_read)
        res ~= "!R ";
    if (src & Access.cant_update)
        res ~= "!U ";
    if (src & Access.cant_delete)
        res ~= "!D ";

    return res;
}

