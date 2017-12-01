/**
 * реализация хранилища, используя tarantool
 */
module veda.core.storage.tarantool_storage;

import std.conv, std.stdio;
import veda.core.common.context, veda.common.logger, veda.common.type;
import veda.connector.storage_connector, veda.connector.requestresponse;
import veda.core.common.transaction, veda.onto.individual, veda.onto.resource;

public class TarantoolStorage
{
    string                    host;
    ushort                    port;
    Logger                    log;
    public TTStorageConnector connector;

    this(string _host, ushort _port, Logger _log)
    {
        host      = _host;
        port      = _port;
        log       = _log;
        connector = new TTStorageConnector(log);
        connector.connect(this.host, this.port);
        log.trace("create TarantoolStorage connector");
    }

    public OpResult put(OptAuthorize op_auth, TransactionItem ti)
    {
        RequestResponse rr = connector.put(op_auth, ti.user_uri, ti2binobj([ ti ]));

        return OpResult(rr.common_rc, ti.op_id);
    }

    public OpResult put(OptAuthorize op_auth, immutable TransactionItem ti)
    {
        RequestResponse rr = connector.put(op_auth, ti.user_uri, ti2binobj([ ti ]));

        return OpResult(rr.common_rc, ti.op_id);
    }

    public OpResult[] put(OptAuthorize op_auth, TransactionItem[] items)
    {
        OpResult[]      rcs;

        RequestResponse lres = connector.put(op_auth, items[ 0 ].user_uri, ti2binobj(items));

        foreach (idx, rr; lres.op_rc)
            rcs ~= OpResult(lres.op_rc[ idx ], items[ idx ].op_id);

        return rcs;
    }

    public OpResult[] put(OptAuthorize op_auth, immutable(TransactionItem)[] items)
    {
        OpResult[]      rcs;

        RequestResponse lres = connector.put(op_auth, items[ 0 ].user_uri, ti2binobj(items));

        foreach (idx, rr; lres.op_rc)
            rcs ~= OpResult(lres.op_rc[ idx ], items[ idx ].op_id);

        return rcs;
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

string[] ti2binobj(immutable (TransactionItem)[] items)
{
    string[] ipack;

    foreach (ti; items)
    {
        Individual imm;
        imm.uri = text(ti.op_id);

        if (ti.prev_binobj !is null && ti.prev_binobj.length > 0)
            imm.addResource("prev_state", Resource(DataType.String, ti.prev_binobj));
        imm.addResource("new_state", Resource(DataType.String, ti.new_binobj));

        ipack ~= imm.serialize_to_msgpack();
    }

    return ipack;
}

string[] ti2binobj(TransactionItem[] items)
{
    string[] ipack;

    foreach (ti; items)
    {
        Individual imm;
        imm.uri = text(ti.op_id);

        if (ti.prev_binobj !is null && ti.prev_binobj.length > 0)
            imm.addResource("prev_state", Resource(DataType.String, ti.prev_binobj));
        imm.addResource("new_state", Resource(DataType.String, ti.new_binobj));

        ipack ~= imm.serialize_to_msgpack();
    }

    return ipack;
}