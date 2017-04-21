module veda.core.common.transaction;

import std.stdio, std.json;
import veda.core.common.context, veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.onto.onto;

struct TransactionItem
{
    INDV_OP    cmd;
    string     ticket_id;
    string     event_id;
    string     user_uri;

    string     uri;

    long       update_counter;
    long       op_id;

    string     prev_binobj;
    string     new_binobj;

    Individual prev_indv;
    Individual new_indv;

    ResultCode rc;

    immutable this(INDV_OP _cmd, string _user_uri, string _uri, string _prev_binobj, string _new_binobj, long _update_counter, string _event_id)
    {
        cmd            = _cmd;
        user_uri       = _user_uri;
        uri            = _uri;
        prev_binobj    = _prev_binobj;
        new_binobj     = _new_binobj;
        update_counter = _update_counter;
        event_id       = _event_id;
    }

    immutable this(TransactionItem ti)
    {
        cmd            = ti.cmd;
        user_uri       = ti.user_uri;
        uri            = ti.uri;
        prev_binobj    = ti.prev_binobj;
        new_binobj     = ti.new_binobj;
        update_counter = ti.update_counter;
        event_id       = ti.event_id;
    }
}

TransactionItem copy_from_immutable(immutable TransactionItem ti)
{
    TransactionItem res;

    res.cmd            = ti.cmd;
    res.user_uri       = ti.user_uri;
    res.uri            = ti.uri;
    res.prev_binobj    = ti.prev_binobj;
    res.new_binobj     = ti.new_binobj;
    res.update_counter = ti.update_counter;
    res.event_id       = ti.event_id;
    return res;
}

JSONValue to_json(immutable TransactionItem ti)
{
    JSONValue res;

    res[ "cmd" ]            = ti.cmd;
    res[ "user_uri" ]       = ti.user_uri;
    res[ "uri" ]            = ti.uri;
    res[ "prev_binobj" ]    = ti.prev_binobj;
    res[ "new_binobj" ]     = ti.new_binobj;
    res[ "update_counter" ] = ti.update_counter;
    res[ "event_id" ]       = ti.event_id;

    return res;
}


public JSONValue to_json(ref immutable(TransactionItem)[] immutable_queue)
{
    JSONValue res;

    foreach (ti; immutable_queue)
    {
        res ~= to_json(ti);
    }

    return res;
}

struct Transaction
{
    private
    {
        TransactionItem *[ string ] buff;
        TransactionItem *[]          queue;
        immutable(TransactionItem)[] immutable_queue;
    }

    bool       is_autocommit = true;
    long       id;
    ResultCode rc;

/*
    public void                 add(ref immutable TransactionItem _ti)
    {
        TransactionItem ti = copy_from_immutable(_ti);

        buff[ ti.new_indv.uri ] = &ti;
        queue ~= &ti;
    }
 */

    public void add(ref immutable TransactionItem _ti)
    {
        immutable_queue ~= _ti;
    }

    public void add(TransactionItem *ti)
    {
        buff[ ti.new_indv.uri ] = ti;
        queue ~= ti;
    }

    public void reset()
    {
        buff  = buff.init;
        queue = queue.init;
    }

    public TransactionItem *get(string uri)
    {
        return buff.get(uri, null);
    }

    public TransactionItem *[] get_queue()
    {
        return queue;
    }

    public ref immutable(TransactionItem)[] get_immutable_queue()
    {
        return immutable_queue;
    }
}
