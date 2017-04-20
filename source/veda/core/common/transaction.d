module veda.core.common.transaction;

import std.stdio;
import veda.core.common.context, veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.onto.onto;

struct TransactionItem
{
    INDV_OP    cmd;
    string     ticket_id;
    string     event_id;
    string     user_id;

    string     uri;

    long       update_counter;
    long       op_id;

    string     prev_binobj;
    string     new_binobj;

    Individual prev_indv;
    Individual new_indv;

    ResultCode rc;
}

struct Transaction
{
    bool                        is_autocommit = false;
    private TransactionItem *[ string ] buff;
    private TransactionItem *[] queue;

    long                        id;
    ResultCode                  rc;

    public void                 add(TransactionItem *ti)
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
}
