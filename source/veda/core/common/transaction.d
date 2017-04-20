module veda.core.common.transaction;

import std.stdio;
import veda.core.common.context, veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.onto.onto;

struct TransactionItem
{
    INDV_OP    cmd;
    string     ticket_id;
    string     event_id;
    string	   user_id;	

	string 	   uri;	

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
}

public ResultCode commit(Transaction *_tnx, Context ctx)
{
    Transaction normalized_tnx;

    normalized_tnx.id = _tnx.id;
    foreach (item; _tnx.queue)
    {
        if (item.cmd != INDV_OP.REMOVE && item.new_indv == Individual.init)
            continue;

        if (item.rc != ResultCode.OK)
            return item.rc;

        Ticket *ticket = ctx.get_ticket(item.ticket_id);

        //log.trace ("transaction: cmd=%s, indv=%s ", item.cmd, item.indv);

        ResultCode rc;

        rc = ctx.add_to_transaction(normalized_tnx, ticket, item.cmd, &item.new_indv, true, item.event_id, false, true).result;

        if (rc == ResultCode.No_Content)
        {
            ctx.get_logger().trace("WARN!: Rejected attempt to save an empty object: %s", item.new_indv);
        }

        if (rc != ResultCode.OK && rc != ResultCode.No_Content)
        {
            ctx.get_logger().trace("FAIL COMMIT");
            return rc;
        }
        //else
        //log.trace ("SUCCESS COMMIT");
    }

    return ResultCode.OK;
}
