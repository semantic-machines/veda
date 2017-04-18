module veda.core.common.transaction;

import std.stdio;
import veda.core.common.context, veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.onto.onto;

struct TransactionItem
{
    INDV_OP    cmd;
    string     ticket_id;
    string     event_id;
    ResultCode rc;

    string     binobj;
    Individual indv;
}

struct Transaction
{
    TransactionItem *[ string ] buff;
    TransactionItem *[] queue;

    public void         add(TransactionItem *ti)
    {
        buff[ ti.indv.uri ] = ti;
        queue ~= ti;
    }
}

public ResultCode commit(long transaction_id, Transaction *_tnx, Context ctx)
{
    foreach (item; _tnx.queue)
    {
        if (item.cmd != INDV_OP.REMOVE && item.indv == Individual.init)
            continue;

        if (item.rc != ResultCode.OK)
            return item.rc;

        Ticket *ticket = ctx.get_ticket(item.ticket_id);

        //log.trace ("transaction: cmd=%s, indv=%s ", item.cmd, item.indv);

        ResultCode rc;

        if (item.cmd == INDV_OP.REMOVE)
            rc = ctx.remove_individual(ticket, item.binobj, true, item.event_id, transaction_id, false).result;
        else
            rc = ctx.put_individual(ticket, item.indv.uri, item.indv, true, item.event_id, transaction_id, false).result;

        if (rc == ResultCode.No_Content)
        {
            ctx.get_logger().trace("WARN!: Rejected attempt to save an empty object: %s", item.indv);
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
