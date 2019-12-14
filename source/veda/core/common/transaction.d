module veda.core.common.transaction;

import std.stdio, std.json;
import veda.core.common.context, veda.common.type, veda.core.common.type, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.onto.onto;

struct TransactionItem {
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

    long       assigned_subsystems;

    ResultCode rc;
}

struct Transaction {
    private
    {
        TransactionItem *[ string ] buff;
        TransactionItem[]            queue;
    }

    string      src;
    long        id;
    ResultCode  rc;
    int         count;


    public void add(TransactionItem ti){
        queue ~= ti;
        TransactionItem *tii = &queue[ count ];
        string          kk   = ti.new_indv.uri.dup;
        buff[ kk ] = tii;
        count++;
    }

    public void reset(){
        if (buff.length > 0)
            buff = buff.init;

        if (queue.length > 0)
            queue = queue.init;

        count = 0;
    }

    public TransactionItem *get(string uri){
        return buff.get(uri, null);
    }

    public TransactionItem[] get_queue(){
        return queue;
    }

}
