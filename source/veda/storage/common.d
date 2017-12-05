module veda.storage.common;

import veda.common.type, veda.core.common.transaction;

interface KeyValueDB
{
    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true);
    public void open();
    public void reopen();
    public void close();
    public long count_entries();

    public void flush (int force);
    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id);    
    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key);
}

interface Storage
{
	public long last_op_id ();
    public OpResult put(OptAuthorize op_auth, immutable TransactionItem ti);
    public OpResult[] put(OptAuthorize op_auth, immutable(TransactionItem)[] items);
    //public OpResult remove(OptAuthorize op_auth, string user_uri, string in_key);
    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true);
    //public string find_ticket(string ticket_id);
    //public ubyte authorize(string user_uri, string uri, bool trace);
    public void flush (int force);
    public void reopen();
    public void open();
    public void close();
    long count_entries();	
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

