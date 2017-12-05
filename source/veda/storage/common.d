module veda.storage.common;

import veda.common.type;

interface KeyValueDB
{
    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true);
    public void open();
    public void reopen();
    public void close();
    public long count_entries();

    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id);
    
    public void flush (int force);
//    public ResultCode remove(OptAuthorize op_auth, string user_id, string in_key);
    
//    public OpResult put(OptAuthorize op_auth, TransactionItem ti);

//    public OpResult put(OptAuthorize op_auth, immutable TransactionItem ti);

//    public OpResult[] put(OptAuthorize op_auth, TransactionItem[] items);

//    public OpResult[] put(OptAuthorize op_auth, immutable(TransactionItem)[] items);

//    public OpResult remove(OptAuthorize op_auth, string user_uri, string in_key);
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

