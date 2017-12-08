module veda.storage.common;

import std.conv, std.datetime, std.uuid;
import veda.common.logger;
import veda.common.type, veda.core.common.transaction;
import veda.onto.individual, veda.onto.resource, veda.core.common.know_predicates, veda.util.module_info, veda.core.util.utils;

/// Режим работы хранилища
enum DBMode
{
    /// чтение
    R  = true,

    /// чтение/запись
    RW = false
}

public interface KeyValueDB
{
    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true);

    public long get_last_op_id();
    public void open();
    public void reopen();
    public void close();
    public void flush(int force);

    public long count_entries();

    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id);
    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key);
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

