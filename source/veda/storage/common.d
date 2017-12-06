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

interface Authorization
{
    ubyte authorize(string _uri, Ticket *ticket, ubyte _request_access, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                                string subject_group,
                                                                                                                string right)
                    _trace_acl,
                    void delegate(string resource_group) _trace_group, void delegate(string log) _trace_info
                    );

    public void open();
    public void reopen();
    public void close();
    public void flush(int force);
}    

public interface KeyValueDB
{
    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true);

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

