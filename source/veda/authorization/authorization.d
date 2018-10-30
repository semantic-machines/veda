module veda.authorization.authorization;

import std.conv, std.datetime, std.outbuffer, std.string, std.stdio;
import veda.common.logger, veda.core.common.define, veda.common.type;
import veda.core.common.know_predicates;

interface Authorization
{
    public ubyte authorize(string _uri, string user_uri, ubyte _request_access, bool is_check_for_reload, OutBuffer _trace_acl, OutBuffer _trace_group,
                           OutBuffer _trace_info);

    public bool authorize(string uri, string user_uri, ubyte request_acess, bool is_check_for_reload);

    public void get_rights_origin_from_acl(Ticket *ticket, string uri, OutBuffer trace_acl, OutBuffer trace_info);

    public bool open();
    public void reopen();
    public void close();
}

const TRACE_ACL   = 0;
const TRACE_GROUP = 1;
const TRACE_INFO  = 2;

string access_to_short_string(const ubyte src)
{
    string res = "";

    if (src & Access.can_create)
        res ~= "C";
    if (src & Access.can_read)
        res ~= "R";
    if (src & Access.can_update)
        res ~= "U";
    if (src & Access.can_delete)
        res ~= "D";

    return res;
}

ubyte access_from_pretty_string(const string access)
{
    ubyte res;

    foreach (c_access; access)
    {
        if (c_access == 'c' || c_access == 'C')
            res = res | Access.can_create;
        if (c_access == 'r' || c_access == 'R')
            res = res | Access.can_read;
        if (c_access == 'u' || c_access == 'U')
            res = res | Access.can_update;
        if (c_access == 'd' || c_access == 'D')
            res = res | Access.can_delete;
    }
    return res;
}

string access_to_string(const ubyte src)
{
    char[] res = new char[ 4 ];

    if (src & Access.can_create)
        res[ 0 ] = 'C';
    else
        res[ 0 ] = '-';
    if (src & Access.can_read)
        res[ 1 ] = 'R';
    else
        res[ 1 ] = '-';
    if (src & Access.can_update)
        res[ 2 ] = 'U';
    else
        res[ 2 ] = '-';
    if (src & Access.can_delete)
        res[ 3 ] = 'D';
    else
        res[ 3 ] = '-';

    return cast(string)res;
}