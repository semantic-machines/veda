module veda.authorization.az_lib;

import std.conv, std.datetime, std.uuid, std.outbuffer, std.string, std.stdio;
import veda.common.logger, veda.core.common.define, veda.common.type;
import veda.core.common.know_predicates, veda.util.module_info;
import veda.authorization.authorization, veda.authorization.right_set, veda.authorization.cache;

extern (C) ubyte authorize_r(immutable(char) *_uri, immutable(char) *_user_uri, ubyte _request_access, bool _is_check_for_reload);
extern (C) char *get_trace(immutable(char) * _uri, immutable(char) * _user_uri, ubyte _request_access, ubyte trace_mode, bool _is_check_for_reload);

OutBuffer trace_acl;
OutBuffer trace_group;
OutBuffer trace_info;

char      *cstr_acl;
char      *cstr_group;
char      *cstr_info;

public class AuthorizationUseLib : Authorization
{
    Logger log;

    this(Logger _log)
    {
        log = _log;
    }

    bool authorize(string uri, string user_uri, ubyte request_acess, bool is_check_for_reload)
    {
        if (user_uri is null)
        {
            return false;
        }

        ubyte res = authorize(uri, user_uri, request_acess, is_check_for_reload, null, null, null);

        return request_acess == res;
    }

    public void get_rights_origin_from_acl(Ticket *ticket, string uri, OutBuffer trace_acl, OutBuffer trace_info)
    {
        if (ticket is null)
            return;

        authorize(uri, ticket.user_uri, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, trace_acl, null, trace_info);
    }


    public ubyte authorize(string _uri, string user_uri, ubyte request_access, bool is_check_for_reload, OutBuffer _trace_acl, OutBuffer _trace_group,
                           OutBuffer _trace_info)
    {
        trace_acl   = _trace_acl;
        trace_group = _trace_group;
        trace_info  = _trace_info;

        if (trace_acl !is null || trace_group !is null || trace_info !is null)
        {
            if (trace_acl !is null)
            {
                cstr_acl = get_trace((_uri ~ "\0").ptr, (user_uri ~ "\0").ptr, request_access, TRACE_ACL, is_check_for_reload);
                string str = to!string(cstr_acl);
                _trace_acl.write(str);
            }

            if (trace_group !is null)
            {
                cstr_group = get_trace((_uri ~ "\0").ptr, (user_uri ~ "\0").ptr, request_access, TRACE_GROUP, is_check_for_reload);
                string str = to!string(cstr_group);
                _trace_group.write(str);
            }

            if (trace_info !is null)
            {
                cstr_info = get_trace((_uri ~ "\0").ptr, (user_uri ~ "\0").ptr, request_access, TRACE_INFO, is_check_for_reload);
                string str = to!string(cstr_info);
                _trace_info.write(str);
            }
            return 0;
        }
        else
        {
            is_check_for_reload = false;
            return authorize_r((_uri ~ "\0").ptr, (user_uri ~ "\0").ptr, request_access, is_check_for_reload);
        }
    }

    public bool open()
    {
        return true;
    };
    public void reopen()
    {
    };
    public void close()
    {
    };
}
