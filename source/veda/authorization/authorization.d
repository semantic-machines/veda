module veda.authorization.authorization;

import std.conv, std.datetime, std.uuid, std.outbuffer, std.string, std.stdio;
import veda.common.logger, veda.core.common.define, veda.common.type;
import veda.core.common.know_predicates, veda.util.module_info;
import veda.authorization.right_set, veda.storage.common, veda.authorization.cache;

string lstr = "                                                                           ";

interface Authorization
{
    ubyte authorize(string _uri, string user_uri, ubyte _request_access, bool is_check_for_reload, OutBuffer _trace_acl, OutBuffer _trace_group,
                    OutBuffer _trace_info);

    public bool open();
    public void reopen();
    public void close();
}

abstract class ImplAuthorization : Authorization
{
    Logger    log;
    string    filter_value;
    ubyte     calc_right_res;

    RightSet  subject_groups;
    ubyte     request_access;
    ubyte[ string ] checked_groups;
    string    str;
    int       rc;
    int       str_num;
    int       count_permissions = 0;
    OutBuffer trace_acl;
    OutBuffer trace_group;
    OutBuffer trace_info;

    bool      use_cache = false;
    Cache     cache;

    ubyte authorize(string _uri, string user_uri, ubyte _request_access, bool is_check_for_reload, OutBuffer _trace_acl, OutBuffer _trace_group,
                    OutBuffer _trace_info)
    {
        if (use_cache)
        {
            if (cache is null)
            {
                cache = new Cache();
            }
            else
            {
                int res = cache.get(user_uri, _uri, _request_access);
                if (res != -1)
                {
                    stderr.writefln("res=%s", access_to_pretty_string(cast(ubyte)res));
                    return cast(ubyte)res;
                }
            }
        }

        trace_acl   = _trace_acl;
        trace_group = _trace_group;
        trace_info  = _trace_info;

        // reset local vars
        checked_groups = (ubyte[ string ]).init;
        calc_right_res = 0;
        str_num        = 0;
        request_access = _request_access;

        string uri = _uri.idup;

        //log.trace("%d authorize uri=%s, user=%s, request_access=%s", str_num++, uri, user_uri, access_to_pretty_string(request_access));

        if (open() == false)
            return calc_right_res;

        void reopen()
        {
            log.trace("reopen acl.R");
            reopen();
        }

        if (trace_info !is null)
            trace_info.write(format("%d authorize uri=%s, user=%s, request_access=%s\n", str_num++, uri, user_uri,
                                    access_to_pretty_string(request_access)));

        try
        {
            if (begin_transaction(is_check_for_reload) == false)
                return calc_right_res;

            try
            {
                string skey;

                // 0. читаем фильтр прав у object (uri)
                string filter = filter_prefix ~ uri;
                filter_value = get_in_current_transaction(filter);

                if (trace_info !is null)
                    trace_info.write(format("%d user_uri=%s\n", str_num++, user_uri));

                // читаем группы subject (ticket.user_uri)
                if (trace_info !is null)
                    trace_info.write(format("\n%d READ SUBJECT GROUPS\n", str_num++));

                ubyte[ string ] walked_groups;
                Right *[] groups;
                get_resource_groups(user_uri, 15, groups, walked_groups, 0);
                subject_groups = new RightSet(groups, log);

                subject_groups.data[ user_uri ] = new Right(user_uri, 15, false);
                if (trace_info !is null)
                    trace_info.write(format("%d subject_groups=%s\n", str_num++, subject_groups));

                // читаем группы object (uri)
                if (trace_info !is null)
                    trace_info.write(format("\n%d PREPARE OBJECT GROUPS\n", str_num++));

                ubyte[ string ] walked_groups1;

                if (trace_info is null && trace_group is null && trace_acl is null)
                {
                    if (authorize_obj_group(veda_schema__AllResourcesGroup, 15) == true)
                        return calc_right_res;

                    if (authorize_obj_group(uri, 15) == true)
                        return calc_right_res;

                    if (prepare_obj_group(uri, 15, walked_groups1, 0) == true)
                        return calc_right_res;
                }
                else
                {
                    // IF NEED TRACE
                    if (authorize_obj_group(veda_schema__AllResourcesGroup, 15) == true)
                    {
                        if (trace_info !is null)
                            trace_info.write(format("\n%d RETURN MY BE ASAP\n", str_num++));
                    }

                    if (authorize_obj_group(uri, 15) == true)
                    {
                        if (trace_info !is null)
                            trace_info.write(format("\n%d RETURN MY BE ASAP\n", str_num++));
                    }

                    Right *[] groups1;
                    if (get_resource_groups(uri, 15, groups1, walked_groups1, 0) == true)
                    {
                        if (trace_info !is null)
                            trace_info.write(format("\n%d RETURN MY BE ASAP\n", str_num++));
                    }

                    RightSet object_groups = new RightSet(groups1, log);

                    if (trace_info !is null)
                        trace_info.write(format("%d object_groups=%s\n", str_num++, object_groups));

                    foreach (object_group; object_groups.data)
                    {
                        if (authorize_obj_group(object_group.id,
                                                object_group.access) == true)
                        {
                            if (trace_info !is null)
                                trace_info.write(format("\n%d RETURN MY BE ASAP\n", str_num++));
                        }
                    }
                }
            }catch (Exception ex)
            {
                stderr.writeln("EX!,", ex.msg);
            }
            return calc_right_res;
        }
        finally
        {
            abort_transaction();

            if (use_cache)
            {
                cache.put(user_uri, _uri, request_access, calc_right_res);
            }

            if (trace_info !is null)
                trace_info.write(format("%d authorize %s, request=%s, answer=[%s]\n", str_num++, uri, access_to_pretty_string(request_access),
                                        access_to_pretty_string(calc_right_res)));
        }
    }

    private bool authorize_obj_group(string object_group_id, ubyte object_group_access)
    {
        bool  is_authorized = false;
        ubyte calc_bits     = 0;

        try
        {
            if (trace_group is null && trace_info is null && trace_acl is null)
            {
                ubyte left_to_check = (calc_right_res ^ request_access) & request_access;

                if ((left_to_check & object_group_access) == 0)
                    return is_authorized;

                ubyte cgrr = checked_groups.get(object_group_id, 0);

                if (cgrr == object_group_access)
                    return is_authorized;

                checked_groups[ object_group_id ] = object_group_access;
            }

            if (trace_group !is null)
                trace_group.write(object_group_id ~ "\n");

            string acl_key;
            if (filter_value !is null)
                acl_key = permission_prefix ~ filter_value ~ object_group_id;
            else
                acl_key = permission_prefix ~ object_group_id;

            if (trace_info !is null)
                trace_info.write(format("%d look acl_key: [%s]\n", str_num++, acl_key));

            str = get_in_current_transaction(acl_key);
            if (str != null)
            {
                RightSet permissions = new RightSet(log);
                rights_from_string(str, permissions);

                string obj_key = object_group_id;

                if (permissions !is null)
                {
                    foreach (subj_id; permissions.data.keys)
                    {
                        if (subj_id in subject_groups.data)
                        {
                            //log.trace("@ authorize_obj_group %s %s %s %s", subj_id, object_group_id, access_to_pretty_string(
                            //                                                                                                 object_group_access),
                            //          access_to_pretty_string(calc_right_res));
                            Right *permission = permissions.data.get(subj_id, null);

                            if (trace_info !is null)
                                trace_info.write(format("%d restriction=%s %s, permission=%s, request=%s\n", str_num++, object_group_id,
                                                        access_to_pretty_string(object_group_access),
                                                        *permission,
                                                        access_to_pretty_string(request_access)));

                            ubyte restriction_access, permission_access;

                            restriction_access = object_group_access;

                            if (permission !is null)
                            {
                                if (permission.access > 15)
                                    permission_access = (((permission.access & 0xF0) >> 4) ^ 0x0F) & permission.access;
                                else
                                    permission_access = permission.access;
                            }

                            foreach (int idx, access; access_list)
                            {
                                if ((request_access & access & restriction_access) != 0)
                                {
                                    calc_bits = cast(ubyte)(access & permission_access);

                                    if (calc_bits > 0)
                                    {
//                                          if (trace !is null)
//                                              trace(buff_object_group[ pos ], buff_subject_group[ pos ], access_list_predicates[ idx ]);

                                        calc_right_res = cast(ubyte)(calc_right_res | calc_bits);

                                        if ((calc_right_res & request_access) == request_access)
                                        {
                                            if (trace_info !is null)
                                                trace_info.write(format("%d EXIT? request_access=%s, res=%s\n", str_num++,
                                                                        access_to_pretty_string(request_access),
                                                                        access_to_pretty_string(calc_right_res)));
                                            else if (trace_group is null)
                                            {
                                                is_authorized = true;

                                                if (use_cache)
                                                {
                                                    cache.put(subj_id, object_group_id, request_access, calc_right_res);
                                                }

                                                return is_authorized;
                                            }
                                        }

                                        if (trace_info !is null)
                                            trace_info.write(format("%d calc_bits=%s, res=%s\n", str_num++, access_to_pretty_string(calc_bits),
                                                                    access_to_pretty_string(calc_right_res)));

                                        if (trace_acl !is null)
                                            trace_acl.write(format("%s;%s;%s\n", obj_key, subj_id, access_list_predicates[ idx ]));
                                    }
                                }
                            }
                        }
                    }
                }

                if (trace_info !is null)
                    trace_info.write(format("%d for [%s] found %s\n", str_num++, acl_key, permissions));
            }

            if ((calc_right_res & request_access) == request_access)
            {
                if (trace_info !is null)
                    trace_info.write(format("%d EXIT? request_access=%s, res=%s\n", str_num++, access_to_pretty_string(request_access),
                                            access_to_pretty_string(calc_right_res)));

                if (trace_info is null && trace_group is null && trace_acl is null)
                {
                    is_authorized = true;
                    return is_authorized;
                }
            }

            return is_authorized;
        }
        finally
        {
        }
    }


    private bool get_resource_groups(string uri, ubyte access, ref Right *[] result_set, ref ubyte[ string ] walked_groups, int level = 0)
    {
        if (level > 32)
        {
            if (trace_info !is null)
                trace_info.write(format("%d ERR! level down > 32, uri=%s\n", str_num++, uri));

            return false;
        }

        string ll;

        if (trace_info !is null)
        {
            if (level * 2 < lstr.length)
                ll = lstr[ 0..level * 2 ];
            else
                ll = text(level);
        }

        try
        {
            string groups_str = get_in_current_transaction(membership_prefix ~ uri);
            if (groups_str != null)
                rights_from_string(groups_str, result_set);

            //if (trace_info !is null)
            //{
            //    foreach (el; result_set)
            //        trace_info.write(format("%s (%d) GROUP FROM DB [%s]", ll, level, *el));
            //}

            long res_lenght = result_set.length;

            for (int idx = 0; idx < res_lenght; idx++)
            {
                Right *group = result_set[ idx ];

                if (group is null)
                {
                    log.trace("WARN! WARN! group is null, uri=%s, idx=%d", uri, idx);
                    continue;
                }

                ubyte orig_access = group.access;
                ubyte new_access  = group.access & access;
                group.access = new_access;

                if (group.id in walked_groups)
                {
                    ubyte preur_access = walked_groups[ group.id ];
                    if (preur_access == new_access)
                    {
                        if (trace_info !is null)
                            trace_info.write(format("%d %s (%d)GROUP [%s].access=%s SKIP, ALREADY ADDED\n", str_num++, ll, level, group.id,
                                                    access_to_pretty_string(preur_access)));

                        continue;
                    }
                }

                walked_groups[ group.id ] = new_access;

                if (trace_info !is null)
                    trace_info.write(format("%d %s (%d)GROUP [%s] %s-> %s\n", str_num++, ll, level, group.id, access_to_pretty_string(orig_access),
                                            access_to_pretty_string(new_access)));

                if (uri == group.id)
                {
                    if (trace_info !is null)
                        trace_info.write(format("%d %s (%d)GROUP [%s].access=%s SKIP, uri == group_key\n", str_num++, ll, level, group.id,
                                                access_to_pretty_string(orig_access)));
                    continue;
                }

                if (use_cache)
                {
                    if (level == 0)
                        cache.add_group(uri, null);

                    cache.add_group(uri, group.id);
                }

                Right *[] up_restrictions;
                get_resource_groups(group.id, new_access, up_restrictions, walked_groups, level + 1);

                RightSet trs = new RightSet(up_restrictions, log);

                foreach (restriction; trs.data)
                {
                    result_set ~= restriction;
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]", level, ex.line, ex.file, ex.info);

            if (trace_info !is null)
                trace_info.write(format("%d ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]\n", str_num++, level, ex.line, ex.file, ex.info));

            return false;
        }

        return false;
    }

    private bool prepare_obj_group(string uri, ubyte access, ref ubyte[ string ] walked_groups, int level = 0)
    {
        if (level > 32)
        {
            return false;
        }

        try
        {
            string    groups_str = get_in_current_transaction(membership_prefix ~ uri);

            Right *[] groups_set;

            if (groups_str != null)
                rights_from_string(groups_str, groups_set);

            for (int idx = 0; idx < groups_set.length; idx++)
            {
                Right *group = groups_set[ idx ];

                if (group is null)
                {
                    log.trace("WARN! WARN! group is null, uri=%s, idx=%d", uri, idx);
                    continue;
                }

                ubyte orig_access = group.access;
                ubyte new_access  = group.access & access;
                group.access = new_access;

                if (group.id in walked_groups)
                {
                    ubyte preur_access = walked_groups[ group.id ];
                    if (preur_access == new_access)
                        continue;
                }

                walked_groups[ group.id ] = new_access;

                if (uri == group.id)
                    continue;

                if (use_cache)
                {
                    if (level == 0)
                        cache.add_group(uri, null);

                    cache.add_group(uri, group.id);
                }

                if (authorize_obj_group(group.id, group.access) == true)
                    return true;

                prepare_obj_group(group.id, new_access, walked_groups, level + 1);
            }
        }
        catch (Throwable ex)
        {
            log.trace("ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]", level, ex.line, ex.file, ex.info);

            return false;
        }

        return false;
    }


    abstract string get_in_current_transaction(string in_key);
    abstract void abort_transaction();
    abstract bool begin_transaction(bool is_check_for_reload);
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
