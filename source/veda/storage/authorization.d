module veda.storage.authorization;

import std.conv, std.datetime, std.uuid, std.outbuffer, std.string, std.stdio;
import veda.common.logger, veda.core.common.define, veda.common.type;
import veda.core.common.know_predicates, veda.util.module_info;
import veda.storage.right_set, veda.storage.common;

string lstr = "                                                                           ";

abstract class Authorization
{
	Logger 								 log;
    string                               filter_value;
    ubyte                                calc_right_res;

    RightSet                             subject_groups;
    RightSet                             object_groups;
    void delegate(string resource_group) trace_group;
    void delegate(string log)            trace_info;
    void delegate(string resource_group,
                  string subject_group,
                  string right)
           trace_acl;
    ubyte  request_access;
    ubyte[ string ] checked_groups;
    string str;
    int    rc;
    int    str_num;
    int    count_permissions = 0;

    ubyte authorize(string _uri, Ticket *ticket, ubyte _request_access, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                                string subject_group,
                                                                                                                string right)
                    _trace_acl,
                    void delegate(string resource_group) _trace_group, void delegate(string log) _trace_info
                    )
    {
        // reset local vars
        checked_groups = (ubyte[ string ]).init;
        calc_right_res = 0;
        str_num        = 0;
        trace_group    = _trace_group;
        trace_info     = _trace_info;
        trace_acl      = _trace_acl;
        request_access = _request_access;

        string uri = _uri.idup;

        //log.trace("%d authorize uri=%s, user=%s, request_access=%s", str_num++, uri, ticket.user_uri,
        //          access_to_pretty_string(request_access));

        if (ticket is null)
        {
            log.trace("ERR! authorize uri=%s, request_access=%s, ticket IS NULL", uri, access_to_pretty_string(request_access));
            return request_access;
        }

        if (open() == false)
            return calc_right_res;

        void reopen()
        {
            log.trace("reopen acl.R");
            reopen();
        }

        if (trace_info !is null)
            trace_info(format("%d authorize uri=%s, user=%s, request_access=%s", str_num++, uri, ticket.user_uri,
                              access_to_pretty_string(request_access)));

        //if (db_is_open.get(path, false) == false)
        //    return res;

        if (begin_transaction(is_check_for_reload) == false)
            return calc_right_res;

        try
        {
            string skey;

            // 0. читаем фильтр прав у object (uri)
            string filter = filter_prefix ~ uri;
            filter_value = get_in_current_transaction(filter);

            if (trace_info !is null)
                trace_info(format("%d user_uri=%s", str_num++, ticket.user_uri));

            // читаем группы subject (ticket.user_uri)
            if (trace_info !is null)
                trace_info(format("\n%d READ SUBJECT GROUPS", str_num++));

            ubyte[ string ] walked_groups;
            Right *[] groups;
            get_resource_groups(false, membership_prefix ~ ticket.user_uri, 15, groups, walked_groups, 0);
            subject_groups = new RightSet(groups, log);

            subject_groups.data[ ticket.user_uri ] = new Right(ticket.user_uri, 15, false);
            if (trace_info !is null)
                trace_info(format("%d subject_groups=%s", str_num++, subject_groups));

            // читаем группы object (uri)
            if (trace_info !is null)
                trace_info(format("\n%d READ OBJECT GROUPS", str_num++));

            ubyte[ string ] walked_groups1;
            Right *[] groups1;

            if (prepare_group(veda_schema__AllResourcesGroup, 15) == true && trace_info is null && trace_group is null && trace_acl is null)
            {
                if (trace_info !is null)
                    trace_info(format("\n%d RETURN MY BE ASAP", str_num++));

                return calc_right_res;
            }

            if (prepare_group(uri, 15) == true && trace_info is null && trace_group is null && trace_acl is null)
            {
                if (trace_info !is null)
                    trace_info(format("\n%d RETURN MY BE ASAP", str_num++));

                return calc_right_res;
            }

            if (get_resource_groups(true, membership_prefix ~ uri, 15, groups1, walked_groups1,
                                    0) == true && trace_info is null && trace_group is null && trace_acl is null)
            {
                if (trace_info !is null)
                    trace_info(format("\n%d RETURN MY BE ASAP", str_num++));

                return calc_right_res;
            }

            if (trace_info is null && trace_group is null && trace_acl is null)
            {
                //object_groups = new RightSet(log);
            }
            else
            {
                object_groups = new RightSet(groups1, log);

                //object_groups.data[ uri ]                            = new Right(uri, 15, false);
                //object_groups.data[ veda_schema__AllResourcesGroup ] = new Right(veda_schema__AllResourcesGroup, 15, false);
                if (trace_info !is null)
                    trace_info(format("%d object_groups=%s", str_num++, object_groups));

                foreach (object_group; object_groups.data)
                {
                    if (prepare_group(object_group.id, object_group.access) == true && trace_info is null && trace_group is null && trace_acl is null)
                    {
                        if (trace_info !is null)
                            trace_info(format("\n%d RETURN MY BE ASAP", str_num++));
                        return calc_right_res;
                    }
                }
            }
        }catch (Exception ex)
        {
            writeln("EX!,", ex.msg);
        }

        abort_transaction();

        scope (exit)
        {
            if (trace_info !is null)
                trace_info(format("%d authorize %s, request=%s, answer=[%s]", str_num++, uri, access_to_pretty_string(request_access),
                                  access_to_pretty_string(calc_right_res)));
        }

        return calc_right_res;
    }

    private bool prepare_group(string object_group_id, ubyte object_group_access)
    {
        if (trace_group is null && trace_info is null && trace_acl is null)
        {
            ubyte left_to_check = (calc_right_res ^ request_access) & request_access;

            if ((left_to_check & object_group_access) == 0)
                return false;

            //if (((calc_right_res & object_group_access) == object_group_access) && ((object_group_access & request_access) == object_group_access))
            //{
            //    return false;
            //}

            ubyte cgrr = checked_groups.get(object_group_id, 0);

            if (cgrr == object_group_access)
                return false;

            checked_groups[ object_group_id ] = object_group_access;
        }

        //log.trace("@ prepare_group %s %s %s", object_group_id, access_to_pretty_string(object_group_access), access_to_pretty_string(calc_right_res));

        if (trace_group !is null)
            trace_group(object_group_id);

        string acl_key;
        if (filter_value !is null)
            acl_key = permission_prefix ~ filter_value ~ object_group_id;
        else
            acl_key = permission_prefix ~ object_group_id;

        if (trace_info !is null)
            trace_info(format("%d look acl_key: [%s]", str_num++, acl_key));

        str = get_in_current_transaction(acl_key);
        if (str != null)
        {
            RightSet permissions = new RightSet(log);
            rights_from_string(str, permissions);

            string obj_key = object_group_id;

            if (permissions !is null)
            {
                foreach (perm_key; permissions.data.keys)
                {
                    if (perm_key in subject_groups.data)
                    {
                        //Right *restriction = object_group;
                        Right *permission = permissions.data.get(perm_key, null);

                        if (trace_info !is null)
                            trace_info(format("%d restriction=%s %s, permission=%s, request=%s", str_num++, object_group_id,
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
                                ubyte set_bit = cast(ubyte)(access & permission_access);

                                if (set_bit > 0)
                                {
//                                          if (trace !is null)
//                                              trace(buff_object_group[ pos ], buff_subject_group[ pos ], access_list_predicates[ idx ]);

                                    calc_right_res = cast(ubyte)(calc_right_res | set_bit);

                                    if ((calc_right_res & request_access) == request_access)
                                    {
                                        if (trace_info !is null)
                                            trace_info(format("%d EXIT? request_access=%s, res=%s", str_num++,
                                                              access_to_pretty_string(request_access),
                                                              access_to_pretty_string(calc_right_res)));
                                        else if (trace_group is null)
                                            return true;
                                    }

                                    if (trace_info !is null)
                                        trace_info(format("%d set_bit=%s, res=%s", str_num++, access_to_pretty_string(set_bit),
                                                          access_to_pretty_string(calc_right_res)));

                                    if (trace_acl !is null)
                                    {
                                        trace_acl(obj_key, perm_key, access_list_predicates[ idx ]);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (trace_info !is null)
                trace_info(format("%d for [%s] found %s", str_num++, acl_key, permissions));
        }

        if ((calc_right_res & request_access) == request_access)
        {
            if (trace_info !is null)
                trace_info(format("%d EXIT? request_access=%s, res=%s", str_num++, access_to_pretty_string(request_access),
                                  access_to_pretty_string(calc_right_res)));

            if (trace_info is null && trace_group is null && trace_acl is null)
                return true;
        }

        return false;
    }


    private bool get_resource_groups(bool is_check_right, string uri, ubyte access, ref Right *[] result_set, ref ubyte[ string ] walked_groups,
                                     int level = 0)
    {
        //if (level > 16)
        //{
        //    log.trace("WARN! level down > 16, uri=%s", uri);
        //}

        if (level > 32)
        {
            log.trace("ERR! level down > 32, uri=%s", uri);
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
            string groups_str = get_in_current_transaction(uri);
            if (groups_str != null)
                rights_from_string(groups_str, result_set);

            //if (trace_info !is null)
            //{
            //    foreach (el; res)
            //        trace_info(format("%s (%d) GROUP FROM DB [%s]", ll, level, *el));
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
                            trace_info(format("%d %s (%d)GROUP [%s].access=%s SKIP, ALREADY ADDED", str_num++, ll, level, group.id,
                                              access_to_pretty_string(preur_access)));

                        continue;
                    }
                }

                walked_groups[ group.id ] = new_access;

                if (trace_info !is null)
                    trace_info(format("%d %s (%d)GROUP [%s] %s-> %s", str_num++, ll, level, group.id, access_to_pretty_string(orig_access),
                                      access_to_pretty_string(new_access)));

                string group_key = membership_prefix ~ group.id;
                if (uri == group_key)
                {
                    if (trace_info !is null)
                        trace_info(format("%d %s (%d)GROUP [%s].access=%s SKIP, uri == group_key", str_num++, ll, level, group.id,
                                          access_to_pretty_string(orig_access)));
                    continue;
                }

                if (is_check_right == true && trace_info is null && trace_group is null && trace_acl is null)
                {
                    if (prepare_group(group.id, group.access) == true)
                        return true;
                }

                //if (is_check_right == true && level == 0 && trace_info is null && trace_group is null && trace_acl is null)
                //{
                //    foreach (gr; result_set)
                //    {
                //        if (prepare_group(gr.id, gr.access) == true)
                //            return true;
                //    }
                //}

                Right *[] up_restrictions;
                get_resource_groups(is_check_right, group_key, new_access, up_restrictions, walked_groups, level + 1);

                RightSet trs = new RightSet(up_restrictions, log);

                //if (is_check_right == true && level == 0 && trace_info is null && trace_group is null && trace_acl is null)
                //{
                //    foreach (gr; trs.data)
                //    {
                //        if (prepare_group(gr.id, gr.access) == true)
                //            return true;
                //    }
                //}

                foreach (restriction; trs.data)
                {
                    result_set ~= restriction;
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]", level, ex.line, ex.file, ex.info);
            return false;
        }

        return false;
    }


    abstract public bool open();
    abstract public void reopen();
    abstract public void close();

    abstract string get_in_current_transaction(string in_key);
    abstract void abort_transaction();
    abstract bool begin_transaction(bool is_check_for_reload);
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
