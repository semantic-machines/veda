module veda.storage.lmdb.lmdb_acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.common.type, veda.onto.individual, veda.onto.resource, veda.bind.lmdb_header, veda.core.common.context, veda.core.common.define,
           veda.core.common.know_predicates, veda.core.common.log_msg, veda.common.type;
    import veda.core.util.utils, veda.common.logger;
    import veda.storage.lmdb.lmdb_driver, veda.storage.right_set, veda.storage.common;
    import veda.util.container, veda.util.module_info;
}

const string   acl_indexes_db_path   = "./data/acl-indexes";

static this ()
{
	paths_list ~= acl_indexes_db_path;
}

string lstr = "                                                                           ";

/// Хранение, чтение PermissionStatement, Membership
class LmdbAuthorization : LmdbDriver, Authorization
{
    Logger log;

    // short life time vars

    MDB_val                              key;
    MDB_val                              data;
    MDB_txn                              *txn_r;
    MDB_dbi                              dbi;
    string                               str;
    int                                  rc;
    string                               filter_value;
    ubyte                                calc_right_res;

    int                                  str_num;
    RightSet                             subject_groups;
    RightSet                             object_groups;
    void delegate(string resource_group) trace_group;
    void delegate(string log)            trace_info;
    void delegate(string resource_group,
                  string subject_group,
                  string right)
          trace_acl;
    ubyte request_access;
    ubyte[ string ] checked_groups;

    this(DBMode mode, string _parent_thread_name, Logger _log)
    {
        log = _log;
        super(acl_indexes_db_path, mode, _parent_thread_name, log);
    }

    int count_permissions = 0;

    override void reopen()
    {
        super.reopen();
    }

    override void open()
    {
        super.open();
    }

    override void close()
    {
        super.close();
    }

    override void flush(int force)
    {
        super.flush(force);
    }

    ubyte authorize(string _uri, Ticket *ticket, ubyte _request_access, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                                string subject_group,
                                                                                                                string right)
                    _trace_acl,
                    void delegate(string resource_group) _trace_group, void delegate(string log) _trace_info
                    )
    {
        checked_groups = (ubyte[ string ]).init;
        trace_group    = _trace_group;
        trace_info     = _trace_info;
        trace_acl      = _trace_acl;
        request_access = _request_access;
        calc_right_res = 0;
        str_num        = 0;

        string uri = _uri.idup;

        //log.trace("%d authorize uri=%s, user=%s, request_access=%s", str_num++, uri, ticket.user_uri,
        //          access_to_pretty_string(request_access));


        if (db_is_opened == false)
            open();

        void reopen()
        {
            log.trace("reopen acl.R");
            this.reopen();
        }

        if (ticket is null)
        {
            log.trace("ERR! authorize uri=%s, request_access=%s, ticket IS NULL", uri, access_to_pretty_string(request_access));
            return request_access;
        }

        if (db_is_opened == false)
            return calc_right_res;

        if (trace_info !is null)
            trace_info(format("%d authorize uri=%s, user=%s, request_access=%s", str_num++, uri, ticket.user_uri,
                              access_to_pretty_string(request_access)));

        if (is_check_for_reload)
            acl_check_for_reload(&reopen);

        //if (db_is_open.get(path, false) == false)
        //    return res;

        if (begin_transaction() == false)
            return calc_right_res;

        try
        {
            string skey;

            // 0. читаем фильтр прав у object (uri)
            string filter = filter_prefix ~ uri;
            filter_value = get_from_storage(filter);

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

    private string get_from_storage(string in_key)
    {
        string sres;

        key.mv_size = in_key.length;
        key.mv_data = cast(char *)in_key;
        rc          = mdb_get(txn_r, dbi, &key, &data);
        if (rc == 0)
            sres = cast(string)(data.mv_data[ 0..data.mv_size ]).dup;

        return sres;
    }

    private void abort_transaction()
    {
        if (txn_r != null)
            mdb_txn_abort(txn_r);
    }

    private bool begin_transaction()
    {
        rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            log.trace("WARN! find 1:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", path);
            for (int i = 0; i < 10 && rc != 0; i++)
            {
                mdb_txn_abort(txn_r);

                if (i > 3)
                {
                    log.trace("WARN! find 1:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", path);
                    core.thread.Thread.sleep(dur!("msecs")(10));
                }

                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) %s", path, fromStringz(mdb_strerror(rc)));
                reopen();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace("WARN! 2: find:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                //core.thread.Thread.sleep(dur!("msecs")(1));
                //rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
                reopen();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) ERR! %s", path, fromStringz(mdb_strerror(rc)));
            return false;
        }

        rc = mdb_dbi_open(txn_r, null, MDB_CREATE, &dbi);
        if (rc != 0)
            throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));

        return true;
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

        str = get_from_storage(acl_key);
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
                            trace_info(format("%d restriction=%s %s, permission=%s, request=%s", str_num++, object_group_id, access_to_pretty_string(object_group_access),
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


    private bool get_resource_groups(bool is_check_right, string uri, ubyte access, ref Right *[] result_set, ref ubyte[ string ] walked_groups, int level = 0)
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
            string groups_str = get_from_storage(uri);
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
}

private ModuleInfoFile[ MODULE ] info_r__2__pmodule;
private MInfo get_info(MODULE module_id)
{
    ModuleInfoFile mdif = info_r__2__pmodule.get(module_id, null);

    if (mdif is null)
    {
        mdif                            = new ModuleInfoFile(text(module_id), log, OPEN_MODE.READER);
        info_r__2__pmodule[ module_id ] = mdif;
    }
    MInfo info = mdif.get_info();
    return info;
}

int  _timeout                         = 10;
long last_committed_op_id_acl_manager = 0;
public bool acl_check_for_reload(void delegate() load)
{
    MInfo mi = get_info(MODULE.acl_preparer);

    //log.trace ("acl_check_for_reload #1, last_committed_op_id_acl_manager=%d, mi=%s", last_committed_op_id_acl_manager, mi);
    if (last_committed_op_id_acl_manager < mi.committed_op_id)
    {
        last_committed_op_id_acl_manager = mi.committed_op_id;
        //log.trace ("acl_check_for_reload #2, last_committed_op_id_acl_manager=%d", last_committed_op_id_acl_manager);
        return true;
    }
    return false;
}
