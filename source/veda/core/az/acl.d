module veda.core.az.acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.common.type, veda.onto.individual, veda.onto.resource, veda.bind.lmdb_header, veda.core.common.context, veda.core.common.define,
           veda.core.common.know_predicates, veda.core.common.log_msg, veda.common.type;
    import veda.core.util.utils, veda.common.logger;
    import veda.core.storage.lmdb_storage, veda.core.az.right_set;
    import veda.util.container, veda.util.module_info;
}

string lstr = "                                                                           ";

/// Хранение, чтение PermissionStatement, Membership
class Authorization : LmdbStorage
{
    Logger log;

    this(string _path, DBMode mode, string _parent_thread_name, Logger _log)
    {
        log = _log;
        super(_path, mode, _parent_thread_name, log);
    }

    int count_permissions = 0;

    override void reopen_db()
    {
        super.reopen_db();
    }

    override public void unload_to_queue(string path, string queue_id, bool only_ids)
    {
        super.unload_to_queue(path, queue_id, only_ids);
    }

    ubyte authorize(string _uri, Ticket *ticket, ubyte request_access, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                               string subject_group,
                                                                                                               string right)
                    trace_acl,
                    void delegate(string resource_group) trace_group, void delegate(string log) trace_info
                    )
    {
        ubyte res = 0;

        //if (cache_right_result !is null && trace_group is null && trace_info is null)
        //{
        //	res = cache_right_result.get (_uri ~ ticket.user_uri ~ request_access);

        //	if (res != 0)
        //		return res;
        //}

        int    str_num = 0;
        string uri     = _uri.idup;

        if (db_is_opened == false)
            open_db();

        void reopen_db()
        {
            log.trace("reopen acl.R");
            this.reopen_db();
        }

        if (ticket is null)
        {
            log.trace("ERR! authorize uri=%s, request_access=%s, ticket IS NULL", uri, access_to_pretty_string(request_access));
            return request_access;
        }

        if (db_is_opened == false)
            return res;

        if (trace_info !is null)
            trace_info(format("%d authorize uri=%s, user=%s, request_access=%s", str_num++, uri, ticket.user_uri,
                              access_to_pretty_string(request_access)));

        MDB_txn *txn_r;
        MDB_dbi dbi;
        string  str;
        int     rc;

        if (is_check_for_reload)
            acl_check_for_reload(&reopen_db);

        //if (db_is_open.get(path, false) == false)
        //    return res;

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
                reopen_db();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace("WARN! 2: find:" ~ text(__LINE__) ~ ",%s) MDB_BAD_RSLOT", path);
                mdb_txn_abort(txn_r);

                // TODO: sleep ?
                //core.thread.Thread.sleep(dur!("msecs")(1));
                //rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
                reopen_db();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ ",%s) ERR! %s", path, fromStringz(mdb_strerror(rc)));
            return res;
        }

        try
        {
            rc = mdb_dbi_open(txn_r, null, MDB_CREATE, &dbi);
            if (rc != 0)
                throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));

            //RightSet[ string ] permission_2_group;

            MDB_val   key;
            MDB_val   data;
            string    skey;

            Right *[] _get_resource_groups(string uri, ubyte access, ref ubyte[ string ] prepared_uris, int level = 0)
            {
                if (level > 16)
                {
                    //log.trace("WARN! level down > 16, uri=%s", uri);
                }

                if (level > 32)
                {
                    log.trace("ERR! level down > 32, uri=%s", uri);
                    return (Right *[]).init;
                }


                Right *[] res;

                string    ll;

                if (trace_info !is null)
                {
                    if (level * 2 < lstr.length)
                        ll = lstr[ 0..level * 2 ];
                    else
                        ll = text(level);
                }

                try
                {
                    string groups_str;
                    key.mv_size = uri.length;
                    key.mv_data = cast(char *)uri;
                    rc          = mdb_get(txn_r, dbi, &key, &data);
                    if (rc == 0)
                    {
                        groups_str = cast(string)(data.mv_data[ 0..data.mv_size ]);
                        rights_from_string(groups_str, res);
                    }

                    //if (trace_info !is null)
                    //{
                    //    foreach (el; res)
                    //        trace_info(format("%s (%d) GROUP FROM DB [%s]", ll, level, *el));
                    //}

                    long res_lenght = res.length;

                    for (int idx = 0; idx < res_lenght; idx++)
                    {
                        Right *group = res[ idx ];

                        if (group is null)
                        {
                            log.trace("WARN! WARN! group is null, uri=%s, idx=%d", uri, idx);
                            continue;
                        }

                        ubyte orig_access = group.access;
                        ubyte new_access  = group.access & access;
                        group.access = new_access;

                        if (group.id in prepared_uris)
                        {
                            ubyte preur_access = prepared_uris[ group.id ];
                            if (preur_access == new_access)
                            {
                                if (trace_info !is null)
                                    trace_info(format("%d %s (%d)GROUP [%s].access=%s SKIP, ALREADY ADDED", str_num++, ll, level, group.id,
                                                      access_to_pretty_string(preur_access)));

                                continue;
                            }
                        }

                        prepared_uris[ group.id ] = new_access;

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

                        Right *[] up_restrictions = _get_resource_groups(group_key, new_access, prepared_uris, level + 1);
                        foreach (restriction; up_restrictions)
                        {
                            res ~= restriction;
                        }
                    }
                }
                catch (Throwable ex)
                {
                    log.trace("ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]", level, ex.line, ex.file, ex.info);
                }

                return res;
            }

            RightSet get_resource_groups(string uri, ubyte access)
            {
                ubyte[ string ] prepared_uris;
                auto groups = _get_resource_groups(uri, access, prepared_uris, 0);

                if (trace_info !is null)
                {
                    //foreach (el; groups)
                    //    trace_info(format("%d FOUND GROUP [%s]", str_num++, *el));
                }

                return new RightSet(groups, log);
            }

            // 0. читаем фильтр прав у object (uri)
            string filter = filter_prefix ~ uri;
            string filter_value;
            key.mv_size = filter.length;
            key.mv_data = cast(char *)filter;
            rc          = mdb_get(txn_r, dbi, &key, &data);
            if (rc == 0)
                filter_value = cast(string)(data.mv_data[ 0..data.mv_size ]).dup;

            if (trace_info !is null)
                trace_info(format("%d user_uri=%s", str_num++, ticket.user_uri));

            // читаем группы subject (ticket.user_uri)
            if (trace_info !is null)
                trace_info(format("\n%d READ SUBJECT GROUPS", str_num++));
            RightSet subject_groups = get_resource_groups(membership_prefix ~ ticket.user_uri, 15);
            subject_groups.data[ ticket.user_uri ] = new Right(ticket.user_uri, 15, false);
            if (trace_info !is null)
                trace_info(format("%d subject_groups=%s", str_num++, subject_groups));

            // читаем группы object (uri)
            if (trace_info !is null)
                trace_info(format("\n%d READ OBJECT GROUPS", str_num++));
            RightSet object_groups = get_resource_groups(membership_prefix ~ uri, 15);
            object_groups.data[ uri ]                            = new Right(uri, 15, false);
            object_groups.data[ veda_schema__AllResourcesGroup ] = new Right(veda_schema__AllResourcesGroup, 15, false);
            if (trace_info !is null)
                trace_info(format("%d object_groups=%s", str_num++, object_groups));

            foreach (object_group; object_groups.data)
            {
                if (trace_group !is null)
                    trace_group(object_group.id);

                string acl_key;
                if (filter_value !is null)
                    acl_key = permission_prefix ~ filter_value ~ object_group.id;
                else
                    acl_key = permission_prefix ~ object_group.id;

                if (trace_info !is null)
                    trace_info(format("%d look acl_key: [%s]", str_num++, acl_key));

                key.mv_size = acl_key.length;
                key.mv_data = cast(char *)acl_key;

                rc = mdb_get(txn_r, dbi, &key, &data);
                if (rc == 0)
                {
                    str = cast(string)(data.mv_data[ 0..data.mv_size ]);
                    RightSet permissions = new RightSet(log);
                    rights_from_string(str, permissions);

                    string obj_key = object_group.id;

                    if (permissions !is null)
                    {
                        foreach (perm_key; permissions.data.keys)
                        {
                            if (perm_key in subject_groups.data)
                            {
                                Right *restriction = object_groups.data.get(obj_key, null);
                                Right *permission  = permissions.data.get(perm_key, null);

                                if (trace_info !is null)
                                    trace_info(format("%d restriction=%s, permission=%s, request=%s", str_num++, *restriction, *permission,
                                                      access_to_pretty_string(request_access)));

                                ubyte restriction_access, permission_access;

                                if (restriction !is null)
                                    restriction_access = restriction.access;

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
//                            				if (trace !is null)
//                                				trace(buff_object_group[ pos ], buff_subject_group[ pos ], access_list_predicates[ idx ]);

                                            res = cast(ubyte)(res | set_bit);

                                            if ((res & request_access) == request_access)
                                            {
                                            	 if (trace_info !is null)
	                                                trace_info(format("%d EXIT? request_access=%s, res=%s", str_num++, access_to_pretty_string(request_access),
                                                                  access_to_pretty_string(res)));
                                            	 else if (trace_group is null) 
	                                            	 return res;
                                            }

                                            if (trace_info !is null)
                                                trace_info(format("%d set_bit=%s, res=%s", str_num++, access_to_pretty_string(set_bit),
                                                                  access_to_pretty_string(res)));

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
            }
        }catch (Exception ex)
        {
            writeln("EX!,", ex.msg);
        }

        mdb_txn_abort(txn_r);

        scope (exit)
        {
            if (trace_info !is null)
                trace_info(format("%d authorize %s, request=%s, answer=[%s]", str_num++, uri, access_to_pretty_string(request_access),
                                  access_to_pretty_string(res)));
        }

        if (mode == DBMode.R)
        {
            records_in_memory[ uri ] = 1;

            if (records_in_memory.length > max_count_record_in_memory)
            {
                log.trace("acl: records_in_memory > max_count_record_in_memory (%d)", max_count_record_in_memory);
                reopen_db();
            }
        }

        return res;
    }

    //finally {
    //cache_right_result.put(_uri ~ ticket.user_uri ~ request_access, res);
    //}
//}
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
    //return _check_for_reload(acl_local_time_check, acl_local_count, &get_acl_manager_op_id, load);
}

unittest
{
    import veda.core.az.right_set;
    import veda.util.tests_tools;

    Logger log = new Logger("test", "log", "ACL");

    string test_path = get_test_path();
    string az_path   = test_path ~ "/az";

    try
    {
        mkdir(az_path);
    }
    catch (Exception ex)
    {
    }

    Authorization storage = new Authorization(az_path, DBMode.RW, "test", log);

    assert(storage !is null);

    Individual new_ind;
    Individual prev_ind;
    long       op_id = 0;

    string     user_uri = "d:user1";
    string     indv_uri = "d:indv1";

    new_ind.addResource("rdf:type", Resource(DataType.Uri, "v-s:PermissionStatement"));
    new_ind.addResource("v-s:canRead", Resource(true));
    new_ind.addResource("v-s:permissionObject", Resource(DataType.Uri, indv_uri));
    new_ind.addResource("v-s:permissionSubject", Resource(DataType.Uri, user_uri));

    prepare_right_set(prev_ind, new_ind, veda_schema__permissionObject, veda_schema__permissionSubject, permission_prefix, 0, op_id, storage);

    storage.flush(1);

    Ticket ticket;

    ticket.user_uri = user_uri;

    ubyte res = storage.authorize(indv_uri, &ticket, Access.can_read, false, null, null);
    assert(res == Access.can_read);

    ubyte res1 = storage.authorize(indv_uri, &ticket, Access.can_update, false, null, null);
    assert(res1 != Access.can_update);

    ubyte res2 = storage.authorize(indv_uri ~ "_1", &ticket, Access.can_update, false, null, null);
    assert(res2 != Access.can_update);

    try
    {
        rmdir("./tmp/az");
    }
    catch (Exception ex)
    {
    }

    try
    {
        rmdir("./tmp");
    }
    catch (Exception ex)
    {
    }

    writeln("unittest [Authorization: store permission] Ok");
}

