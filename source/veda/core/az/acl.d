module veda.core.az.acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.common.type, veda.onto.individual, veda.onto.resource, veda.bind.lmdb_header, veda.core.common.context, veda.core.common.define,
           veda.core.common.know_predicates, veda.core.common.log_msg;
    import veda.core.util.utils, veda.common.logger;
    import veda.core.storage.lmdb_storage, veda.core.az.right_set;
    import veda.util.container;
}

int max_count_in_cache = 200;

/// Хранение, чтение PermissionStatement, Membership
class Authorization : LmdbStorage
{
    Logger log;
    Cache!(Right *[], string) cache_of_group;
    Cache!(RightSet, string) cache_of_permission;

    this(string _path, DBMode mode, string _parent_thread_name, Logger _log)
    {
        log = _log;
        super(_path, mode, _parent_thread_name, log);
        //cache_of_group      = new Cache!(Right *[], string)(max_count_in_cache, "group");
        //cache_of_permission = new Cache!(RightSet, string)(max_count_in_cache, "permission");
    }

    int count_permissions = 0;

    override void reopen_db()
    {
        super.reopen_db();

        if (cache_of_group !is null)
            cache_of_group = new Cache!(Right *[], string)(max_count_in_cache, "group");

        if (cache_of_permission !is null)
            cache_of_permission = new Cache!(RightSet, string)(max_count_in_cache, "permission");

        //writeln("ACL:CACHE:RESET");
    }

    ubyte authorize(string _uri, Ticket *ticket, ubyte request_access, Context context, bool is_check_for_reload, void delegate(string resource_group,
                                                                                                                                string subject_group,
                                                                                                                                string right)
                    trace_acl,
                    void delegate(string resource_group) trace_group
                    )
    {
        string uri = _uri.idup;

        if (db_is_opened == false)
            open_db();

        void reopen_db()
        {
            log.trace("reopen acl.R");
            this.reopen_db();
        }

        ubyte res = 0;

        if (ticket is null)
        {
            log.trace("ERR! authorize uri=%s, request_access=%s, ticket IS NULL", uri, access_to_pretty_string(request_access));
            return request_access;
        }

        if (db_is_opened == false)
            return res;

        if (trace_msg[ 111 ] == 1)
            log.trace("authorize uri=%s, user=%s, request_access=%s", uri, ticket.user_uri, access_to_pretty_string(request_access));

        MDB_txn *txn_r;
        MDB_dbi dbi;
        string  str;
        int     rc;

        if (is_check_for_reload)
            context.acl_check_for_reload(&reopen_db);

        //if (db_is_open.get(path, false) == false)
        //    return res;

        rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            log.trace("WARN! find 1:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", path);
            for (int i = 0; i < 10 && rc != 0; i++)
            {
                mdb_txn_abort(txn_r);

                if (i > 3)
                {
                    log.trace("WARN! find 1:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", path);
                    core.thread.Thread.sleep(dur!("msecs")(10));
                }

                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace("WARN! " ~ __FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) %s", path, fromStringz(mdb_strerror(rc)));
                reopen_db();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace("WARN! 2: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", path);
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
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR! %s", path, fromStringz(mdb_strerror(rc)));
            return res;
        }

        try
        {
            rc = mdb_dbi_open(txn_r, null, MDB_CREATE, &dbi);
            if (rc != 0)
                throw new Exception(cast(string)("Fail:" ~  fromStringz(mdb_strerror(rc))));

            RightSet[ string ] permission_2_group;

            MDB_val   key;
            MDB_val   data;
            string    skey;

            Right *[] _get_resource_groups(string uri, ubyte access, ref bool[ string ] prepared_uris, int level = 0)
            {
                Right *[] res;

                //writeln ("~10 level=", level, ", uri=", uri);

                try
                {
                    string groups_str;
                    if (cache_of_group !is null)
                    {
                        res = cache_of_group.get(uri);
                        //writeln ("ACL:CACHE:found in cache, uri=", uri);
                    }

                    if (res is null)
                    {
                        key.mv_size = uri.length;
                        key.mv_data = cast(char *)uri;
                        rc          = mdb_get(txn_r, dbi, &key, &data);
                        if (rc == 0)
                        {
                            groups_str = cast(string)(data.mv_data[ 0..data.mv_size ]);
                            rights_from_string(groups_str, res);
                            if (cache_of_group !is null)
                                cache_of_group.put(uri, res);
                        }
                    }

                    long res_lenght = res.length;

                    for (int idx = 0; idx < res_lenght; idx++)
                    {
                        Right *group = res[ idx ];

                        if (prepared_uris.get(group.id, false) == true)
                            continue;

                        string group_key = membership_prefix ~ group.id;
                        group.access = group.access & access;
                        //res ~= group;
                        prepared_uris[ group.id ] = true;

                        if (uri == group_key)
                            continue;

                        Right *[] up_restrictions = _get_resource_groups(group_key, group.access & access, prepared_uris, level + 1);
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
                bool[ string ] prepared_uris;
                return new RightSet(_get_resource_groups(uri, access, prepared_uris, 0), log);
            }

            // 0. читаем фильтр прав у object (uri)
            string filter = filter_prefix ~ uri;
            string filter_value;
            key.mv_size = filter.length;
            key.mv_data = cast(char *)filter;
            rc          = mdb_get(txn_r, dbi, &key, &data);
            if (rc == 0)
            {
                filter_value = cast(string)(data.mv_data[ 0..data.mv_size ]).dup;
            }

            // 1. читаем группы object (uri)
            RightSet object_groups = get_resource_groups(membership_prefix ~ uri, 15);
            object_groups.data[ uri ]                            = new Right(uri, 15, false);
            object_groups.data[ veda_schema__AllResourcesGroup ] = new Right(veda_schema__AllResourcesGroup, 15, false);

            // 2. читаем группы subject (ticket.user_uri)
            RightSet subject_groups = get_resource_groups(membership_prefix ~ ticket.user_uri, 15);
            subject_groups.data[ ticket.user_uri ] = new Right(ticket.user_uri, 15, false);

            if (trace_msg[ 113 ] == 1)
            {
                log.trace("user_uri=%s", ticket.user_uri);
                log.trace("subject_groups=%s", subject_groups);
                log.trace("object_groups=%s", object_groups);
            }


            foreach (object_group; object_groups.data)
            {
                if (trace_group !is null)
                {
                    trace_group(object_group.id);
                }

                string acl_key;
                if (filter_value !is null)
                    acl_key = permission_prefix ~ filter_value ~ object_group.id;
                else
                    acl_key = permission_prefix ~ object_group.id;

                if (trace_msg[ 112 ] == 1)
                    log.trace("look acl_key: [%s]", acl_key);

                RightSet permission;

                if (cache_of_permission !is null)
                {
                    permission = cache_of_permission.get(acl_key);

                    if (permission !is null)
                    {
                        if (trace_msg[ 112 ] == 1)
                            log.trace("for [%s] found in cache %s", acl_key, permission);
                        permission_2_group[ object_group.id ] = permission;
                    }
                }

                if (permission is null)
                {
                    key.mv_size = acl_key.length;
                    key.mv_data = cast(char *)acl_key;

                    rc = mdb_get(txn_r, dbi, &key, &data);
                    if (rc == 0)
                    {
                        str = cast(string)(data.mv_data[ 0..data.mv_size ]);
                        RightSet pp = new RightSet(log);
                        rights_from_string(str, pp);
                        permission_2_group[ object_group.id ] = pp;

                        if (cache_of_permission !is null)
                            cache_of_permission.put(acl_key, pp);

                        if (trace_msg[ 112 ] == 1)
                            log.trace("for [%s] found %s", acl_key, pp);
                    }
                }
            }

            mdb_txn_abort(txn_r);

            foreach (obj_key; object_groups.data.keys)
            {
                RightSet permissions = permission_2_group.get(obj_key, null);
                //log.trace("obj_key=%s, permissions=%s", obj_key, permissions);
                if (permissions !is null)
                {
                    foreach (perm_key; permissions.data.keys)
                    {
                        if (perm_key in subject_groups.data)
                        {
                            Right *restriction = object_groups.data.get(obj_key, null);
                            Right *permission  = permissions.data.get(perm_key, null);

                            //log.trace("restriction=%s, permission=%s, request=%s", *restriction, *permission, access_to_pretty_string(request_access));

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
//                            if (trace !is null)
//                                trace(buff_object_group[ pos ], buff_subject_group[ pos ], access_list_predicates[ idx ]);

                                        res = cast(ubyte)(res | set_bit);
                                        //log.trace("set_bit=%s, res=%s", access_to_pretty_string(set_bit), access_to_pretty_string(res));

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
            }
        }catch (Exception ex)
        {
            writeln("EX!,", ex.msg);
        }

        scope (exit)
        {
            if (trace_msg[ 111 ] == 1)
                log.trace("authorize %s, request=%s, answer=[%s]", uri, access_to_pretty_string(request_access), access_to_pretty_string(res));
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
}

