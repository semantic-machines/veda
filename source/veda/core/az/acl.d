/**
   авторизация
 */

module veda.core.az.acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.type, veda.onto.individual, veda.onto.resource, veda.core.bind.lmdb_header, veda.core.context, veda.core.define,
           veda.core.know_predicates, veda.core.log_msg, veda.core.util.cbor8individual;
    import util.utils, util.cbor, util.logger;
    import veda.core.storage.lmdb_storage, veda.core.thread_context, veda.core.az.right_set;
}

// ////////////// ACLManager
protected byte err;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "ACL");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

public string backup(Context ctx, string backup_id)
{
    string res;

    Tid    tid_acl_manager = ctx.getTid(P_MODULE.acl_manager);

    send(tid_acl_manager, CMD.BACKUP, backup_id, thisTid);
    receive((string _res) { res = _res; });

    return res;
}

/// Хранение, чтение PermissionStatement, Membership
class Authorization : LmdbStorage
{
    this(string _path, DBMode mode, string _parent_thread_name)
    {
        super(_path, mode, _parent_thread_name);
    }

    int count_permissions = 0;

    override void reopen_db()
    {
        super.reopen_db();
    }

    import util.container;

    Cache!(RightSet, string) cache;

    ubyte authorize(string uri, Ticket *ticket, ubyte request_access, Context context, void delegate(string resource_group,
                                                                                                     string subject_group,
                                                                                                     string right) trace = null)
    {
        //if (cache is null)
        //cache = new Cache!(RightSet, string)(1);

        void reopen_db()
        {
            this.reopen_db();
        }

        ubyte res = 0;

        if (ticket is null)
            return request_access;

        if (trace_msg[ 111 ] == 1)
            log.trace("authorize uri=%s, user=%s, request_access=%s", uri, ticket.user_uri, access_to_pretty_string(request_access));

        MDB_txn *txn_r;
        MDB_dbi dbi;
        string  str;
        int     rc;

        context.acl_check_for_reload(&reopen_db);

        if (db_is_open.get(path, false) == false)
            return res;

        rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
        if (rc == MDB_BAD_RSLOT)
        {
            log.trace_log_and_console("warn: find 1:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", path);
            for (int i = 0; i < 10 && rc != 0; i++)
            {
                mdb_txn_abort(txn_r);

                if (i > 3)
                {
                    log.trace_log_and_console("warn: find 1:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", path);
                    core.thread.Thread.sleep(dur!("msecs")(10));
                }

                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
            }
            //subject_groups_cache[ ticket.user_uri ] = string[].init;
        }

        if (rc != 0)
        {
            if (rc == MDB_MAP_RESIZED)
            {
                log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) WARN:%s", path, fromStringz(mdb_strerror(rc)));
                reopen_db();
                rc = mdb_txn_begin(env, null, MDB_RDONLY, &txn_r);
                //subject_groups_cache[ ticket.user_uri ] = string[].init;
            }
            else if (rc == MDB_BAD_RSLOT)
            {
                log.trace_log_and_console("warn 2: find:" ~ text(__LINE__) ~ "(%s) MDB_BAD_RSLOT", path);
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
            log.trace_log_and_console(__FUNCTION__ ~ ":" ~ text(__LINE__) ~ "(%s) ERR:%s", path, fromStringz(mdb_strerror(rc)));
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
                    //if (cache !is null)
                    //	res = cache.get (uri);

                    if (res is null)
                    {
                        key.mv_size = uri.length;
                        key.mv_data = cast(char *)uri;
                        rc          = mdb_get(txn_r, dbi, &key, &data);
                        if (rc == 0)
                        {
                            groups_str = cast(string)(data.mv_data[ 0..data.mv_size ]);
                            rights_from_string(groups_str, res);
                            //if (cache !is null)
                            //	cache.put (uri, res);
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
                return new RightSet(_get_resource_groups(uri, access, prepared_uris, 0));
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
                string acl_key = permission_prefix ~ object_group.id;

                if (trace_msg[ 112 ] == 1)
                    log.trace("look acl_key: [%s]", acl_key);

                key.mv_size = acl_key.length;
                key.mv_data = cast(char *)acl_key;

                rc = mdb_get(txn_r, dbi, &key, &data);
                if (rc == 0)
                {
                    str = cast(string)(data.mv_data[ 0..data.mv_size ]);
                    RightSet pp = new RightSet();
                    rights_from_string(str, pp);
                    permission_2_group[ object_group.id ] = pp;

                    if (trace_msg[ 112 ] == 1)
                        log.trace("for [%s] found %s", acl_key, text(getAccessListFromByte(cast(ubyte)str[ 0 ])));
                }
            }

            mdb_txn_abort(txn_r);

            foreach (obj_key; object_groups.data.keys)
            {
                RightSet permissions = permission_2_group.get(obj_key, null);
                if (permissions !is null)
                {
                    foreach (perm_key; permissions.data.keys)
                    {
                        if (perm_key in subject_groups.data)
                        {
                            Right *restriction = object_groups.data.get(obj_key, null);
                            Right *permission  = permissions.data.get(perm_key, null);

                            //log.trace("restriction=%s, permission=%s, request=%s", restriction, permission, access_to_pretty_string (request_access));

                            ubyte restriction_access, permission_access;

                            if (restriction !is null)
                                restriction_access = restriction.access;

                            if (permission !is null)
                                permission_access = permission.access;

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
                log.trace("authorize %s, request=%s, answer=%s", uri, text(request_access), text(res));
        }

        if (trace_msg[ 111 ] == 1)
            log.trace("acl:res=%d", res);

        return res;
    }
}

void acl_manager(string thread_name, string db_path)
{
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;

    core.thread.Thread.getThis().name         = thread_name;
    Authorization                storage      = new Authorization(acl_indexes_db_path, DBMode.RW, "acl_manager");
    string                       bin_log_name = get_new_binlog_name(db_path);

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });
    while (true)
    {
        try
        {
            receive(
                    (CMD cmd)
                    {
                        if (cmd == CMD.COMMIT)
                        {
                            storage.flush(1);
                        }
                    },
                    (CMD cmd, EVENT type, string msg, long op_id)
                    {
                        if (cmd == CMD.PUT)
                        {
                            try
                            {
                                Individual ind;
                                if (cbor2individual(&ind, msg) < 0)
                                {
                                    log.trace("!ERR:invalid individual: [%s] op_id=%d", msg, op_id);
                                    return;
                                }

                                Resources rdfType = ind.resources[ rdf__type ];

                                if (rdfType.anyExist(veda_schema__PermissionStatement) == true)
                                {
                                    prepare_permission_statement(ind, op_id, storage);
                                }
                                else if (rdfType.anyExist(veda_schema__Membership) == true)
                                {
                                    prepare_membership(ind, op_id, storage);
                                }
                                else if (rdfType.anyExist(veda_schema__PermissionFilter) == true)
                                {
                                    prepare_permission_filter(ind, op_id, storage);
                                }
                            }
                            finally
                            {
                                set_acl_manager_op_id(op_id);
                            }
                        }
                    },
                    (CMD cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.NOP)
                            send(tid_response_reciever, true);
                        else
                            send(tid_response_reciever, false);
                    },
                    (CMD cmd, string msg, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.BACKUP)
                        {
                            try
                            {
                                string backup_id;
                                if (msg.length > 0)
                                    backup_id = msg;

                                if (backup_id is null)
                                    backup_id = "0";

                                Result res = storage.backup(backup_id);
                                if (res == Result.Ok)
                                {
                                    size_bin_log = 0;
                                    bin_log_name = get_new_binlog_name(db_path);
                                }
                                else if (res == Result.Err)
                                {
                                    backup_id = "";
                                }
                                send(tid_response_reciever, backup_id);
                            }
                            catch (Exception ex)
                            {
                                send(tid_response_reciever, "");
                            }
                        }
                        else
                        {
                            send(tid_response_reciever, "?");
                        }
                    },
                    (CMD cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD.SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (Variant v) { writeln(thread_name, "::acl_manager::Received some other type: [", v, "]"); });
        }
        catch (Throwable ex)
        {
            log.trace("acl manager# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}
