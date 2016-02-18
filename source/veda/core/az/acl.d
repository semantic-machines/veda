/**
   авторизация
 */

module az.acl;

private
{
    import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
    import veda.type, veda.onto.individual, veda.onto.resource, veda.core.bind.lmdb_header, veda.core.context, veda.core.define,
           veda.core.know_predicates, veda.core.log_msg;
    import util.logger, util.utils, util.cbor, veda.core.util.cbor8individual, util.logger;
    import veda.core.storage.lmdb_storage, veda.core.thread_context;
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

struct Right
{
    string id;
    ubyte  access;
    bool   is_deleted = false;

    void   toString(scope void delegate(const(char)[]) sink) const
    {
        string aaa = access_to_pretty_string(access);

        sink("(" ~ text(id) ~ ", " ~ aaa ~ ")");
//        sink("(" ~ text(id) ~ ", " ~ to!string(access, 16) ~ ", " ~ text(is_deleted) ~ ")");
//        sink("(A:" ~ to!string(access, 16) ~ ", D:" ~ text(is_deleted) ~ ")");
    }
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

    return res;
}

class RightSet
{
    Right[ string ] data;
}

public static string membership_prefix = "M";
public static string permission_prefix = "P";

/// Хранение, чтение PermissionStatement, Membership
class Authorization : LmdbStorage
{
    this(string _path, DBMode mode, string _parent_thread_name)
    {
        super(_path, mode, _parent_thread_name);
    }

    private bool rights_from_string(string src, RightSet new_rights)
    {
        //writeln ("@rights_from_string=", src);
        string[] tokens = src.split(";");
        if (tokens.length > 2)
        {
            for (long idx = 0; idx < tokens.length; idx += 2)
            {
                string key = tokens[ idx ];
                if (key !is null && key.length > 0)
                {
                    ubyte access = parse!ubyte (tokens[ idx + 1 ], 16);
                    new_rights.data[ key ] = Right(key, access, false);
                }
            }
            return true;
        }
        return false;
    }


    private string rights_as_string(RightSet new_rights)
    {
        OutBuffer outbuff = new OutBuffer();

        foreach (key; new_rights.data.keys)
        {
            Right right = new_rights.data.get(key, Right.init);
            if (right !is Right.init)
            {
                if (right.is_deleted == false)
                {
                    outbuff.write(right.id);
                    outbuff.write(';');
                    outbuff.write(to!string(right.access, 16));
                    outbuff.write(';');
                }
            }
        }
        return outbuff.toString();
    }

    void prepare_right_set(ref Individual ind, string p_resource, string p_in_set, string prefix, ubyte default_access)
    {
        bool     is_deleted = ind.isExist("v-s:deleted", true);

        ubyte    access;
        //writeln ("#1 access=", to!string(access, 16));
        Resource canCreate = ind.getFirstResource("v-s:canCreate");

        if (canCreate !is Resource.init)
        {
            if (canCreate == true)
                access = access | Access.can_create;
            else
                access = access | Access.cant_create;
        }

        Resource canRead = ind.getFirstResource("v-s:canRead");
        if (canRead !is Resource.init)
        {
            if (canRead == true)
                access = access | Access.can_read;
            else
                access = access | Access.cant_read;
        }

        Resource canUpdate = ind.getFirstResource("v-s:canUpdate");
        if (canUpdate !is Resource.init)
        {
            if (canUpdate == true)
                access = access | Access.can_update;
            else
                access = access | Access.cant_update;
        }

        Resource canDelete = ind.getFirstResource("v-s:canDelete");
        if (canDelete !is Resource.init)
        {
            if (canDelete == true)
                access = access | Access.can_delete;
            else
                access = access | Access.cant_delete;
        }

        if (access == 0)
            access = default_access;
        //writeln ("#2 access=", to!string(access, 16));


        Resources resource = ind.getResources(p_resource);
        Resources in_set   = ind.getResources(p_in_set);

        // для каждого из ресурсов выполним операцию добавления/удаления
        foreach (rs; resource)
        {
            RightSet new_right_set = new RightSet();

            string   prev_data_str = this.find(prefix ~ rs.uri);
            //writeln ("@ key=", prefix ~ rs.uri);
            if (prev_data_str !is null)
                rights_from_string(prev_data_str, new_right_set);

//      writeln ("@3 prev_right_set=", new_right_set);
//      writeln ("@3 in_set=", in_set);

            foreach (mb; in_set)
            {
                Right rr = new_right_set.data.get(mb.uri, Right.init);

                if (rr !is Right.init)
                {
                    rr.is_deleted                = is_deleted;
                    rr.access                    = access;
                    new_right_set.data[ mb.uri ] = rr;
                    //writeln ("@3.1 rr=", rr);
                }
                else
                {
                    Right nrr = Right(mb.uri, access, false);
//      writeln ("@3.2 nrr=", nrr);
                    new_right_set.data[ mb.uri ] = nrr;
                }
            }

            string new_record = rights_as_string(new_right_set);
            //log.trace ("@ new_record= [%s][%s]", prefix ~ rs.uri, new_right_set);

            if (new_record.length == 0)
                new_record = "X";
            ResultCode res = this.put(prefix ~ rs.uri, new_record);

            if (trace_msg[ 101 ] == 1)
                log.trace("[acl index] (%s) new right set: %s : [%s]", text(res), rs.uri, new_record);
        }
    }

    void prepare_membership(ref Individual ind, long op_id)
    {
        if (trace_msg[ 114 ] == 1)
            log.trace("store Membership: [%s] op_id=%d", ind, op_id);

        prepare_right_set(ind, veda_schema__resource, veda_schema__memberOf, membership_prefix,
                          Access.can_create | Access.can_read | Access.can_update | Access.can_delete);
    }

    void prepare_permission_filter(ref Individual ind, long op_id)
    {
        if (trace_msg[ 114 ] == 1)
            log.trace("store PermissionFilter: [%s] op_id=%d", ind, op_id);

        Resource   permissionObject = ind.getFirstResource(veda_schema__permissionObject);

        ResultCode res = this.put("F+" ~ permissionObject.uri, ind.uri);

        if (trace_msg[ 101 ] == 1)
            log.trace("[acl index] (%s) PermissionFilter: %s : %s", text(res), permissionObject.uri, ind.uri);
    }

    void prepare_permission_statement(ref Individual ind, long op_id)
    {
        if (trace_msg[ 114 ] == 1)
            log.trace("store PermissionStatement: [%s] op_id=%d", ind, op_id);

        prepare_right_set(ind, veda_schema__permissionObject, veda_schema__permissionSubject, permission_prefix, 0);
    }

//    string[][ string ] subject_groups_cache;
//    ubyte[ 1024 ] buff_permissions;
//    string[ 1024 ] buff_subject_group;
//    string[ 1024 ] buff_object_group;

    int count_permissions = 0;

    override void reopen_db()
    {
        //log.trace("@1 ACL:reopen_db");
        super.reopen_db();
        //subject_groups_cache = string[][ string ].init;
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
            //log.trace("@2 ACL:reopen_db");
            this.reopen_db();
            //subject_groups_cache[ ticket.user_uri ] = string[].init;
        }

        ubyte res = 0;

        if (ticket is null)
            return request_access;

        //log.trace("authorize uri=%s, user=%s, request_access=%s", uri, ticket.user_uri, access_to_pretty_string (request_access));

//        if (trace_msg[ 111 ] == 1)
//            log.trace("authorize %s", uri);

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
                //subject_groups_cache[ ticket.user_uri ] = string[].init;
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
            RightSet object_groups  = new RightSet();
            RightSet subject_groups = new RightSet();

            MDB_val  key;
            MDB_val  data;
            string   skey;

            RightSet get_resource_groups(string uri, ubyte access, int level = 0)
            {
                //log.trace ("%d[%s]@1 uri=%s, access=%s", level, tttt[0..level*3], uri, access_to_pretty_string (access));
                RightSet res;

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
                            res        = new RightSet();
                            rights_from_string(groups_str, res);
                            //if (cache !is null)
                            //	cache.put (uri, res);
                        }
                    }

                    if (res is null)
                        res = new RightSet();

                    foreach (group; res.data)
                    {
                        string group_key = membership_prefix ~ group.id;
                        group.access         = group.access & access;
                        res.data[ group.id ] = group;

                        if (uri == group_key)
                            continue;

                        RightSet up_restrictions = get_resource_groups(group_key, group.access & access, level + 1);

                        //log.trace ("%d[%s]@3 up_restrictions=%s", level, tttt[0..level*3], up_restrictions.values);
                        if (up_restrictions !is null)
                            foreach (restriction; up_restrictions.data)
                            {
//                            restriction.access    = restriction.access & access;
                                //up_restrictions[ restriction.id ] = restriction;
                                res.data[ restriction.id ] = restriction;
                            }
                    }
                }
                catch (Throwable ex)
                {
                    log.trace("ERR! (%d) LINE:[%s], FILE:[%s], MSG:[%s]", level, ex.line, ex.file, ex.info);
                }
                return res;
            }

            // 1. читаем группы object (uri)
            object_groups                                        = get_resource_groups(membership_prefix ~ uri, 15);
            object_groups.data[ uri ]                            = Right(uri, 15, false);
            object_groups.data[ veda_schema__AllResourcesGroup ] = Right(veda_schema__AllResourcesGroup, 15, false);

            // 2. читаем группы subject (ticket.user_uri)
            subject_groups                         = get_resource_groups(membership_prefix ~ ticket.user_uri, 15);
            subject_groups.data[ ticket.user_uri ] = Right(ticket.user_uri, 15, false);

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
                    // writeln ("@ premisson key=", acl_key);
                    RightSet pp = new RightSet();
                    rights_from_string(str, pp);
                    permission_2_group[ object_group.id ] = pp;

                    if (trace_msg[ 112 ] == 1)
                        log.trace("for [%s] found %s", acl_key, text(getAccessListFromByte(cast(ubyte)str[ 0 ])));
                }
            }

            mdb_txn_abort(txn_r);

            //log.trace("permission_2_group=%s", permission_2_group.values);

            foreach (obj_key; object_groups.data.keys)
            {
                RightSet permissions = permission_2_group.get(obj_key, null);
                if (permissions !is null)
                {
                    foreach (perm_key; permissions.data.keys)
                    {
                        if (perm_key in subject_groups.data)
                        {
                            Right restriction = object_groups.data.get(obj_key, Right.init);
                            Right permission  = permissions.data.get(perm_key, Right.init);

                            //log.trace("restriction=%s, permission=%s, request=%s", restriction, permission, access_to_pretty_string (request_access));

                            foreach (int idx, access; access_list)
                            {
                                if ((request_access & access & restriction.access) != 0)
                                {
                                    ubyte set_bit = cast(ubyte)(access & permission.access);

                                    if (set_bit > 0)
                                    {
//                            if (trace !is null)
//                                trace(buff_object_group[ pos ], buff_subject_group[ pos ], access_list_predicates[ idx ]);

                                        res = cast(ubyte)(res | set_bit);

                                        //if (res == request_access && trace is null)
                                        //    break;
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

private Access[] getAccessListFromByte(ubyte permission)
{
    Access[] res;

    foreach (int idx, access; access_list)
    {
        ubyte set_bit = cast(ubyte)(access & permission);

        if (set_bit > 0)
            res ~= access;
    }
    foreach (int idx, access; denied_list)
    {
        ubyte set_bit = cast(ubyte)(access & permission);

        if (set_bit > 0)
            res ~= access;
    }

    return res;
}

void acl_manager(string thread_name, string db_path)
{
    int                          size_bin_log     = 0;
    int                          max_size_bin_log = 10_000_000;

    core.thread.Thread.getThis().name = thread_name;
//    writeln("SPAWN: acl manager");
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
                                    storage.prepare_permission_statement(ind, op_id);
                                }
                                else if (rdfType.anyExist(veda_schema__Membership) == true)
                                {
                                    storage.prepare_membership(ind, op_id);
                                }
                                else if (rdfType.anyExist(veda_schema__PermissionFilter) == true)
                                {
                                    storage.prepare_permission_filter(ind, op_id);
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
