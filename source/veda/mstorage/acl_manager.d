/**
   авторизация
 */

module veda.mstorage.acl_manager;

import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
import veda.util.properd;
import veda.common.type, veda.onto.individual, veda.onto.resource, veda.core.common.context, veda.core.common.define,
       veda.core.common.know_predicates;
import veda.core.common.log_msg, veda.storage.common, veda.core.util.utils, veda.common.logger, veda.util.module_info, veda.core.impl.thread_context;
import veda.storage.common, veda.storage.right_set;
import veda.storage.lmdb.lmdb_acl, veda.storage.lmdb.lmdb_driver;
import veda.storage.tarantool.tarantool_driver;

// ////////////// ACLManager
protected byte err;

// ////// Logger ///////////////////////////////////////////
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-mstorage", "log", "ACL-MANAGER");
    return _log;
}
// ////// ////// ///////////////////////////////////////////
enum CMD : byte
{
    /// Сохранить
    PUT       = 1,

    /// Коммит
    COMMIT    = 16,

    /// Включить/выключить отладочные сообщения
    SET_TRACE = 33,

    /// Пустая комманда
    NOP       = 64,

    EXIT      = 49
}

public ResultCode flush(bool is_wait)
{
    ResultCode rc;
    Tid        tid = getTid(P_MODULE.acl_preparer);

    if (tid != Tid.init)
    {
        if (is_wait == false)
        {
            send(tid, CMD_COMMIT);
        }
        else
        {
            send(tid, CMD_COMMIT, thisTid);
            receive((bool isReady) {});
        }
        rc = ResultCode.OK;
    }
    return rc;
}


void acl_manager(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;

    KeyValueDB                   storage;

    string[ string ] properties;
    properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    if (tarantool_url !is null)
    {
        storage = new TarantoolDriver(log, "acl-indexes", 513);
    }
    else
    {
        storage = new LmdbDriver(acl_indexes_db_path, DBMode.RW, "acl_manager", log);
    }


    long l_op_id;
    long committed_op_id;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    ModuleInfoFile module_info = new ModuleInfoFile(thread_name, _log, OPEN_MODE.WRITER);
    if (!module_info.is_ready)
    {
        log.trace("thread [%s] terminated", process_name);
        return;
    }

    bool is_exit = false;

    while (is_exit == false)
    {
        try
        {
            receive(
                    (byte cmd)
                    {
                        if (cmd == CMD_COMMIT)
                        {
                            if (committed_op_id != l_op_id)
                            {
                                storage.flush(1);
                                //log.trace("acl commit op_id=%d", l_op_id);
                                committed_op_id = l_op_id;
                                module_info.put_info(l_op_id, committed_op_id);
                            }
                        }
                    },
                    (byte cmd, string prev_state, string new_state, long op_id)
                    {
                        if (cmd == CMD_PUT)
                        {
                            try
                            {
                                Individual new_ind;
                                if (new_ind.deserialize(new_state) < 0)
                                {
                                    log.trace("ERR! invalid individual: [%s] op_id=%d", new_state, op_id);
                                    return;
                                }

                                Individual prev_ind;
                                if (prev_state !is null && prev_ind.deserialize(prev_state) < 0)
                                {
                                    log.trace("ERR! invalid individual: [%s] op_id=%d", prev_state, op_id);
                                    return;
                                }

                                Resources rdfType = new_ind.resources[ rdf__type ];

                                if (rdfType.anyExists(veda_schema__PermissionStatement) == true)
                                {
                                    prepare_permission_statement(prev_ind, new_ind, op_id, storage);
                                }
                                else if (rdfType.anyExists(veda_schema__Membership) == true)
                                {
                                    prepare_membership(prev_ind, new_ind, op_id, storage);
                                }
                                else if (rdfType.anyExists(veda_schema__PermissionFilter) == true)
                                {
                                    prepare_permission_filter(prev_ind, new_ind, op_id, storage);
                                }
                            }
                            finally
                            {
                                l_op_id = op_id;
                                
                                if (tarantool_url !is null)
	                                committed_op_id = l_op_id;
                                
                                module_info.put_info(l_op_id, committed_op_id);
                            }
                        }
                    },
                    (byte cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD_EXIT)
                        {
                            is_exit = true;
                            writefln("[%s] recieve signal EXIT", "acl_manager");
                            send(tid_response_reciever, true);
                        }
                        else if (cmd == CMD_NOP)
                            send(tid_response_reciever, true);
                        else
                            send(tid_response_reciever, false);
                    },
                    (byte cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD_SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (OwnerTerminated ot)
                    {
                        //log.trace("%s::acl_manager::OWNER TERMINATED", thread_name);
                        return;
                    },
                    (Variant v) { writeln(thread_name, "::acl_manager::Received some other type: [", v, "]"); });
        }
        catch (Throwable ex)
        {
            log.trace("acl manager# ERR! MSG:[%s] [%s]", ex.msg, ex.info);
        }
    }

    module_info.close();
}

void prepare_right_set(ref Individual prev_ind, ref Individual new_ind, string p_resource, string p_in_set, string prefix, ubyte default_access,
                       long op_id,
                       KeyValueDB storage)
{
    bool     is_deleted = new_ind.isExists("v-s:deleted", true);

    ubyte    access;
    Resource canCreate = new_ind.getFirstResource("v-s:canCreate");

    if (canCreate !is Resource.init)
    {
        if (canCreate == true)
            access = access | Access.can_create;
        else
            access = access | Access.cant_create;
    }

    Resource canRead = new_ind.getFirstResource("v-s:canRead");
    if (canRead !is Resource.init)
    {
        if (canRead == true)
            access = access | Access.can_read;
        else
            access = access | Access.cant_read;
    }

    Resource canUpdate = new_ind.getFirstResource("v-s:canUpdate");
    if (canUpdate !is Resource.init)
    {
        if (canUpdate == true)
            access = access | Access.can_update;
        else
            access = access | Access.cant_update;
    }

    Resource canDelete = new_ind.getFirstResource("v-s:canDelete");
    if (canDelete !is Resource.init)
    {
        if (canDelete == true)
            access = access | Access.can_delete;
        else
            access = access | Access.cant_delete;
    }

    if (access == 0)
        access = default_access;

    Resource  useFilter = new_ind.getFirstResource(veda_schema__useFilter);

    Resources resource = new_ind.getResources(p_resource);
    Resources in_set   = new_ind.getResources(p_in_set);

    Resources prev_resource = prev_ind.getResources(p_resource);
    Resources prev_in_set   = prev_ind.getResources(p_in_set);

    Resources delta_resource = get_disappeared(prev_resource, resource);
    Resources delta_in_set   = get_disappeared(prev_in_set, in_set);

    if (delta_resource.length > 0)
    {
        //	    log.trace ("- delta_resource=%s", delta_resource);
        //	    log.trace ("- delta_in_set=%s", delta_in_set);

        update_right_set(resource, in_set, is_deleted, useFilter, prefix, access, op_id, storage);
        update_right_set(delta_resource, in_set, true, useFilter, prefix, access, op_id, storage);
    }
    else
    {
        delta_resource = get_disappeared(resource, prev_resource);
        delta_in_set   = get_disappeared(in_set, prev_in_set);

        //	    log.trace ("+ delta_resource=%s", delta_resource);
        //	    log.trace ("+ delta_in_set=%s", delta_in_set);

        update_right_set(resource, in_set, is_deleted, useFilter, prefix, access, op_id, storage);
        //update_right_set(delta_resource, delta_in_set, false, useFilter, prefix, access, op_id, storage);
    }

/*
    update_right_set(resource, in_set, is_deleted, useFilter, prefix, access, op_id, storage);
    update_right_set(delta_resource, delta_in_set, true, useFilter, prefix, access, op_id, storage);
 */
}

private void update_right_set(ref Resources resource, ref Resources in_set, bool is_deleted, ref Resource useFilter, string prefix, ubyte access,
                              long op_id,
                              KeyValueDB storage)
{
    // для каждого из ресурсов выполним операцию добавления/удаления
    foreach (rs; resource)
    {
        RightSet new_right_set = new RightSet(log);

        string   prev_data_str = storage.find(OptAuthorize.NO, null, prefix ~ rs.uri);
        if (prev_data_str !is null)
            rights_from_string(prev_data_str, new_right_set);

        foreach (mb; in_set)
        {
            Right *rr = new_right_set.data.get(mb.uri, null);

            if (rr !is null)
            {
                rr.is_deleted                = is_deleted;
                rr.access                    = rr.access | access;
                new_right_set.data[ mb.uri ] = rr;
                //writeln ("@3.1 rr=", rr);
            }
            else
            {
                Right *nrr = new Right(mb.uri, access, is_deleted);
                new_right_set.data[ mb.uri ] = nrr;
            }
        }

        string new_record = rights_as_string(new_right_set);

        if (new_record.length == 0)
            new_record = "X";

        string key;

        if (useFilter !is Resource.init)
            key = prefix ~ useFilter.uri ~ rs.uri;
        else
            key = prefix ~ rs.uri;

        ResultCode res = storage.put(OptAuthorize.NO, null, key, new_record, op_id);

        if (trace_msg[ 101 ] == 1)
            log.trace("[acl index] (%s) new right set: %s : [%s]", text(res), rs.uri, new_record);
    }
}

void prepare_membership(ref Individual prev_ind, ref Individual new_ind, long op_id, KeyValueDB storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store Membership: [%s] op_id=%d", new_ind.uri, op_id);

    prepare_right_set(prev_ind, new_ind, veda_schema__resource, veda_schema__memberOf, membership_prefix,
                      Access.can_create | Access.can_read | Access.can_update | Access.can_delete, op_id, storage);
}

void prepare_permission_filter(ref Individual prev_ind, ref Individual new_ind, long op_id, KeyValueDB storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store PermissionFilter: [%s] op_id=%d", new_ind, op_id);

    Resource   permissionObject = new_ind.getFirstResource(veda_schema__permissionObject);

    ResultCode res = storage.put(OptAuthorize.NO, null, filter_prefix ~ permissionObject.uri, new_ind.uri, op_id);

    if (trace_msg[ 101 ] == 1)
        log.trace("[acl index] (%s) PermissionFilter: %s : %s", text(res), permissionObject.uri, new_ind.uri);
}

void prepare_permission_statement(ref Individual prev_ind, ref Individual new_ind, long op_id, KeyValueDB storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store PermissionStatement: [%s] op_id=%d", new_ind, op_id);

    prepare_right_set(prev_ind, new_ind, veda_schema__permissionObject, veda_schema__permissionSubject, permission_prefix, 0, op_id, storage);
}

