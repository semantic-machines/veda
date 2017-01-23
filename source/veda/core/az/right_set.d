module veda.core.az.right_set;

private import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
private import veda.common.type, veda.onto.individual, veda.onto.resource, veda.core.common.context, veda.core.common.log_msg,
               veda.core.common.know_predicates;
private import veda.core.util.utils, veda.common.logger;

public static string membership_prefix = "M";
public static string permission_prefix = "P";
public static string filter_prefix     = "F";

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

class RightSet
{
    Right *[ string ] data;
    Logger log;

    this(Logger _log)
    {
        log = _log;
    }

    this(Right *[] src, Logger _log)
    {
        log = _log;
        foreach (el; src)
        {
            data[ el.id ] = el;
        }
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        foreach (key, value; data)
        {
            sink(text(*value) ~ ", ");
        }
    }
}

public bool rights_from_string(string src, RightSet new_rights)
{
    string[] tokens = src.split(";");
    if (tokens.length > 2)
    {
        for (long idx = 0; idx < tokens.length; idx += 2)
        {
            string key = tokens[ idx ].dup;
            if (key !is null && key.length > 0)
            {
                ubyte access = parse!ubyte (tokens[ idx + 1 ], 16);
                new_rights.data[ key ] = new Right(key, access, false);
            }
        }
        return true;
    }
    return false;
}

public bool rights_from_string(string src, ref Right *[] rights_list)
{
    string[] tokens = src.split(";");
    if (tokens.length > 2)
    {
        for (long idx = 0; idx < tokens.length; idx += 2)
        {
            string key = tokens[ idx ].dup;
            if (key !is null && key.length > 0)
            {
                ubyte access = parse!ubyte (tokens[ idx + 1 ], 16);
                rights_list ~= new Right(key, access, false);
            }
        }
        return true;
    }
    return false;
}


public string rights_as_string(RightSet new_rights)
{
    OutBuffer outbuff = new OutBuffer();

    foreach (key; new_rights.data.keys)
    {
        Right *right = new_rights.data.get(key, null);
        if (right !is null)
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

void prepare_right_set(ref Individual prev_ind, ref Individual new_ind, string p_resource, string p_in_set, string prefix, ubyte default_access,
                       long op_id,
                       Storage storage)
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
                              Storage storage)
{
    // для каждого из ресурсов выполним операцию добавления/удаления
    foreach (rs; resource)
    {
        RightSet new_right_set = new RightSet(log);

        string   prev_data_str = storage.find(prefix ~ rs.uri);
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

        ResultCode res = storage.put(key, new_record, op_id);

        if (trace_msg[ 101 ] == 1)
            log.trace("[acl index] (%s) new right set: %s : [%s]", text(res), rs.uri, new_record);
    }
}

void prepare_membership(ref Individual prev_ind, ref Individual new_ind, long op_id, Storage storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store Membership: [%s] op_id=%d", new_ind.uri, op_id);

    prepare_right_set(prev_ind, new_ind, veda_schema__resource, veda_schema__memberOf, membership_prefix,
                      Access.can_create | Access.can_read | Access.can_update | Access.can_delete, op_id, storage);
}

void prepare_permission_filter(ref Individual prev_ind, ref Individual new_ind, long op_id, Storage storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store PermissionFilter: [%s] op_id=%d", new_ind, op_id);

    Resource   permissionObject = new_ind.getFirstResource(veda_schema__permissionObject);

    ResultCode res = storage.put(filter_prefix ~ permissionObject.uri, new_ind.uri, op_id);

    if (trace_msg[ 101 ] == 1)
        log.trace("[acl index] (%s) PermissionFilter: %s : %s", text(res), permissionObject.uri, new_ind.uri);
}

void prepare_permission_statement(ref Individual prev_ind, ref Individual new_ind, long op_id, Storage storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store PermissionStatement: [%s] op_id=%d", new_ind, op_id);

    prepare_right_set(prev_ind, new_ind, veda_schema__permissionObject, veda_schema__permissionSubject, permission_prefix, 0, op_id, storage);
}

Access[] getAccessListFromByte(ubyte permission)
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
