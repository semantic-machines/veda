module veda.core.az.right_set;

private import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
private import veda.type, veda.onto.individual, veda.onto.resource, veda.core.context, veda.core.log_msg, veda.core.know_predicates;
private import util.utils, veda.util.cbor, util.logger;

public static string membership_prefix = "M";
public static string permission_prefix = "P";


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
    Right *[ string ] data;

    this()
    {
    }

    this(Right *[] src)
    {
        foreach (el; src)
        {
            data[ el.id ] = el;
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
            string key = tokens[ idx ];
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
            string key = tokens[ idx ];
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

void prepare_right_set(ref Individual ind, string p_resource, string p_in_set, string prefix, ubyte default_access, Storage storage)
{
    bool     is_deleted = ind.isExist("v-s:deleted", true);

    ubyte    access;
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

    Resources resource = ind.getResources(p_resource);
    Resources in_set   = ind.getResources(p_in_set);

    // для каждого из ресурсов выполним операцию добавления/удаления
    foreach (rs; resource)
    {
        RightSet new_right_set = new RightSet();

        string   prev_data_str = storage.find(prefix ~ rs.uri);
        if (prev_data_str !is null)
            rights_from_string(prev_data_str, new_right_set);

        foreach (mb; in_set)
        {
            Right *rr = new_right_set.data.get(mb.uri, null);

            if (rr !is null)
            {
                rr.is_deleted                = is_deleted;
                rr.access                    = access;
                new_right_set.data[ mb.uri ] = rr;
                //writeln ("@3.1 rr=", rr);
            }
            else
            {
                Right *nrr = new Right(mb.uri, access, false);
                new_right_set.data[ mb.uri ] = nrr;
            }
        }

        string new_record = rights_as_string(new_right_set);

        if (new_record.length == 0)
            new_record = "X";
        ResultCode res = storage.put(prefix ~ rs.uri, new_record);

        if (trace_msg[ 101 ] == 1)
            log.trace("[acl index] (%s) new right set: %s : [%s]", text(res), rs.uri, new_record);
    }
}

void prepare_membership(ref Individual ind, long op_id, Storage storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store Membership: [%s] op_id=%d", ind, op_id);

    prepare_right_set(ind, veda_schema__resource, veda_schema__memberOf, membership_prefix,
                      Access.can_create | Access.can_read | Access.can_update | Access.can_delete, storage);
}

void prepare_permission_filter(ref Individual ind, long op_id, Storage storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store PermissionFilter: [%s] op_id=%d", ind, op_id);

    Resource   permissionObject = ind.getFirstResource(veda_schema__permissionObject);

    ResultCode res = storage.put("F+" ~ permissionObject.uri, ind.uri);

    if (trace_msg[ 101 ] == 1)
        log.trace("[acl index] (%s) PermissionFilter: %s : %s", text(res), permissionObject.uri, ind.uri);
}

void prepare_permission_statement(ref Individual ind, long op_id, Storage storage)
{
    if (trace_msg[ 114 ] == 1)
        log.trace("store PermissionStatement: [%s] op_id=%d", ind, op_id);

    prepare_right_set(ind, veda_schema__permissionObject, veda_schema__permissionSubject, permission_prefix, 0, storage);
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
