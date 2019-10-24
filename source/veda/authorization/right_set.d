module veda.authorization.right_set;

private import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.array, std.outbuffer, std.string;
private import veda.common.type, veda.core.common.define, veda.core.common.type, veda.common.logger;

public static string membership_prefix = "M";
public static string permission_prefix = "P";
public static string filter_prefix     = "F";

const                M_IS_EXCLUSIVE     = 'X';
const                M_IGNORE_EXCLUSIVE = 'N';

struct Right
{
    string id;
    ubyte  access;
    char   marker;
    bool   is_deleted = false;

    void   toString(scope void delegate(const(char)[]) sink) const
    {
        string aaa = access_to_pretty_string1(access);

        sink("(" ~ text(id) ~ ", " ~ aaa ~ "," ~ marker ~ ")");
//        sink("(" ~ text(id) ~ ", " ~ to!string(access, 16) ~ ", " ~ text(is_deleted) ~ ")");
//        sink("(A:" ~ to!string(access, 16) ~ ", D:" ~ text(is_deleted) ~ ")");
    }
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
            if (el.id in data)
            {
                ubyte prev_access = data[ el.id ].access;
                ubyte new_access  = el.access | prev_access;
                el.access = new_access;
            }

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
                string s_access = tokens[ idx + 1 ][ 0..2 ];
                ubyte  access   = parse!ubyte (s_access, 16);
                char   marker   = 0;
                if (tokens[ idx + 1 ].length > 1)
                    marker = tokens[ idx + 1 ][ 1 ];

                new_rights.data[ key ] = new Right(key, access, marker, false);
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
                string s_access = tokens[ idx + 1 ][ 0..2 ];
                char   marker   = 0;
                if (tokens[ idx + 1 ].length > 1)
                    marker = tokens[ idx + 1 ][ 1 ];

                ubyte access = parse!ubyte (s_access, 16);
                rights_list ~= new Right(key, access, marker, false);
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

                if (right.marker > 0)
                    outbuff.write(right.marker);
                outbuff.write(';');
            }
        }
    }
    return outbuff.toString();
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

string access_to_pretty_string1(const ubyte src)
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


