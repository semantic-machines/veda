module veda.authorization.cache;

import std.stdio, std.conv, veda.common.type;

class GroupInfo
{
    //int       level;
    bool is_deprecated;
    GroupInfo[ string ] parents;

    override
    string toString()
    {
        return "" ~ text(parents) ~ "";
    }
}

class CacheElement
{
    ubyte     req_access;
    ubyte     res_access;

    GroupInfo subject_group;
    GroupInfo object_group;

    this(ubyte _req_access, ubyte _res_access)
    {
        req_access = _req_access;
        res_access = _res_access;
    }
}

//const byte EL_IN_TREE   = 1;
//const byte OBJ_IN_TREE  = 1;
//const byte SUBJ_IN_TREE = 2;

class Cache
{
    CacheElement[ string ] ckey_2_cache_element;  // ckey = subject_group_id ~ object_group_id
    GroupInfo[ string ] group_index;
    int[ string ] ckey_2_permissons;              // ckey = subject_group_id ~ object_group_id


    void add_permission(string subject_group_id, string object_group_id, ubyte access)
    {
        string ckey = subject_group_id ~ object_group_id;
        ubyte  ea   = cast(ubyte)ckey_2_permissons.get(ckey, 0);

        if ((ea & access) != access)
            ckey_2_permissons[ ckey ] = ea & access;
    }

    void remove_permission(string subject_group_id, string object_group_id, ubyte prev_access)
    {
        string ckey = subject_group_id ~ object_group_id;
        ubyte  ea   = cast(ubyte)ckey_2_permissons.get(ckey, 0);

        ckey_2_permissons[ ckey ] = (ea ^ prev_access) & ea;
    }

    // добавляет группу id с родителем parent_id в дерево
    bool add_group(string id, string parent_id)
    {
        //stderr.writefln("#0 cache.add_group id=%s, parent_id=%s", id, parent_id);

        GroupInfo gi = group_index.get(id, null);

        if (gi !is null && gi.is_deprecated)
            return false;

        if (gi is null)
        {
            gi                = new GroupInfo();
            group_index[ id ] = gi;
        }

        GroupInfo pgi = group_index.get(parent_id, null);
        if (pgi !is null && pgi.is_deprecated)
            return false;

        if (pgi is null)
        {
            pgi                      = new GroupInfo();
            group_index[ parent_id ] = pgi;
        }

        if (pgi !is null && gi !is null)
        {
            gi.parents[ id ] = pgi;
        }

        return true;
    }

    // отмечает группу как deprecated
    void deprecated_group(string group_id)
    {
        GroupInfo gi = group_index.get(group_id, null);

        if (gi !is null)
        {
            gi.is_deprecated = true;
        }
    }

    void put(string subject_group_id, string object_group_id, ubyte req_access, ubyte res_access)
    {
        stderr.writefln("cache.put, subject_group_id=%s, object_group_id=%s, req_access=%s res_access=%s", subject_group_id, object_group_id,
                        access_to_pretty_string1(req_access), access_to_pretty_string1(res_access));

        string       ckey = subject_group_id ~ object_group_id;
        CacheElement ce   = ckey_2_cache_element.get(ckey, null);

        stderr.writefln("cache.put, ckey=%s", ckey);

        if (ce is null)
        {
            ce                           = new CacheElement(req_access, res_access);
            ce.subject_group             = group_index.get(subject_group_id, null);
            ce.object_group              = group_index.get(object_group_id, null);
            ckey_2_cache_element[ ckey ] = ce;
        }
        else
        {
            ce.req_access = ce.req_access | req_access;
            ce.res_access = ce.res_access | res_access;
        }
    }

    bool is_contains_deprecated_way_up(GroupInfo start_gr)
    {
        foreach (gr; start_gr.parents)
        {
            if (gr.is_deprecated == true)
                return false;

            if (is_contains_deprecated_way_up(gr) == false)
                return false;
        }
        return true;
    }

    int get(string subject_group_id, string object_group_id, ubyte req_access)
    {
        ubyte        res;

        string       ckey = subject_group_id ~ object_group_id;

        CacheElement ce = ckey_2_cache_element.get(ckey, null);

        stderr.writefln("cache.get, subject_group_id=%s, object_group_id=%s, req_access=%s", subject_group_id, object_group_id,
                        access_to_pretty_string1(
                                                 req_access));

        stderr.writefln("cache.get, ckey=%s", ckey);

        if (ce !is null)
        {
            stderr.writefln("cache.get, ce !is null");

            // проверить актуальность subject дерева в верх
            if (ce.subject_group is null)
                return -1;

            stderr.writefln("cache.get, #1");

            if (is_contains_deprecated_way_up(ce.subject_group) == true)
            {
                ckey_2_cache_element.remove(ckey);
                return -1;
            }

            if (is_contains_deprecated_way_up(ce.object_group) == true)
            {
                ckey_2_cache_element.remove(ckey);
                return -1;
            }

            res = req_access & ce.req_access;

            int ea = ckey_2_permissons.get(ckey, -1);
            if (ea != -1)
            {
                // для пары subject + object были изменения, откорректировать результат из кэша
                return res & cast(ubyte)ea;
            }
        }
        else
            return -1;

        return res;
    }
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