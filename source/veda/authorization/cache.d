module veda.authorization.cache;

class GroupInfo
{
    int       level;
    bool      is_deprecated;
    GroupInfo parent;
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

class Cache
{
    CacheElement[ string ] ckey_2_cache_element;
    GroupInfo[ string ] group_index;

    void add_group(string id, string parent_id)
    {
        GroupInfo gi = group_index.get(id, null);

        if (gi !is null && gi.is_deprecated)
            gi = null;

        if (gi is null)
        {
            int level;
            gi = new GroupInfo();
            if (parent_id !is null)
            {
                GroupInfo pgi = group_index.get(parent_id, null);
                if (pgi !is null)
                {
                    level     = pgi.level;
                    gi.parent = pgi;
                }
            }

            gi.level          = level;
            group_index[ id ] = gi;
        }
    }

    void put(string subject_group_id, string object_group_id, ubyte req_access, ubyte res_access, string permission_id)
    {
        string       ckey = subject_group_id ~ object_group_id;
        CacheElement ce   = ckey_2_cache_element.get(ckey, null);

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

    int get(string subject_group_id, string object_group_id, ubyte req_access)
    {
        ubyte        res;

        string       ckey = subject_group_id ~ object_group_id;

        CacheElement ce = ckey_2_cache_element.get(ckey, null);

        if (ce !is null)
        {
            // проверить актуальность subject дерева в верх
            if (ce.subject_group is null)
                return -1;

            for (GroupInfo igri = ce.subject_group; igri !is null; igri = igri.parent)
            {
                if (igri.is_deprecated == true)
                {
                    // ветки выше устарели, удалить этот результат из кэша
                    ckey_2_cache_element.remove(ckey);
                    return -1;
                }
            }

            // проверить актуальность object дерева в верх
            if (ce.object_group is null)
                return -1;

            for (GroupInfo igri = ce.object_group; igri !is null; igri = igri.parent)
            {
                if (igri.is_deprecated == true)
                {
                    // ветки выше устарели, удалить этот результат из кэша
                    ckey_2_cache_element.remove(ckey);
                    return -1;
                }
            }


            if (ce.object_group)
                return req_access & ce.req_access;
        }

        return res;
    }

    void reset_group(string group_id)
    {
        GroupInfo gi = group_index.get(group_id, null);

        if (gi !is null)
        {
            gi.is_deprecated = true;
        }
    }

    void reset_permission(string permission_id)
    {
    }
}

