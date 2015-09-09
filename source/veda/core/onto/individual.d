/**
 * Индивидуал (субьект)
 */
module onto.individual;

private
{
    import std.stdio, std.typecons, std.conv, std.exception : assumeUnique;

    import onto.resource;
    import veda.core.know_predicates, veda.core.context;
    import util.utils, util.container, util.cbor8individual;
}
/// Массив индивидуалов
alias Individual[] Individuals;

/// Индивидуал
public struct Individual
{
    /// URI
    string uri;

    /// Hashmap массивов ресурсов, где ключем является predicate (P из SPO)
    Resources[ string ]    resources;

    private ResultCode rc;

    /// Вернуть код ошибки
    public ResultCode  getStatus()
    {
        return rc;
    }

    void setStatus(ResultCode _rc)
    {
        rc = _rc;
    }

    this(string _uri, Resources[ string ] _resources)
    {
        uri       = _uri;
        resources = _resources;
    }

    immutable this(string _uri, immutable(Resources[ string ]) _resources)
    {
        uri       = _uri;
        resources = _resources;
    }

    immutable(Individual) idup()
    {
        resources.rehash();
        immutable Resources[ string ]    tmp1 = assumeUnique(resources);

        immutable(Individual) result = immutable Individual(uri, tmp1);
        return result;
    }

    Individual dup()
    {
        resources.rehash();
        Resources[ string ]    tmp1 = resources.dup;

        Individual result = Individual(uri, tmp1);
        return result;
    }

    Resource getFirstResource(string predicate)
    {
        Resources rss;

        rss = resources.get(predicate, rss);
        if (rss.length > 0)
            return rss[ 0 ];

        return Resource.init;
    }

    string getFirstLiteral(string predicate)
    {
        Resources rss;

        rss = resources.get(predicate, rss);
        if (rss.length > 0)
            return rss[ 0 ].literal;

        return null;
    }

    long getFirstInteger(string predicate, long default_value = 0)
    {
        Resources rss;

        rss = resources.get(predicate, rss);
        if (rss.length > 0)
            return rss[ 0 ].get!long;

        return default_value;
    }

    void addResource(string uri, Resource rs)
    {
        Resources rss = resources.get(uri, Resources.init);

        rss ~= rs;
        resources[ uri ] = rss;
    }

    void set_Resources(string uri, Resources in_rss)
    {
        Resources new_rss;

        foreach (in_rs; in_rss)
        {
            new_rss ~= in_rs;
        }

        resources[ uri ] = new_rss;
    }

    void add_unique_Resources(string uri, Resources in_rss)
    {
        Resources new_rss;
        Resources rss = resources.get(uri, Resources.init);

        foreach (rs; rss)
        {
            new_rss ~= rs;
        }

        foreach (in_rs; in_rss)
        {
            bool find = false;
            foreach (rs; rss)
            {
                if (in_rs == rs)
                {
                    find = true;
                    break;
                }
            }
            if (find == false)
                new_rss ~= in_rs;
        }

        resources[ uri ] = new_rss;
    }

    Resources getResources(string predicate)
    {
        Resources rss;

        rss = resources.get(predicate, rss);
        return rss;
    }

    bool isExist(T) (string predicate, T object)
    {
        Resources rss;

        rss = resources.get(predicate, rss);
        foreach (rs; rss)
        {
            //writeln ("@rs=[", rs.get!string, "] object=[", object, "]");
            if (rs == object)
            {
                //writeln ("@ true");
                return true;
            }
        }
        return false;
    }

    bool anyExist(T) (string predicate, T[] objects)
    {
        Resources rss;

        rss = resources.get(predicate, rss);
        foreach (rs; rss)
        {
            foreach (object; objects)
            {
                if (rs == object)
                    return true;
            }
        }
        return false;
    }

    Individual apply(Individual item)
    {
        Individual res = this.dup();

        if (item.uri != uri)
            return res;

        foreach (key, rss; item.resources)
        {
            Resources new_rss = Resources.init;
            foreach (rs; rss)
            {
                new_rss ~= rs;
            }

            Resources exists_rss = resources.get(key, Resources.init);
            foreach (rs; exists_rss)
            {
                // проверить, чтоб в new_rss, не было rs
                bool rs_found = false;
                foreach (rs1; new_rss)
                {
                    if (rs1 == rs)
                    {
                        rs_found = true;
                        break;
                    }
                }

                if (rs_found == false)
                    new_rss ~= rs;
            }

            res.resources[ key ] = new_rss;
        }

        return res;
    }

    Individual repare_unique(string predicate)
    {
        Resources rdf_type = resources.get(predicate, Resources.init);

        if (rdf_type != Resources.init)
        {
            Individual res = this.dup();

            Resources  new_rss = Resources.init;

            auto       uniq_rdf_type = std.algorithm.uniq(rdf_type);

            Resource   rc;
            while (uniq_rdf_type.empty == false)
            {
                rc = uniq_rdf_type.front;
                new_rss ~= rc;
                uniq_rdf_type.popFront;
            }

            res.resources[ predicate ] = new_rss;
            return res;
        }
        return this;
    }
}
