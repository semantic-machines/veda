/**
 * VQL executor
 */

module veda.search.xapian.xapian_search;

private
{
    import std.string, std.stdio, std.conv, std.outbuffer, core.stdc.string;
    import veda.util.container, veda.common.logger, veda.core.util.utils;
    import veda.core.common.context, veda.core.common.define, veda.core.common.know_predicates, veda.common.type;
    import veda.search.common.isearch, veda.search.common.vel, veda.search.xapian.xapian_reader;
    import veda.onto.individual;
}

class XapianSearch : Search
{
    private Context      context;
    private XapianReader xr;
    private Logger       log;

    this(Context _context)
    {
        context = _context;
        log     = context.get_logger();
        xr      = new XapianReader(_context);
    }

    public bool close_db()
    {
        return xr.close_dbs();
    }

    public void reopen_db()
    {
        xr.reopen_dbs();
    }

    public int query(string user_uri, string filter, string sort, string db_names, int top, int limit,
                     ref Individual[] individuals, OptAuthorize op_auth, bool trace)
    {
        int                       res_count;

        void delegate(string uri) dg;
        void collect_subject(string uri)
        {
            if (uri is null)
            {
                individuals = individuals.init;
                return;
            }

            Individual individual = Individual();

            context.get_storage().get_obj_from_individual_storage(uri, individual);

            if (individual.getStatus() == ResultCode.NotFound)
            {
                log.trace("ERR! FT:get Unable to find the object [%s] it should be, query=[%s]", uri, filter);
            }
            else if (individual.getStatus() == ResultCode.Ok)
            {
                individuals ~= individual;
            }
            else
            {
                log.trace("ERR!: FT:get invalid individual=%s, status=%s, query=%s", uri, individual.getStatus(), filter);
            }
        }
        dg = &collect_subject;

        SearchResult sr = xr.query(user_uri, filter, sort, db_names, 0, top, limit, dg, op_auth, trace);
        res_count = sr.count;

        return res_count;
    }

    public SearchResult query(string user_uri, string filter, string sort, string db_names, int from, int top, int limit,
                              OptAuthorize op_auth, bool trace)
    {
        string[]                  res;

        void delegate(string uri) dg;
        void collect_subject(string uri)
        {
            if (uri is null)
            {
                res = res.init;
                return;
            }
            res ~= uri;
        }
        dg = &collect_subject;

        SearchResult sr = xr.query(user_uri, filter, sort, db_names, from, top, limit, dg, op_auth, trace);

        if (sr.result_code == ResultCode.Ok)
            sr.result = res;

        return sr;
    }
}
