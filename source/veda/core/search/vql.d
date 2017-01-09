/**
 * VQL executor
 */

module veda.core.search.vql;

private
{
    import std.string, std.array, std.stdio, std.conv, std.datetime, std.json, std.outbuffer, core.stdc.string, std.concurrency;
    import veda.util.container, veda.common.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual;
    import veda.core.common.context, veda.core.common.define, veda.core.common.know_predicates;
    import veda.core.search.vel, veda.core.search.xapian_reader;
    import veda.onto.individual, veda.core.az.acl;
}

static const int RETURN    = 0;
static const int FILTER    = 1;
static const int SORT      = 2;
static const int RENDER    = 3;
static const int AUTHORIZE = 4;
static const int SOURCE    = 5;

static const int XAPIAN = 2;
static const int LMDB   = 3;

class VQL
{
    private string[]     sections;
    private bool[]       section_is_found;
    private string[]     found_sections;

    private Context      context;
    private XapianReader xr;
    private Logger       log;

    this(Context _context)
    {
        sections         = [ "return", "filter", "sort", "render", "authorize", "source" ];
        found_sections   = new string[ sections.length ];
        section_is_found = new bool[ sections.length ];

        context = _context;
        log     = context.get_logger();
        xr      = new XapianReader(_context);
    }

    public bool close_db()
    {
        return xr.close_db();
    }

    public void reopen_db()
    {
        xr.reopen_db();
    }

    public int get(Ticket *ticket, string filter, string freturn, string sort, int top, int limit,
                   ref Individual[] individuals, bool inner_get = false)
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

            string     data = context.get_from_individual_storage(uri);

            if (data is null)
            {
                log.trace("ERR! Unable to find the object [%s] it should be, query=[%s]", text(uri), filter);
            }
            else
            {
                if (cbor2individual(&individual, data) > 0)
                {
                    individuals ~= individual;
                }
                else
                {
                    log.trace("ERR!:invalid individual=%s", uri);
                }
            }
        }
        dg = &collect_subject;

        SearchResult sr = xr.get(ticket, filter, freturn, sort, 0, top, limit, dg, inner_get);
        res_count = sr.count;

        return res_count;
    }

    public SearchResult get(Ticket *ticket, string filter, string freturn, string sort, int from, int top, int limit,
                            bool inner_get = false)
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

        SearchResult sr = xr.get(ticket, filter, freturn, sort, from, top, limit, dg, inner_get);

        if (sr.result_code == ResultCode.OK)
            sr.result = res;

        return sr;
    }

    public int get(Ticket *ticket, string query_str, ref Individual[] res, bool inner_get = false)
    {
        //		if (ticket !is null)
        //		writeln ("userId=", ticket.userId);

        //		writeln ("@ query_str=", query_str);
        //		StopWatch sw;
        //		sw.start();

        split_on_section(query_str);
        int top = 10000;
        try
        {
            if (found_sections[ RENDER ] !is null && found_sections[ RENDER ].length > 0)
                top = parse!int (found_sections[ RENDER ]);
        } catch (Exception ex)
        {
        }
        int limit = 10000;
        try
        {
            if (found_sections[ AUTHORIZE ] !is null && found_sections[ AUTHORIZE ].length > 0)
                limit = parse!int (found_sections[ AUTHORIZE ]);
        } catch (Exception ex)
        {
        }
        string sort;
        if (section_is_found[ SORT ] == true)
            sort = found_sections[ SORT ];
        int type_source = XAPIAN;
        if (found_sections[ SOURCE ] == "xapian")
            type_source = XAPIAN;
        else if (found_sections[ SOURCE ] == "lmdb")
            type_source = LMDB;

//        OI  from_search_point = null;

//        if (from_search_points.size > 0)
//            from_search_point = from_search_points.items[ 0 ];

        //writeln ("found_sections[SOURCE]=", found_sections[SOURCE]);

        string dummy;
        double d_dummy;
        int    res_count;

//        if (type_source == LMDB)
//        {
//            if (found_sections[ FILTER ] !is null)
//            {
//                TTA tta = parse_expr(found_sections[ FILTER ]);
//                transform_and_execute_vql_to_lmdb(tta, "", dummy, dummy, d_dummy, 0, res, context);
//            }
//        }
//        else
        if (type_source == XAPIAN)
        {
            void delegate(string uri) dg;
            void collect_subject(string uri)
            {
                if (uri is null)
                {
                    res = res.init;
                    return;
                }
                string data = context.get_from_individual_storage(uri);

                if (data is null)
                {
                    log.trace("ERR! Unable to find the object [%s] it should be, query=[%s]", text(uri), query_str);
                }
                else
                {
                    Individual ind;

                    if (cbor2individual(&ind, data) > 0)
                    {
                        res ~= ind;
                    }
                    else
                    {
                        //writeln("ERR! invalid individual=", uri);
                        context.reopen_ro_subject_storage_db();
                        data = context.get_from_individual_storage(uri);
                        if (cbor2individual(&ind, data) > 0)
                        {
                            res ~= ind;
                        }
                        else
                        {
                            log.trace("ERR! vql.get attempt 2, invalid individual=%s", uri);
                        }
                    }
                }
            }
            dg = &collect_subject;

            SearchResult sr = xr.get(ticket, found_sections[ FILTER ], found_sections[ RETURN ], sort, 0, top, limit, dg, inner_get);
            res_count = sr.count;
        }

//          sw.stop();
//          long t = cast(long) sw.peek().usecs;
//          writeln("execute:", t, " µs");

        return res_count;
    }

/*
    private void remove_predicates(Subject ss, ref string[ string ] fields)
    {
        if (ss is null || ("*" in fields) !is null)
            return;

        // TODO возможно не оптимальная фильтрация
        foreach (pp; ss.getPredicates)
        {
   //			writeln ("pp=", pp);
            if ((pp.predicate in fields) is null)
            {
                pp.count_objects = 0;
            }
        }
    }
 */

    private void split_on_section(string query)
    {
        section_is_found[] = false;
        if (query is null)
            return;

        for (int pos = 0; pos < query.length; pos++)
        {
            for (int i = 0; i < sections.length; i++)
            {
                char cc = query[ pos ];
                if (section_is_found[ i ] == false)
                {
                    found_sections[ i ] = null;

                    int j     = 0;
                    int t_pos = pos;
                    while (sections[ i ][ j ] == cc && t_pos < query.length && j < sections[ i ].length)
                    {
                        j++;
                        t_pos++;

                        if (t_pos >= query.length || j >= sections[ i ].length)
                            break;

                        cc = query[ t_pos ];
                    }

                    if (j == sections[ i ].length)
                    {
                        pos = t_pos;
                        // нашли
                        section_is_found[ i ] = true;

                        while (query[ pos ] != '{' && pos < query.length)
                            pos++;
                        pos++;

                        while (query[ pos ] == ' ' && pos < query.length)
                            pos++;

                        int bp = pos;
                        while (query[ pos ] != '}' && pos < query.length)
                            pos++;
                        pos--;

                        while (query[ pos ] == ' ' && pos > bp)
                            pos--;
                        int ep = pos + 1;

                        found_sections[ i ] = query[ bp .. ep ];
                    }
                }
            }
        }
    }
}
