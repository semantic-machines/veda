/**
 *     XAPIAN READER
 */

module search.xapian_reader;

import std.concurrency, std.outbuffer, std.datetime, std.conv, std.typecons, std.stdio, std.string, std.file, std.container.slist;
import bind.xapian_d_header;
import veda.core.util.utils, veda.util.cbor, veda.core.common.define, veda.core.common.know_predicates, veda.core.common.context, veda.core.log_msg;
import search.vel, veda.core.search.xapian_vql, search.indexer_property;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "SEARCH");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

protected byte err;

// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
interface SearchReader
{
    public int get(Ticket *ticket, string str_query, string str_sort, string db_names, int top, int limit,
                   void delegate(string uri) add_out_element, bool inner_get);

    public void reopen_db();
}

class Database_QueryParser
{
    XapianDatabase    db;
    XapianQueryParser qp;
}


class XapianReader : SearchReader
{
    private                 Database_QueryParser[ string[] ]      using_dbqp;

    private                 XapianDatabase[ string ]              opened_db;

    private XapianStem      xapian_stemmer;
    private string          xapian_lang = "russian";
    private XapianEnquire   xapian_enquire;

    private Context         context;
    private IndexerProperty iproperty;

    this(Context _context)
    {
        context   = _context;
        iproperty = new IndexerProperty(context);
    }

    private string dummy;
    private double d_dummy;

    string getDatabasesOfClass(TTA tta, ref bool[ string ] databasenames, IndexerProperty iproperty)
    {
        if (tta is null)
            return null;

        string ll = getDatabasesOfClass(tta.L, databasenames, iproperty);
        string rr = getDatabasesOfClass(tta.R, databasenames, iproperty);

        if (ll !is null && rr !is null)
        {
            if (ll == "rdf:type")
            {
                string dbn = iproperty.get_dbname_of_class(rr);
                databasenames[ dbn ] = false;
            }
            else if (ll == "v-s:deleted")
            {
                databasenames[ "deleted" ] = false;
            }
        }

        return tta.op;
    }


    public int get(Ticket *ticket, string str_query, string str_sort, string _db_names, int top, int limit,
                   void delegate(string uri) add_out_element, bool inner_get)
    {
        int[ string ] key2slot = context.get_key2slot();

        if (key2slot == (int[ string ]).init)
            return 0;

        //writeln ("@key2slot=", key2slot);

        XapianQuery query;
        TTA         tta = parse_expr(str_query);

        if (tta is null)
        {
            log.trace("[%s]fail parse query (phase 1) [%s], tta is null", context.get_name(), str_query);
            return 0;
        }

        string[] db_names;

        if (_db_names is null || _db_names.length == 0)
        {
            // если не указанны базы данных, то попробуем определить их из текста запроса

            if (inner_get == false)
                iproperty.load();

            bool[ string ] databasenames = iproperty.get_dbnames();

            getDatabasesOfClass(tta, databasenames, iproperty);

            foreach (key, value; databasenames)
            {
                if (value == false)
                    db_names ~= key;
            }
        }
        else
        {
            db_names = split(_db_names, ',');
            int idx = 0;
            foreach (el; db_names)
            {
                if (el[ 0 ] == ' ' || el[ $ ] == ' ')
                    db_names[ idx ] = strip(el);
                idx++;
            }
        }

        if (db_names.length == 0)
            db_names = [ "base" ];


        if (trace_msg[ 321 ] == 1)
            log.trace("[%s][Q:%X] query [%s]", context.get_name(), cast(void *)str_query, str_query);

        if (trace_msg[ 322 ] == 1)
            log.trace("[%s][Q:%X] TTA [%s]", context.get_name(), cast(void *)str_query, tta.toString());

        //log.trace("@xapian_reader:get #1 [%s]", str_query);
        context.ft_check_for_reload(&reopen_db);

        Database_QueryParser db_qp = get_dbqp(db_names);

        int                  state         = -1;
        int                  attempt_count = 1;

        while (state < 0)
        {
            try
            {
                transform_vql_to_xapian(context, tta, "", dummy, dummy, query, key2slot, d_dummy, 0, db_qp.qp);
                state = 0;
            }
            catch (XapianError ex)
            {
                log.trace("[%s]fail parse query (phase 2) [%s], err:[%s]", context.get_name(), str_query, ex.msg);
                state = ex.code;
            }
            catch (Throwable tr)
            {
                log.trace("[%s]fail parse query (phase 2) [%s], err:[%s]", context.get_name(), str_query, tr.msg);
                return 0;
            }

            if (state < 0)
            {
                attempt_count++;
                if (attempt_count > 10)
                {
                    query = null;
                    break;
                }

                reopen_db();
                log.trace("[%s][Q:%X] transform_vql_to_xapian, attempt=%d",
                          context.get_name(), cast(void *)str_query,
                          attempt_count);
            }
        }

        if (opened_db.keys.length == 0)
            return 0;

        if (trace_msg[ 323 ] == 1)
            log.trace("[%s][Q:%X] xapian query [%s]", context.get_name(), cast(void *)str_query, get_query_description(query));

        if (query !is null)
        {
            xapian_enquire = db_qp.db.new_Enquire(&err);

            if (err < 0)
            {
                log.trace("ERR! xapian_reader:get err=%s", get_xapian_err_msg(err));
                return 0;
            }

            XapianMultiValueKeyMaker sorter = get_sorter(str_sort, key2slot);

            xapian_enquire.set_query(query, &err);
            if (sorter !is null)
            {
                xapian_enquire.set_sort_by_key(sorter, true, &err);
            }

            state = -1;
            while (state < 0)
            {
                state = exec_xapian_query_and_queue_authorize(ticket, xapian_enquire, top, limit, add_out_element,
                                                              context);
                if (state < 0)
                {
                    add_out_element(null); // reset previous collected data
                    attempt_count++;
                    if (attempt_count > 10)
                        break;

                    reopen_db();
                    log.trace("WARN! [%s][%s] exec_xapian_query_and_queue_authorize, res=%d, attempt=%d",
                              context.get_name(), str_query,
                              state, attempt_count);
                }
            }

            int read_count = 0;

            if (state > 0)
                read_count = state;

            if (state < 0)
                log.trace("ERR! [%s][Q:%X] exec_xapian_query_and_queue_authorize, attempt=%d, query=[%s]",
                          context.get_name(), cast(void *)str_query,
                          attempt_count, str_query);

            destroy_Enquire(xapian_enquire);
            destroy_Query(query);
            destroy_MultiValueKeyMaker(sorter);

            return read_count;
        }
        else
        {
            //log.trace("[%s]invalid query [%s]", context.get_name(), str_query);
        }

        return 0;
    }

////////////////////////////////////////////////////////

    Database_QueryParser get_dbqp(string[] db_names)
    {
        Database_QueryParser dbqp = using_dbqp.get(db_names, null);

        if (dbqp is null)
        {
            dbqp    = new Database_QueryParser();
            dbqp.db = new_Database(&err);

            foreach (el; db_names)
            {
                XapianDatabase _db = open_db(el);
                if (_db !is null)
                {
                    dbqp.db.add_database(_db, &err);
                    if (err != 0)
                        log.trace("xapian_reader:add_database:err=%s", get_xapian_err_msg(err));
                }
                else
                    log.trace("xapian_reader:add_database:db not opened:%s", el);
            }

            dbqp.qp = new_QueryParser(&err);
            if (err != 0)
                log.trace("xapian_reader:new_QueryParser:err=%s", get_xapian_err_msg(err));

            xapian_stemmer = new_Stem(cast(char *)xapian_lang, cast(uint)xapian_lang.length, &err);
            dbqp.qp.set_stemmer(xapian_stemmer, &err);
            if (err != 0)
                log.trace("xapian_reader:set_stemmer:err=%s", get_xapian_err_msg(err));

            dbqp.qp.set_database(dbqp.db, &err);
            if (err != 0)
                log.trace("xapian_reader:set_database:err=%s", get_xapian_err_msg(err));

            using_dbqp[ db_names.idup ] = dbqp;
        }

        return dbqp;
    }

    private XapianDatabase open_db(string db_name)
    {
        byte           err;
        XapianDatabase db;

        db = opened_db.get(db_name, null);

        if (db is null)
        {
            string path = get_xapiab_db_path(db_name);

            if (path !is null)
            {
                db = new_Database(path.ptr, cast(uint)path.length, xapian_db_type, &err);

                if (err < 0)
                    log.trace("ERR! xapian_reader:open_db:new_Database[%s]:err=%s", db_name, get_xapian_err_msg(err));
                else
                    opened_db[ db_name ] = db;
            }
            else
                log.trace("ERR! xapian_reader:open_db: not set db_path to [%s]", db_name);
        }

        if (db is null)
            log.trace("ERR! xapian_reader:open_db: not open db [%s]", db_name);

        return db;
    }

    public void reopen_db()
    {
        //log.trace("@FTR:reopen_db");

        foreach (el; using_dbqp.values)
        {
            el.db.reopen(&err);
            if (err != 0)
                log.trace("ERR! xapian_reader:reopen_db:err=%s", get_xapian_err_msg(err));

            el.qp.set_database(el.db, &err);
            if (err != 0)
                log.trace("ERR! xapian_reader:set_database:err=%s", get_xapian_err_msg(err));
        }
//        foreach (db; opened_db.values)
//        {
//          db.reopen(&err);
//            if (err != 0)
//                writeln("xapian_reader:reopen_db:err", err);
//        }
    }

    private void close_db()
    {
        foreach (el; using_dbqp.values)
        {
            el.db.close(&err);
            if (err != 0)
                log.trace("xapian_reader:close database:err=%s", get_xapian_err_msg(err));
        }
        foreach (db; opened_db.values)
        {
            db.close(&err);
            if (err != 0)
                log.trace("xapian_reader:close database:err=%s", get_xapian_err_msg(err));
        }
    }
}

