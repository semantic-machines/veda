/**
 *     XAPIAN READER
 */

module veda.core.search.xapian_reader;

import std.concurrency, std.outbuffer, std.datetime, std.conv, std.typecons, std.stdio, std.string, std.file, std.container.slist;
import veda.bind.xapian_d_header;
import veda.core.util.utils, veda.util.cbor, veda.core.common.define, veda.core.common.know_predicates, veda.core.common.context;
import veda.core.common.log_msg, veda.common.logger;
import veda.core.search.vel, veda.core.search.xapian_vql, veda.core.search.indexer_property, veda.util.module_info;

protected byte err;

// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
interface SearchReader
{
    public SearchResult get(Ticket *ticket, string str_query, string str_sort, string db_names, int from, int top, int limit,
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
    XapianVQL               xpnvql;
    Logger                  log;
    private                 Database_QueryParser[ string[] ]      using_dbqp;

    private                 XapianDatabase[ string ]              opened_db;

    private XapianStem      xapian_stemmer;
    private string          xapian_lang = "russian";
    private XapianEnquire   xapian_enquire;

    private Context         context;
    private IndexerProperty iproperty;
    ModuleInfoFile          mdif;

    this(Context _context)
    {
        context   = _context;
        xpnvql    = new XapianVQL(context.get_logger());
        log       = context.get_logger();
        iproperty = new IndexerProperty(context);
    }

    private MInfo get_info()
    {
        if (mdif is null)
        {
            mdif = new ModuleInfoFile(text(P_MODULE.fulltext_indexer), log, OPEN_MODE.READER);
        }
        MInfo info = mdif.get_info();
        return info;
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


    public SearchResult get(Ticket *ticket, string str_query, string str_sort, string _db_names, int from, int top, int limit,
                            void delegate(string uri) add_out_element, bool inner_get)
    {
        SearchResult sr;

        int[ string ] key2slot = context.get_key2slot();

        if (key2slot == (int[ string ]).init)
            return sr;

        //log.trace ("@key2slot=%s", key2slot);

        XapianQuery query;
        TTA         tta = parse_expr(str_query);

        if (tta is null)
        {
            log.trace("fail parse query (phase 1) [%s], tta is null", str_query);
            sr.result_code = ResultCode.Bad_Request;
            return sr;
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
            log.trace("[Q:%X] query [%s]", cast(void *)str_query, str_query);

        if (trace_msg[ 322 ] == 1)
            log.trace("[Q:%X] TTA [%s]", cast(void *)str_query, tta.toString());

        long cur_committed_op_id = get_info().committed_op_id;
        if (cur_committed_op_id > committed_op_id)
        {
            log.trace("reopen_db: cur_committed_op_id(%d) > committed_op_id(%d)", cur_committed_op_id, committed_op_id);
            reopen_db();
        }
        else
        {
            //log.trace ("cur_committed_op_id=%d, committed_op_id=%d", cur_committed_op_id, committed_op_id);
        }

        Database_QueryParser db_qp = get_dbqp(db_names);

        int                  state         = -1;
        int                  attempt_count = 1;

        while (state < 0)
        {
            try
            {
                xpnvql.transform_vql_to_xapian(context, tta, "", dummy, dummy, query, key2slot, d_dummy, 0, db_qp.qp);
                state = 0;
            }
            catch (XapianError ex)
            {
                log.trace("fail parse query (phase 2) [%s], err:[%s]", str_query, ex.msg);
                state = ex.code;
            }
            catch (Throwable tr)
            {
                log.trace("fail parse query (phase 2) [%s], err:[%s]", str_query, tr.msg);
                sr.result_code = ResultCode.Bad_Request;
                return sr;
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
                log.trace("[Q:%X] transform_vql_to_xapian, attempt=%d",
                          cast(void *)str_query,
                          attempt_count);
            }
        }

        if (opened_db.keys.length == 0)
            return sr;

        if (trace_msg[ 323 ] == 1)
            log.trace("[Q:%X] xapian query [%s]", cast(void *)str_query, xpnvql.get_query_description(query));

        if (query !is null)
        {
            xapian_enquire = db_qp.db.new_Enquire(&err);

            if (err < 0)
            {
                log.trace("ERR! xapian_reader:get err=%s", get_xapian_err_msg(err));
                sr.result_code = ResultCode.Bad_Request;
                return sr;
            }

            XapianMultiValueKeyMaker sorter = xpnvql.get_sorter(str_sort, key2slot);

            xapian_enquire.set_query(query, &err);
            if (sorter !is null)
                xapian_enquire.set_sort_by_key(sorter, true, &err);

            while (sr.result_code != ResultCode.OK)
            {
                sr = xpnvql.exec_xapian_query_and_queue_authorize(ticket, xapian_enquire, from, top, limit, add_out_element,
                                                                  context);
                if (sr.result_code != ResultCode.OK)
                {
                    add_out_element(null); // reset previous collected data
                    attempt_count++;
                    if (attempt_count > 10)
                        break;

                    reopen_db();
                    log.trace("WARN! [%s] exec_xapian_query_and_queue_authorize, res=%d, attempt=%d",
                              str_query, state, attempt_count);
                }
            }

            if (sr.result_code != ResultCode.OK)
                log.trace("ERR! [Q:%X] exec_xapian_query_and_queue_authorize, attempt=%d, query=[%s]",
                          cast(void *)str_query, attempt_count, str_query);

            destroy_Enquire(xapian_enquire);
            destroy_Query(query);
            destroy_MultiValueKeyMaker(sorter);
        }
        else
        {
            sr.result_code = ResultCode.Bad_Request;
            log.trace("invalid query [%s]", str_query);
        }

        return sr;
    }

////////////////////////////////////////////////////////
    long committed_op_id;

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

        committed_op_id = get_info().committed_op_id;

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
        long cur_committed_op_id = get_info().committed_op_id;

        //log.trace("reopen_db, prev committed_op_id=%d, now committed_op_id=%d", committed_op_id, cur_committed_op_id);

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

        committed_op_id = cur_committed_op_id;
    }

    public bool close_db()
    {
        foreach (el; using_dbqp.values)
        {
            el.db.close(&err);
            if (err != 0)
            {
                log.trace("xapian_reader:close database:err=%s", get_xapian_err_msg(err));
                return false;
            }
        }
        foreach (db; opened_db.values)
        {
            db.close(&err);
            if (err != 0)
            {
                log.trace("xapian_reader:close database:err=%s", get_xapian_err_msg(err));
                return false;
            }
        }
        return true;
    }
}

