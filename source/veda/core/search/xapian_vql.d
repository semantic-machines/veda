/**
 * VQL -> xapian
 */

module veda.core.search.xapian_vql;

import std.string, std.concurrency, std.stdio, std.datetime, std.conv, std.algorithm;
import veda.bind.xapian_d_header;
import veda.core.util.utils, veda.onto.onto, veda.common.logger;
import veda.core.search.vel;
import veda.common.type, veda.core.common.context, veda.core.common.define, veda.core.common.log_msg, veda.core.storage.lmdb_storage;


class XapianVQL
{
    private Logger log;
    protected byte err;

    this(Logger _log)
    {
        log = _log;
    }

    public XapianMultiValueKeyMaker get_sorter(string sort, ref int[ string ] key2slot)
    {
        XapianMultiValueKeyMaker sorter;

        if (sort !is null && sort.length > 0)
        {
            sorter = new_MultiValueKeyMaker(&err);
            foreach (field; split(sort, ","))
            {
                bool asc_desc;

                long bp = indexOf(field, '\'');
                long ep = lastIndexOf(field, '\'');
                long dp = lastIndexOf(field, " desc");

                if (ep > bp && ep - bp > 0)
                {
                    string key = field[ bp + 1 .. ep ];

                    if (dp > ep)
                        asc_desc = false;
                    else
                        asc_desc = true;

                    int slot = key2slot.get(key, -1);
                    if (slot >= 0)
                    {
                        if (trace_msg[ 200 ] == 1)
                            log.trace("sort key=%s, slot=%d", key, slot);
                        sorter.add_value(slot, asc_desc, &err);
                    }
                }
            }
        }
        return sorter;
    }

    enum TokenType
    {
        TEXT,
        NUMBER,
        DATE,
        BOOLEAN
    }

    private TokenType get_token_type(string token, out double value)
    {
        TokenType res = TokenType.TEXT;

        token = token.strip();

        if (token == "true")
        {
            value = 1;
            return TokenType.BOOLEAN;
        }
        else if (token == "false")
        {
            value = 0;
            return TokenType.BOOLEAN;
        }
        else if (token.length == 19 && token[ 4 ] == '-' && token[ 7 ] == '-' && token[ 10 ] == 'T' && token[ 13 ] == ':' && token[ 16 ] == ':')
        {
            value = stdTimeToUnixTime(SysTime.fromISOExtString(token).stdTime);
            return TokenType.DATE;
        }
        else if (token.length == 24 && token[ 4 ] == '-' && token[ 7 ] == '-' && token[ 10 ] == 'T' && token[ 13 ] == ':' && token[ 16 ] ==
                 ':' && token[ 19 ] == '.')
        {
            value = stdTimeToUnixTime(SysTime.fromISOExtString(token).stdTime);
            return TokenType.DATE;
        }
        else
        {
            bool is_digit = false;
            try
            {
                string tt = token.dup;
                value = parse!double (tt);
                return TokenType.NUMBER;
            }
            catch (Exception ex)
            {
            }
        }

        return res;
    }

    public string transform_vql_to_xapian(Context ctx, TTA tta, string p_op, out string l_token, out string op, out XapianQuery query,
                                          ref int[ string ] key2slot, out double _rd, int level, XapianQueryParser qp)
    {
        //log.trace ("tta in= %s", tta);
        prepare_subproperties(ctx, tta);
        //log.trace ("tta out= %s", tta);
        return _transform_vql_to_xapian(ctx, tta, p_op, l_token, op, query, key2slot, _rd, level, qp);
    }

    private void prepare_subproperties(Context ctx, TTA tta)
    {
        string ls;
        string rs;

        if (tta.L !is null)
        {
            if (tta.L.token_decor == Decor.RANGE)
            {
                string el = tta.L.op;
                if (el[ 0 ] == '\'' && el.length > 2 && el[ $ - 1 ] == '\'')
                    el = el[ 1..$ - 1 ];

                Names subproperties = ctx.get_onto().get_sub_properies(el);
                //log.trace("@0, subroperties of [%s]=%s", el, subproperties);

                TTA    tmpR   = tta.R;
                string tmp_op = tta.op;

                // L1, L2, L3
                // (L == R || (L1 == R || (L2 == R || L3 == R)))
                //
                TTA curL  = new TTA(tmp_op, new TTA(el, null, null), tmpR);
                TTA nextR = new TTA("||", null, null);
                tta.L  = curL;
                tta.R  = nextR;
                tta.op = "||";
                //log.trace ("#1 tta=%s", tta);

                int idx = 0;
                foreach (key, value; subproperties)
                {
                    //log.trace ("#2 nextR=%s", nextR);
                    //log.trace ("key=%s", key);
                    curL = new TTA(tmp_op, new TTA(key, null, null), tmpR);

                    idx++;
                    if (idx < subproperties.length)
                    {
                        nextR.L = curL;
                        nextR.R = new TTA("||", null, null);
                        nextR   = nextR.R;
                    }
                    else
                    {
                        nextR.op = curL.op;
                        nextR.L  = curL.L;
                        nextR.R  = curL.R;
                    }

                    //log.trace ("#3 nextR=%s", nextR);
                }
            }

            prepare_subproperties(ctx, tta.L);
        }

        if (tta.R !is null)
            prepare_subproperties(ctx, tta.R);
    }

    private string _transform_vql_to_xapian(Context ctx, TTA tta, string p_op, out string l_token, out string op, out XapianQuery query,
                                            ref int[ string ] key2slot, out double _rd, int level, XapianQueryParser qp)
    {
        //if (level == 0)
        //	log.trace ("----------------------------");

        //log.trace ("%d TTA=%s", level, tta);

        try
        {
            if (key2slot.length == 0)
            {
                log.trace("!!! WARN: key2slot is EMPTY, tta=%s", tta);
                return null;
            }

            string      dummy;
            double      rd, ld;
            XapianQuery query_r;
            XapianQuery query_l;

            if (tta.op == ">" || tta.op == "<")
            {
                if (tta.L is null || tta.R is null)
                {
                    log.trace("_transform_vql_to_xapian, invalid tta=[%s]", tta);
                    throw new XapianError(err, "invalid tta=" ~ text(tta));
                }

                string    ls = _transform_vql_to_xapian(ctx, tta.L, tta.op, dummy, dummy, query_l, key2slot, ld, level + 1, qp);
                string    rs = _transform_vql_to_xapian(ctx, tta.R, tta.op, dummy, dummy, query_r, key2slot, rd, level + 1, qp);

                double    value;
                TokenType rs_type = get_token_type(rs, value);
                if (rs_type == TokenType.DATE || rs_type == TokenType.NUMBER)
                {
                    l_token = ls;
                    op      = tta.op;
                    _rd     = value;
                    //writeln("@p RS=", rs);
                    //writeln("@p _rd=", _rd);
                    return rs;
                }
            }
            else if (tta.op == "==" || tta.op == "!=" || tta.op == "===")
            {
                bool is_strict_equality = false;
                if (tta.op == "===")
                {
                    is_strict_equality = true;
                    tta.op             = "==";
                }

                if (tta.L is null || tta.R is null)
                {
                    log.trace("_transform_vql_to_xapian, invalid tta=[%s]", tta);
                    throw new XapianError(err, "invalid tta=" ~ text(tta));
                }

                string ls = _transform_vql_to_xapian(ctx, tta.L, tta.op, dummy, dummy, query_l, key2slot, ld, level + 1, qp);
                string rs = _transform_vql_to_xapian(ctx, tta.R, tta.op, dummy, dummy, query_r, key2slot, rd, level + 1, qp);

                //log.trace("%d query_l=%s", level, query_l);
                //log.trace("%d query_r=%s", level, query_r);
                //log.trace("%d ls=%s", level, ls);
                //log.trace("%d rs=%s", level, rs);

                if (!is_strict_equality && rs.indexOf(':') > 0)
                {
                    Names subclasses = ctx.get_onto().get_sub_classes(rs);

                    foreach (classz; subclasses.keys)
                        rs ~= " OR " ~ classz;
                }

                if (query_l is null && query_r is null)
                {
                    string xtr;
                    if (ls != "*")
                    {
                        if (ls == "@")
                        {
                            feature_flag flags = feature_flag.FLAG_DEFAULT | feature_flag.FLAG_WILDCARD;

                            string       query_str = "uid_" ~ to_lower_and_replace_delimeters(rs);

                            if (tta.op == "!=")
                            {
                                flags     = flags | feature_flag.FLAG_PURE_NOT;
                                query_str = "NOT " ~ query_str;
                            }

                            query = qp.parse_query(cast(char *)query_str, query_str.length, flags, &err);
                            if (err != 0)
                                throw new XapianError(err, "parse_query1 query=" ~ query_str);
                        }
                        else
                        {
                            int slot;
                            if (rs !is null && rs.length > 2 && rs[ 0 ] == '*')
                                slot = key2slot.get(ls ~ "#F", -1);
                            else
                                slot = key2slot.get(ls, -1);

                            //log.trace("@p slot=%d, predicate=%s", slot, ls);

                            if (slot > 0)
                            {
                                double    value;
                                TokenType rs_type = get_token_type(rs, value);
                                if (rs_type == TokenType.BOOLEAN)
                                {
                                    xtr = "X" ~ text(slot) ~ "D";
                                    string query_str = "F";
                                    if (value == 1)
                                        query_str = "T";
                                    feature_flag flags = feature_flag.FLAG_DEFAULT | feature_flag.FLAG_PHRASE |
                                                         feature_flag.FLAG_LOVEHATE;

                                    query = qp.parse_query(cast(char *)query_str, query_str.length, flags, cast(char *)xtr,
                                                           xtr.length, &err);
                                    if (query is null)
                                        throw new XapianError(err, "parse_query '" ~ tta.toString() ~ "'");

                                    if (err != 0)
                                        throw new XapianError(err, "parse_query1.1 query=" ~ query_str);
                                }
                                else
                                {
                                    if (tta.R.token_decor == Decor.QUOTED || (indexOf(rs, '*') >= 0) && rs.length > 3)
                                    {
                                        char[] query_str = to_lower_and_replace_delimeters(rs).dup;
                                        if (rs[ 0 ] == '*')
                                            reverse(query_str);

                                        xtr = "X" ~ text(slot) ~ "X";

                                        feature_flag flags = feature_flag.FLAG_DEFAULT | feature_flag.FLAG_WILDCARD | feature_flag.FLAG_PHRASE |
                                                             feature_flag.FLAG_LOVEHATE;
                                        if (tta.op == "!=")
                                        {
                                            /*	TODO
                                             *  вероятно получаются не оптимальными запросы вида
                                             *  '*' == 'rdf' && '*' != 'List*'
                                             *  @query=Xapian::Query((rdf:(pos=1) AND (<alldocuments> AND_NOT (list:(pos=1) SYNONYM lists:(pos=1)))))
                                             */
                                            flags     = flags | feature_flag.FLAG_PURE_NOT;
                                            query_str = "NOT " ~ query_str;
                                        }

                                        query = qp.parse_query(cast(char *)query_str, query_str.length, flags, cast(char *)xtr,
                                                               xtr.length, &err);
                                        if (err != 0)
                                            throw new XapianError(err,
                                                                  cast(string)("parse_query2('x'=*) query='" ~ query_str ~ "', xtr='" ~ xtr ~ "'"));
                                    }
                                    else
                                    {
                                        if (tta.R.token_decor == Decor.RANGE)
                                        {
                                            string[] vals = rs.split(",");
                                            if (vals.length == 2)
                                            {
                                                double    c_from, c_to;

                                                TokenType tt = get_token_type(vals[ 0 ], c_from);
                                                if (tt == TokenType.DATE || tt == TokenType.NUMBER)
                                                {
                                                    tt = get_token_type(vals[ 1 ], c_to);
                                                    if (tt == TokenType.DATE || tt == TokenType.NUMBER)
                                                    {
                                                        query = new_Query_range(xapian_op.OP_VALUE_RANGE, slot, c_from, c_to, &err);
                                                        if (query is null)
                                                            throw new XapianError(err, "parse_query '" ~ tta.toString() ~ "'");
                                                    }
                                                }
                                            }
                                            else if (vals.length == 1)
                                            {
                                                string el = rs;
                                                if (el[ 0 ] == '\'' && el.length > 2 && el[ $ - 1 ] == '\'')
                                                    el = el[ 1..$ - 1 ];

                                                Names  subclasses = ctx.get_onto().get_sub_classes(el);
                                                string query_str  = el;
                                                xtr = "X" ~ text(slot) ~ "X";
                                                foreach (classz; subclasses.keys)
                                                    query_str ~= " OR " ~ classz;

                                                query_str = to_lower_and_replace_delimeters(query_str);

                                                feature_flag flags = feature_flag.FLAG_DEFAULT | feature_flag.FLAG_WILDCARD |
                                                                     feature_flag.FLAG_PHRASE |
                                                                     feature_flag.FLAG_LOVEHATE;

                                                query = qp.parse_query(cast(char *)query_str, query_str.length, flags, cast(char *)xtr,
                                                                       xtr.length, &err);

                                                if (err != 0)
                                                    throw new XapianError(err,
                                                                          cast(string)("parse_query2.1('x'=*) query='" ~ query_str ~ "', xtr='" ~ xtr
                                                                                       ~ "'"));

                                                //log.trace("_transform_vql_to_xapian: query_str=[%s], query=|%s|", query_str, get_query_description(query));
                                            }
                                        }
                                        else
                                        {
                                            double d_val;

                                            try
                                            {
                                                d_val = parse!double (rs);
                                            }
                                            catch (Exception ex)
                                            {
                                                writeln("Ex!: ", __FUNCTION__, ":", text(__LINE__), ", ", ex.msg ~ " [", rs, "]");
                                            }


                                            char *str_val;
                                            uint *str_val_length;
                                            sortable_serialise(d_val, &str_val, &str_val_length, &err);

                                            uint   len = *str_val_length;

                                            string tt = cast(string)str_val[ 0..len ];
                                            //writeln("@ length=", len, ", tt=", tt, ", d_val=", d_val);
                                            xtr   = "X" ~ text(slot) ~ "X" ~ tt;
                                            query = new_Query(cast(char *)xtr, cast(uint)xtr.length, &err);
                                            if (query is null)
                                                throw new XapianError(err, "parse_query '" ~ tta.toString() ~ "'");
                                        }

                                        if (err != 0)
                                            log.trace("XAPIAN:_transform_vql_to_xapian:parse_query3 ('x'=x) [%s], err=%s", xtr,
                                                      get_xapian_err_msg(err));
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        xtr = to_lower_and_replace_delimeters(rs);
                        //writeln("xtr=", xtr);

                        if (indexOf(xtr, '*') > 0 && xtr.length > 3)
                        {
                            feature_flag flags = feature_flag.FLAG_DEFAULT | feature_flag.FLAG_WILDCARD | feature_flag.FLAG_PHRASE;
                            if (tta.op == "!=")
                            {
/*	TODO
                         вероятно получаются не оптимальны запросы вида
                         '*' == 'rdf' && '*' != 'List*'
                         @query=Xapian::Query((rdf:(pos=1) AND (<alldocuments> AND_NOT (list:(pos=1) SYNONYM lists:(pos=1)))))
 */

                                flags = flags | feature_flag.FLAG_PURE_NOT;
                                xtr   = "NOT " ~ xtr;
                            }

                            query = qp.parse_query(cast(char *)xtr, xtr.length, flags, &err);

                            if (err != 0)
                                throw new XapianError(err, "parse_query4('*'=*) '" ~ xtr ~ "'");
                        }
                        else
                        {
                            query = qp.parse_query(cast(char *)xtr, xtr.length, &err);
                            if (err != 0)
                                throw new XapianError(err, "parse_query5('*'=x) '" ~ xtr ~ "'");
                        }
                    }
                }

                if (query_l !is null)
                    destroy_Query(query_l);
                if (query_r !is null)
                    destroy_Query(query_r);
            }
            else if (tta.op == "&&")
            {
                //writeln("#3.0 &&, p_op=", p_op);
                string t_op_l;
                string t_op_r;
                string token_L;

                string tta_R;
                if (tta.R !is null)
                    tta_R = _transform_vql_to_xapian(ctx, tta.R, tta.op, token_L, t_op_r, query_r, key2slot, rd, level + 1, qp);

                if (t_op_r !is null)
                    op = t_op_r;

                string tta_L;
                if (tta.L !is null)
                    tta_L = _transform_vql_to_xapian(ctx, tta.L, tta.op, dummy, t_op_l, query_l, key2slot, ld, level + 1, qp);

                if (t_op_l !is null)
                    op = t_op_l;

                //writeln("@p && token_L=", token_L);
                //writeln("@p query_l=", get_query_description(query_l));
                //writeln("@p query_r=", get_query_description(query_r));


                if (token_L !is null && tta_L !is null)
                {
                    //writeln("@p #E0.1 &&");
                    // это range
                    //writeln("@p token_L=", token_L);
                    //writeln("@p tta_R=", tta_R);
                    //writeln("@p tta_L=", tta_L);
                    //writeln("@p t_op_l=", t_op_l);
                    //writeln("@p t_op_r=", t_op_r);

                    double c_to, c_from;

                    if (t_op_r == ">")
                        c_from = rd;
                    if (t_op_r == "<")
                        c_to = rd;

                    if (t_op_l == ">")
                        c_from = ld;
                    if (t_op_l == "<")
                        c_to = ld;

                    //writeln("@p c_from=", c_from);
                    //writeln("@p c_to=", c_to);

                    int slot = key2slot.get(token_L, -1);

                    query_r = new_Query_range(xapian_op.OP_VALUE_RANGE, slot, c_from, c_to, &err);

                    if (query_l is null)
                    {
                        query   = query_r;
                        query_r = null;
                    }
                    else
                        query = query_l.add_right_query(xapian_op.OP_AND, query_r, &err);

                    if (query is null)
                        throw new XapianError(err, "parse_query '" ~ tta.toString() ~ "'");

                    if (query_r !is null)
                        destroy_Query(query_r);

                    if (query_l !is null)
                        destroy_Query(query_l);
                }
                else
                {
                    //writeln("#E0.2 &&");
                    if (query_r !is null)
                    {
                        //writeln("#E0.2 && query_l=", get_query_description(query_l));
                        //writeln("#E0.2 && query_r=", get_query_description(query_r));
                        if (query_l is null)
                        {
                            query   = query_r;
                            query_r = null;
                        }
                        else
                            query = query_l.add_right_query(xapian_op.OP_AND, query_r, &err);

                        if (query is null)
                            throw new XapianError(err, "parse_query '" ~ tta.toString() ~ "'");
                        //writeln("#3.1 && query=", get_query_description(query));
                    }
                    else
                    {
                        query   = query_l;
                        query_l = null;
                    }
                    if (query_r !is null)
                        destroy_Query(query_r);

                    if (query_l !is null)
                        destroy_Query(query_l);
                }

                if (tta_R !is null && tta_L is null)
                {
                    _rd = rd;
                    return tta_R;
                }

                if (tta_L !is null && tta_R is null)
                {
                    _rd = ld;
                    return tta_L;
                }
            }
            else if (tta.op == "||")
            {
                if (tta.R !is null)
                    _transform_vql_to_xapian(ctx, tta.R, tta.op, dummy, dummy, query_r, key2slot, rd, level + 1, qp);

                if (tta.L !is null)
                    _transform_vql_to_xapian(ctx, tta.L, tta.op, dummy, dummy, query_l, key2slot, ld, level + 1, qp);

                if (query_l !is null)
                    query = query_l.add_right_query(xapian_op.OP_OR, query_r, &err);

                if (query is null)
                    throw new XapianError(err, "parse_query '" ~ tta.toString() ~ "'");

                if (query_r !is null)
                    destroy_Query(query_r);

                if (query_l !is null)
                    destroy_Query(query_l);
            }
            else
            {
//		query = new_Query_equal (xapian_op.OP_FILTER, int slot, cast(char*)tta.op, tta.op.length);
                //writeln("#5 tta.op=", tta.op);
                return tta.op;
            }
            //log.trace ("%d return, tta=%s, query=%s", level, tta, get_query_description(query));
            return null;
        }
        catch (Throwable tr)
        {
            log.trace("EX: _transform_vql_to_xapian, tta=[%s], err=[%s]", tta, tr.info);
            throw tr;
            //return null;
        }
    }

    public SearchResult exec_xapian_query_and_queue_authorize(Ticket *ticket,
                                                              XapianEnquire xapian_enquire,
                                                              int from,
                                                              int top,
                                                              int limit,
                                                              void delegate(string uri) add_out_element,
                                                              Context context,
                                                              void delegate(string uri) prepare_element_event
                                                              )
    {
        SearchResult sr;

        if (top == 0)
            top = 10000;

        if (limit == 0)
            limit = 10000;

        int       read_count = 0;
        StopWatch sw;

        if (trace_msg[ 200 ] == 1)
            sw.start;

        byte err;

        if (ticket is null)
        {
            log.trace("exec_xapian_query_and_queue_authorize:ticket is null");
            sr.result_code = ResultCode.Ticket_not_found;
            return sr;
        }

        //writeln (cast(void*)xapian_enquire, " count_authorize=", count_authorize);
        if (prepare_element_event !is null)
            prepare_element_event("");

        XapianMSet matches = xapian_enquire.get_mset(from, limit, &err);
        if (err < 0)
        {
            log.trace("exec_xapian_query_and_queue_authorize:get_mset, err=(%d)", err);
            sr.result_code = ResultCode.Internal_Server_Error;
//            sr.err         = err;
            return sr;
        }

        int processed = 0;

        if (matches !is null)
        {
            sr.estimated = matches.get_matches_estimated(&err);

	        if (prepare_element_event !is null)
	            prepare_element_event("");

            XapianMSetIterator it = matches.iterator(&err);

            bool               acl_db_reopen = true;

            while (it.is_next(&err) == true)
            {            	
                if (err < 0)
                {
                    sr.result_code = ResultCode.Internal_Server_Error;
                    log.trace("exec_xapian_query_and_queue_authorize:mset:is_next, err=(%d)", err);
//                    sr.err = err;
                    return sr;
                }

                char *data_str;
                uint *data_len;
                it.get_document_data(&data_str, &data_len, &err);
                if (err < 0)
                {
                    sr.result_code = ResultCode.Internal_Server_Error;
                    log.trace("exec_xapian_query_and_queue_authorize:get_document_data, err=(%d)", err);
//                    sr.err = err;
                    return sr;
                }

                processed++;

                string subject_id = data_str[ 0..*data_len ].idup;

                if (prepare_element_event !is null)
                    prepare_element_event(subject_id);

                if (trace_msg[ 201 ] == 1)
                    log.trace("found subject_id:[%s]", subject_id);

                if (context.authorize(subject_id, ticket, Access.can_read, acl_db_reopen))
                {
                    //log.trace("found subject_id:[%s] authorized", subject_id);

                    add_out_element(subject_id);
                    read_count++;
                    if (read_count >= top)
                        break;
                }
                else
                {
                    log.trace("subject_id:[%s] not authorized, ticket=[%s], user_uri=[%s]", subject_id, ticket.id, ticket.user_uri);
                }

                acl_db_reopen = false;

                it.next(&err);
            }

            if (trace_msg[ 200 ] == 1)
            {
                sw.stop();
                long t = cast(long)sw.peek().usecs;
                log.trace("authorized:%d, total time execute query: %s µs", read_count, text(t));
            }

            destroy_MSetIterator(it);
            destroy_MSet(matches);
        }

        sr.processed   = processed;
        sr.count       = read_count;
        sr.result_code = ResultCode.OK;
        sr.cursor      = from + processed;

        return sr;
    }

    string get_query_description(XapianQuery query)
    {
        if (query !is null)
        {
            char *descr_str;
            uint *descr_len;
            query.get_description(&descr_str, &descr_len, &err);
            if (descr_len !is null && *descr_len > 0)
            {
                string str = cast(immutable)descr_str[ 0..*descr_len ].dup;
                writeln("QUERY:", str);
                return str;
            }
            else
                return "no content";
        }
        return "NULL";
    }
}