/**
 * XAPIAN indexer thread
 */

module veda.ft_indexer.xapian_indexer;

private import std.concurrency, std.outbuffer, std.datetime, std.conv, std.typecons, std.stdio, std.string, std.file, std.algorithm;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.common.type;
private import veda.bind.xapian_d_header;
private import veda.core.util.utils, veda.common.logger;
private import veda.onto.onto, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.core.storage.lmdb_storage;
private import veda.core.common.define, veda.core.common.know_predicates, veda.core.common.context, veda.core.common.log_msg,
               veda.core.impl.thread_context;
private import veda.core.search.vel, veda.core.search.xapian_vql, veda.core.search.indexer_property;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-" ~ process_name, "log", "SEARCH");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

protected byte err;

public class IndexerContext
{
    Context                context;

    IndexerProperty        iproperty;

    XapianWritableDatabase indexer_base_db;
    XapianWritableDatabase indexer_system_db;
    XapianWritableDatabase indexer_deleted_db;

    XapianTermGenerator    indexer;
    string                 lang = "russian";

    int[ string ] key2slot;
    File   *ff_key2slot_w = null;

    long   counter                         = 0;
    long   last_counter_after_timed_commit = 0;

    ulong  last_size_key2slot = 0;

    string thread_name;

    Ticket *ticket;

    void close()
    {
        if (indexer_base_db !is null)
            indexer_base_db.close(&err);
        if (indexer_system_db !is null)
            indexer_system_db.close(&err);
        if (indexer_deleted_db !is null)
            indexer_deleted_db.close(&err);
    }

    bool init(Ticket *_ticket, Context _context)
    {
        context = _context;
        ticket  = _ticket;
        string file_name_key2slot = xapian_info_path ~ "/key2slot";

        if (exists(file_name_key2slot) == false)
            ff_key2slot_w = new File(file_name_key2slot, "w");
        else
        {
            ff_key2slot_w = new File(file_name_key2slot, "r+");

            ff_key2slot_w.seek(0);
            auto buf = ff_key2slot_w.rawRead(new char[ 100 * 1024 ]);

            //writefln("@indexer:init:data [%s]", cast(string)buf);
            ResultCode rc;
            key2slot = deserialize_key2slot(cast(string)buf, rc);
            //writeln("@indexer:init:key2slot", key2slot);
        }

        // attempt to create a path
        try
        {
            mkdir("data");
        }
        catch (Exception ex)
        {
        }

        string db_path_base    = get_xapiab_db_path("base");
        string db_path_system  = get_xapiab_db_path("system");
        string db_path_deleted = get_xapiab_db_path("deleted");

        bool   need_all_reindex = false;

        byte   count_created_db_folder = 0;
        try
        {
            mkdir(db_path_base);
            count_created_db_folder++;
        } catch (Exception ex) {}

        try
        {
            mkdir(db_path_system);
            count_created_db_folder++;
        } catch (Exception ex) {}

        try
        {
            mkdir(db_path_deleted);
            count_created_db_folder++;
        }
        catch (Exception ex) {}

        // check in key2slot for properties
        string[] props = context.get_onto().get_properies();
        foreach (prop; props)
        {
            //log.trace ("prop=%s", prop);
            get_slot_and_set_if_not_found(prop, key2slot);
        }


        // /////////// XAPIAN INDEXER ///////////////////////////
        XapianStem stemmer = new_Stem(cast(char *)this.lang, cast(uint)this.lang.length, &err);

        string     dummy;
        double     d_dummy;

        //bool       is_exist_db = exists(xapian_search_db_path);

        this.indexer_base_db = new_WritableDatabase(db_path_base.ptr, cast(uint)db_path_base.length, DB_CREATE_OR_OPEN, xapian_db_type, &err);
        if (err == 0)
        {
            this.indexer_system_db = new_WritableDatabase(db_path_system.ptr, cast(uint)db_path_system.length, DB_CREATE_OR_OPEN,
                                                          xapian_db_type, &err);
            if (err != 0)
            {
                log.trace("ERR! in new_WritableDatabase[%s], err=%s", db_path_system, get_xapian_err_msg(err));
                return false;
            }

            this.indexer_deleted_db = new_WritableDatabase(db_path_deleted.ptr, cast(uint)db_path_deleted.length, DB_CREATE_OR_OPEN,
                                                           xapian_db_type, &err);
            if (err != 0)
            {
                log.trace("ERR! in new_WritableDatabase[%s], err=%s", db_path_deleted, get_xapian_err_msg(err));
                return false;
            }
        }
        else
        {
            log.trace("ERR! in new_WritableDatabase[%s], err=%s", db_path_base, get_xapian_err_msg(err));
            return false;
        }

        this.indexer = new_TermGenerator(&err);
        this.indexer.set_stemmer(stemmer, &err);

        this.commit_all_db();

        if (count_created_db_folder != 0)
        {
            log.trace("index is empty or not completed");

            if (need_all_reindex == true)
            {
                log.trace("it does not correspond to the index database, need reindexes");
            }
        }
        return true;
    }

    void reload_index_schema()
    {
        if (iproperty !is null)
            iproperty.load(true);
    }

    void index_msg(ref Individual indv, ref Individual prev_indv, INDV_OP cmd, long op_id, Context context)
    {
        //writeln("@ft index, indv.uri=", indv.uri);
        bool is_deleted;

        if (cmd == INDV_OP.REMOVE)
            is_deleted = true;

        if (is_deleted == false && indv.isExists(veda_schema__deleted, true) == true)
            is_deleted = true;

        try
        {
            if (iproperty is null)
                iproperty = new IndexerProperty(context);

            iproperty.load(false);

            if (indv.uri !is null && indv.resources.length > 0)
            {
                string isDraftOf            = indv.getFirstLiteral("v-s:isDraftOf");
                string actualVersion        = indv.getFirstLiteral("v-s:actualVersion");
                string previousVersion_prev = prev_indv.getFirstLiteral("v-s:previousVersion");
                string previousVersion_new  = indv.getFirstLiteral("v-s:previousVersion");

                if (isDraftOf !is null)
                    return;

                if (is_deleted == false && (actualVersion !is null && actualVersion != indv.uri ||
                                            (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)))
                    return;

                OutBuffer      all_text = new OutBuffer();
                XapianDocument doc      = new_Document(&err);
                indexer.set_document(doc, &err);

                if (trace_msg[ 220 ] == 1)
                    log.trace("index document:[%s]", indv.uri);

                Resources types = indv.getResources(rdf__type);

                // используем информацию о типе, для определения, в какой базе следует проводить индексацию
                string dbname = "base";
                foreach (_type; types)
                {
                    if (_type.uri == "vdi:ClassIndex")
                    {
                        iproperty.add_schema_data(indv);
                    }

                    dbname = iproperty.get_dbname_of_class(_type.uri);
                    if (dbname != "base")
                        break;
                }

                if (dbname == "not-indexed")
                    return;

                foreach (predicate, resources; indv.resources)
                {
                    string prefix;
                    //int slot = get_slot_and_set_if_not_found(predicate, key2slot);

                    //all_text.write('|');

                    string type = "xsd__string";

                    string p_text_ru = "";
                    string p_text_en = "";

                    void index_double(string predicate, Resource oo)
                    {
                        int slot_L1 = get_slot_and_set_if_not_found(predicate, key2slot);

                        prefix = "X" ~ text(slot_L1) ~ "X";

                        decimal dd     = oo.get!decimal();
                        double  l_data = dd.toDouble();
                        doc.add_value(slot_L1, l_data, &err);
                        prefix = "X" ~ text(slot_L1) ~ "D";
                        indexer.index_data(l_data, prefix.ptr, prefix.length, &err);

                        if (trace_msg[ 220 ] == 1)
                            log.trace("index [DataType.Double] :[%s], prefix=%s[%s]", text(l_data), prefix,
                                      predicate);
                    }

                    void index_boolean(string predicate, Resource oo)
                    {
                        int    slot_L1 = get_slot_and_set_if_not_found(predicate, key2slot);

                        string data = "F";

                        prefix = "X" ~ text(slot_L1) ~ "D";

                        if (oo.get!bool() == true)
                            data = "T";

                        indexer.index_text(data.ptr, data.length, prefix.ptr, prefix.length, &err);
                        doc.add_value(slot_L1, data.ptr, data.length, &err);

                        if (trace_msg[ 220 ] == 1)
                            log.trace("index [DataType.Boolean] :[%s], prefix=%s[%s]", data, prefix,
                                      predicate);
                    }

                    void index_integer(string predicate, Resource oo)
                    {
                        int slot_L1 = get_slot_and_set_if_not_found(predicate, key2slot);

                        prefix = "X" ~ text(slot_L1) ~ "X";

                        double l_data = cast(double)(oo.get!long ());
                        doc.add_value(slot_L1, l_data, &err);
                        prefix = "X" ~ text(slot_L1) ~ "D";
                        indexer.index_data(l_data, prefix.ptr, prefix.length, &err);

                        if (trace_msg[ 220 ] == 1)
                            log.trace("index [DataType.Integer] :[%s], prefix=%s[%s]", text(l_data), prefix,
                                      predicate);
                    }

                    void index_date(string predicate, Resource oo)
                    {
                        int slot_L1 = get_slot_and_set_if_not_found(predicate, key2slot);

                        prefix = "X" ~ text(slot_L1) ~ "X";

                        long   l_data   = oo.get!long ();
                        long   std_time = unixTimeToStdTime(l_data);
                        string data     = SysTime(std_time).toISOString();
                        indexer.index_text(data.ptr, data.length, prefix.ptr, prefix.length, &err);

                        doc.add_value(slot_L1, l_data, &err);
                        //   slot_L1 = get_slot_and_set_if_not_found(predicate ~ ".Datetime", key2slot);
                        prefix = "X" ~ text(slot_L1) ~ "D";
                        indexer.index_data(l_data, prefix.ptr, prefix.length, &err);

                        if (trace_msg[ 220 ] == 1)
                            log.trace("index [DataType.Datetime] :[%s][%s], prefix=%s[%s]", data, text(l_data), prefix,
                                      predicate);
                    }

                    void index_uri(string predicate, Resource oo)
                    {
                        if (oo.literal !is null)
                        {
                            int slot_L1 = get_slot_and_set_if_not_found(predicate, key2slot);
                            prefix = "X" ~ text(slot_L1) ~ "X";

                            string data = to_lower_and_replace_delimeters(oo.literal);

                            if (trace_msg[ 220 ] == 1)
                                log.trace("index [DataType.Uri] :[%s], prefix=%s[%s]", data, prefix, predicate);
                            indexer.index_text(data.ptr, data.length, prefix.ptr, prefix.length, &err);

                            doc.add_value(slot_L1, oo.literal.ptr, oo.literal.length, &err);

                            all_text.write(data);
                            all_text.write('|');
                        }
                    }

                    void index_string(string predicate, Resource oo)
                    {
                        string data;

                        data = oo.literal;
                        if (data.length < 1)
                            return;

                        // if (resources.length > 1)
                        {
                            if (oo.lang == LANG.RU)
                                p_text_ru ~= oo.literal;
                            else if (oo.lang == LANG.EN)
                                p_text_en ~= oo.literal;
                        }

                        int slot_L1 = get_slot_and_set_if_not_found(predicate, key2slot);
                        prefix = "X" ~ text(slot_L1) ~ "X";

                        if (trace_msg[ 220 ] == 1)
                            log.trace("index_literal:[%s], lang=%s, prefix=%s[%s]", data, oo.lang, prefix,
                                      predicate);

                        indexer.index_text(data.ptr, data.length, prefix.ptr, prefix.length, &err);
                        doc.add_value(slot_L1, oo.literal.ptr, oo.literal.length, &err);

                        all_text.write(data);
                        all_text.write('|');
                    }

                    void index_string_for_first_wildcard(string predicate, Resource oo)
                    {
                        char[] data = oo.literal.dup;

                        if (data.length < 1)
                            return;

                        reverse(data);

                        int slot_L1 = get_slot_and_set_if_not_found(predicate ~ "#F", key2slot);
                        prefix = "X" ~ text(slot_L1) ~ "X";

                        if (trace_msg[ 220 ] == 1)
                            log.trace("revers index_literal:[%s], lang=%s, prefix=%s[%s]", data, oo.lang, prefix,
                                      predicate);

                        indexer.index_text(data.ptr, data.length, prefix.ptr, prefix.length, &err);
//                                        doc.add_value(slot_L1, oo.literal.ptr, oo.literal.length, &err);

                        //all_text.write(data);
                        //all_text.write('|');
                    }

                    void prepare_index(ref Individual idx, Resource rs, string ln, int level = 0)
                    {
                        if (rs.type == DataType.String)
                        {
                            Resources indexed_field_as_fwildcardZ =
                                idx.getResources("vdi:indexed_field_as_fwildcard");

                            if (indexed_field_as_fwildcardZ != Resources.init)
                            {
                                foreach (indexed_field_as_fwildcard; indexed_field_as_fwildcardZ)
                                {
                                    //writeln("indexed_field_as_fwildcard = ", indexed_field_as_fwildcard, ", rs=", rs.literal);

                                    index_string_for_first_wildcard(predicate, rs);
                                }
                            }
                        }
                        else if (rs.type == DataType.Uri)
                        {
                            try
                            {
                                // 1. считать индивид по ссылке
                                Individual inner_indv = context.get_individual(ticket, rs.uri);

                                //string tab; for (int i = 0; i < level; i++)
                                //    tab ~= "	";

                                //writeln (tab);
                                //writeln (tab, "@inner_indv = ", inner_indv);
                                //writeln (tab, "@idx = ", idx);
                                foreach (predicate, values; idx.resources)
                                {
                                    //writeln (tab, "@@@5 predicate = ", predicate);
                                    if (predicate == "vdi:inherited_index")
                                    {
                                        foreach (value; values)
                                        {
                                            // ссылка на наследуемый индекс, переходим вниз
                                            Individual inhr_idx = iproperty.get_index(value.uri);

                                            if (trace_msg[ 220 ] == 1)
                                                log.trace("[%s]ссылка на наследуемый индекс, переходим вниз по иерархии индекса [%s]", value,
                                                          inhr_idx);

                                            if (inhr_idx != Individual.init)
                                            {
                                                Resources forProperties =
                                                    inhr_idx.getResources("vdi:forProperty");
                                                if (forProperties != Resources.init)
                                                {
                                                    foreach (forProperty; forProperties)
                                                    {
                                                        Resources links =
                                                            inner_indv.getResources(forProperty.uri);

                                                        if (trace_msg[ 220 ] == 1)
                                                            log.trace("forProperty=[%s], links=[%s]", forProperty, links);

                                                        foreach (link; links)
                                                        {
                                                            prepare_index(inhr_idx, link,
                                                                          ln ~ "." ~ forProperty.uri,
                                                                          level + 1);
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    // в этом индексе не указанно на какое свойство будет индексация,
                                                    // значит берем поля указанные vdi:indexed_field в текущем индивиде
                                                    Resources indexed_fields =
                                                        inhr_idx.getResources("vdi:indexed_field");
                                                    if (indexed_fields != Resources.init)
                                                    {
                                                        foreach (indexed_field; indexed_fields)
                                                        {
                                                            Resources rrc =
                                                                inner_indv.getResources(indexed_field.uri);
                                                            foreach (rc; rrc)
                                                            {
                                                                if (trace_msg[ 213 ] == 1)
                                                                    log.trace("index %s = %s ", ln ~ "." ~ indexed_field.uri,
                                                                              rc);

                                                                if (rc.type == DataType.Uri)
                                                                {
                                                                    index_uri(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                                if (rc.type == DataType.String)
                                                                {
                                                                    index_string(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                                else if (rc.type == DataType.Integer)
                                                                {
                                                                    index_integer(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                                else if (rc.type == DataType.Datetime)
                                                                {
                                                                    index_date(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                                else if (rc.type == DataType.Boolean)
                                                                {
                                                                    index_boolean(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                                else if (rc.type == DataType.Decimal)
                                                                {
                                                                    index_double(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                throw new Exception("prepare index:" ~ ex.msg);
                            }
                        }
                    }

                    if (resources.length > 0)
                    {
                        index_boolean(predicate ~ ".isExists", Resource(true));
                    }

                    foreach (oo; resources)
                    {
                        if (oo.literal !is null)
                        {
                            // если это относится к class_property__2__indiviual, следует обновить

                            if (predicate != rdf__type)
                            {
                                // используем информацию о типе
                                foreach (_type; types)
                                {
                                    // в онтологии найти для данного класса и для данного предиката
                                    // информацию об индексировании
                                    Individual idx = iproperty.get_index(_type.uri, predicate);
                                    if (idx != Individual.init)
                                    {
                                        //writeln("@@@A class= ", _type.uri, ", predicate=", predicate);

                                        //writeln("@@@A 1 _type.uri ~ predicate= ", _type.uri ~ predicate);
                                        //writeln("@@@A idx=", idx.uri);
                                        prepare_index(idx, oo, predicate);
                                    }
                                    else
                                    {
                                        idx = iproperty.get_index_of_property(predicate);

                                        if (idx != Individual.init)
                                        {
                                            //writeln("@@@B class= ", _type.uri, ", predicate=", predicate);

                                            // для предиката
                                            //writeln("@@@B 3");
                                            //writeln("@@@B idx=", idx.uri);

                                            // индексируем по найденному idx
                                            prepare_index(idx, oo, predicate);
                                        }
                                    }
                                }
                            }
                        }

                        if (oo.type == DataType.Boolean)
                            index_boolean(predicate, oo);
                        else if (oo.type == DataType.Integer)
                            index_integer(predicate, oo);
                        else if (oo.type == DataType.Decimal)
                            index_double(predicate, oo);
                        else if (oo.type == DataType.String)
                            index_string(predicate, oo);
                        else if (oo.type == DataType.Uri)
                            index_uri(predicate, oo);
                        else if (oo.type == DataType.Datetime)
                            index_date(predicate, oo);
                    }

                    if (resources.length > 1)
                    {
                        if (p_text_ru.length > 0)
                        {
                            int slot_L1 = get_slot_and_set_if_not_found(predicate ~ "_ru", key2slot);
                            prefix = "X" ~ text(slot_L1) ~ "X";

                            indexer.index_text(p_text_ru.ptr, p_text_ru.length, prefix.ptr, prefix.length, &err);

                            if (trace_msg[ 220 ] == 1)
                                log.trace("index as ru text:[%s]", p_text_ru);

                            doc.add_value(slot_L1, p_text_ru.ptr, p_text_ru.length, &err);
                            //writeln ("slot:", slot_L1, ", value:", p_text_ru);
                        }

                        if (p_text_en.length > 0)
                        {
                            int slot_L1 = get_slot_and_set_if_not_found(predicate ~ "_en", key2slot);
                            prefix = "X" ~ text(slot_L1) ~ "X";

                            indexer.index_text(p_text_en.ptr, p_text_en.length, prefix.ptr, prefix.length, &err);

                            if (trace_msg[ 220 ] == 1)
                                log.trace("index as en text:[%s]", p_text_en);

                            doc.add_value(slot_L1, p_text_en.ptr, p_text_en.length, &err);
                            //writeln ("slot:", slot_L1, ", value:", p_text_en);
                        }
                    }

                    int slot_L1;

                    if (type == xsd__string)
                    {
                        bool sp = true;
                        foreach (oo; resources)
                        {
                            if (oo.type == DataType.String && (oo.lang == LANG.RU || oo.lang == LANG.NONE))
                            {
                                if (sp == true)
                                {
                                    slot_L1 = get_slot_and_set_if_not_found(predicate ~ ".text_ru", key2slot);
                                    prefix  = "X" ~ text(slot_L1) ~ "X";

                                    sp = false;
                                }

                                doc.add_value(slot_L1, oo.literal.ptr, oo.literal.length, &err);
                                indexer.index_text(oo.literal.ptr, oo.literal.length, prefix.ptr, prefix.length, &err);

                                if (trace_msg[ 220 ] == 1)
                                    log.trace("index as (ru or none) xsd:string [%s]", oo.literal);

                                all_text.write(oo.literal);
                                all_text.write('|');

                                //writeln ("slot:", slot_L1, ", value:", oo.literal);
                            }
                        }

                        sp = true;
                        foreach (oo; resources)
                        {
                            if (oo.type == DataType.String && oo.lang == LANG.EN)
                            {
                                if (sp == true)
                                {
                                    slot_L1 = get_slot_and_set_if_not_found(predicate ~ ".text_en", key2slot);
                                    prefix  = "X" ~ text(slot_L1) ~ "X";

                                    sp = false;
                                }

                                doc.add_value(slot_L1, oo.literal.ptr, oo.literal.length, &err);
                                indexer.index_text(oo.literal.ptr, oo.literal.length, prefix.ptr, prefix.length, &err);

                                if (trace_msg[ 220 ] == 1)
                                    log.trace("index as (en) xsd:string [%s]", oo.literal);

                                all_text.write(oo.literal);
                                all_text.write('|');
                                //writeln ("slot:", slot_L1, ", value:", oo.literal);
                            }
                        }
                    }
                    else if (type == xsd__decimal)
                    {
                        slot_L1 = get_slot_and_set_if_not_found(predicate ~ ".decimal", key2slot);
                        prefix  = "X" ~ text(slot_L1) ~ "X";

                        foreach (oo; resources)
                        {
                            if (oo.type == DataType.String)
                            {
                                double data = to!double (oo.literal);
                                doc.add_value(slot_L1, data, &err);
                                all_text.write(oo.literal);
                                all_text.write('|');

                                indexer.index_data(data, prefix.ptr, prefix.length, &err);
                            }
                        }
                    }
                    else if (type == xsd__dateTime)
                    {
                        slot_L1 = get_slot_and_set_if_not_found(predicate ~ ".dateTime", key2slot);
                        prefix  = "X" ~ text(slot_L1) ~ "X";

                        foreach (oo; resources)
                        {
                            if (oo.type == DataType.String)
                            {
                                long data = stringToTime(oo.literal);
                                doc.add_value(slot_L1, data, &err);
                                all_text.write(oo.literal);
                                all_text.write('|');

                                indexer.index_data(data, prefix.ptr, prefix.length, &err);
                            }
                        }
                    }
                    else
                    {
                        //writeln ("not type for:", pp.predicate);
                    }
                }

                string data = all_text.toString;
                //writeln("@index = ", data);

                indexer.index_text(data.ptr, data.length, &err);
                if (trace_msg[ 221 ] == 1)
                    log.trace("index all text [%s]", data);

                string uuid = "uid_" ~ to_lower_and_replace_delimeters(indv.uri);
                doc.add_boolean_term(uuid.ptr, uuid.length, &err);
                doc.set_data(indv.uri.ptr, indv.uri.length, &err);

                if (is_deleted)
                {
                    indexer_deleted_db.replace_document(uuid.ptr, uuid.length, doc, &err);
                    doc = new_Document(&err);
                    indexer.set_document(doc, &err);
                }

                if (dbname == "system")
                {
                    indexer_system_db.replace_document(uuid.ptr, uuid.length, doc, &err);
                }
                else
                {
                    indexer_base_db.replace_document(uuid.ptr, uuid.length, doc, &err);
                }

//            if (counter % 100 == 0)
//            {
//                if (trace_msg[ 211 ] == 1)
//                    log.trace("prepare msg counter:%d,slot size=%d", counter, key2slot.length);
//            }

                if (counter % 5000 == 0)
                {
                    if (trace_msg[ 212 ] == 1)
                        log.trace("commit index..");

                    if (key2slot.length > 0)
                        store__key2slot();

                    commit_all_db();
                }

                destroy_Document(doc);
            }
        } finally
        {
            if (trace_msg[ 221 ] == 1)
                log.trace("FT: end");
            //log.trace ("@FT:indexing=%d, uri=%s", counter, indv.uri);
            counter = op_id;
        }
    }

    void commit_all_db()
    {
        indexer_base_db.commit(&err);
        if (err != 0)
            log.trace("EX! FT:commit:base fail=%d", counter);

        indexer_system_db.commit(&err);
        if (err != 0)
            log.trace("EX! FT:commit:system fail=%d", counter);

        indexer_deleted_db.commit(&err);
        if (err != 0)
            log.trace("EX! FT:commit:deleted fail=%d", counter);

//        set_count_indexed(counter);
        //log.trace("@FT:commit=%d", counter);
    }

    private void store__key2slot()
    {
        //writeln("#1 store__key2slot");
        string hash;
        string data = serialize_key2slot(key2slot, hash);

        try
        {
            ff_key2slot_w.seek(0);
            ff_key2slot_w.write('"');
            ff_key2slot_w.write(hash);
            ff_key2slot_w.write("\",");
            ff_key2slot_w.write(data.length);
            ff_key2slot_w.write('\n');
            ff_key2slot_w.write(data);
            ff_key2slot_w.flush();
        }
        catch (Throwable tr)
        {
            log.trace("fail store__key2slot [%s] [%s]", data, tr.msg);
            return;
        }
    }

    private int get_slot_and_set_if_not_found(string field, ref int[ string ] key2slot)
    {
//	writeln ("get_slot:", field);
        int slot = key2slot.get(field, -1);

        if (slot == -1)
        {
            // create new slot
            slot              = cast(int)key2slot.length + 1;
            key2slot[ field ] = slot;
            store__key2slot();
            log.trace("create new slot %s=%d", field, slot);
        }

        return slot;
    }
}

string node_id;

/////////////////////////////////////////////////////////////
