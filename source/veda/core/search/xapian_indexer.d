/**
 * XAPIAN indexer thread
 */

module search.xapian_indexer;

private import std.concurrency, std.outbuffer, std.datetime, std.conv, std.typecons, std.stdio, std.string, std.file, std.algorithm;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.type;
private import bind.xapian_d_header;
private import util.utils, util.cbor, veda.core.util.cbor8individual, util.logger;
private import veda.onto.onto, veda.onto.resource, onto.lang, veda.onto.individual;
private import veda.core.define, veda.core.know_predicates, veda.core.context, veda.core.log_msg, veda.core.thread_context;
private import storage.lmdb_storage;
private import search.vel, search.xapian_vql, search.indexer_property;

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


public void xapian_thread_context(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;

    long                         last_update_time;

//    writeln("SPAWN: xapian_thread_io");
    last_update_time = Clock.currTime().stdTime();

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });
    while (true)
    {
        receive(
                (CMD cmd, CNAME cname, string _key2slot_str)
                {
                    if (cmd == CMD.PUT)
                    {
                        if (cname == CNAME.LAST_UPDATE_TIME)
                        {
                            last_update_time = Clock.currTime().stdTime() / 10000;
                        }
                    }
                },
                (CMD cmd, CNAME cname, Tid tid_sender)
                {
                    if (cmd == CMD.GET)
                    {
                        if (cname == CNAME.LAST_UPDATE_TIME)
                        {
                            //writeln ("GET:\n", last_update_time, ");
                            send(tid_sender, last_update_time);
                        }
                    }
                }, (Variant v) { writeln(thread_name, "::xapian_thread_context::Received some other type.", v); });
    }
}

private void store__key2slot(ref int[ string ] key2slot, Tid tid_subject_manager)
{
//	writeln ("#1 store__key2slot");
    string data = serialize_key2slot(key2slot);

    send(tid_subject_manager, CMD.PUT_KEY2SLOT, xapian_metadata_doc_id, data);
}

private int[ string ] read_key2slot(Tid tid_subject_manager)
{
    int[ string ] key2slot;

    send(tid_subject_manager, CMD.FIND, xapian_metadata_doc_id, thisTid);
    receive((string key, string data, Tid tid)
            {
//    writeln ("@KEY@SLOT=", data);
                key2slot = deserialize_key2slot(data);
            });

//    writeln("slot size=", key2slot.length);
    return key2slot;
}

private void printTid(string tag)
{
    writefln("%s: %s, address: %s", tag, thisTid, &thisTid);
}


private class IndexerContext
{
    Context                context;

    IndexerProperty        iproperty;

    XapianWritableDatabase indexer_base_db;
    XapianWritableDatabase indexer_system_db;
    XapianWritableDatabase indexer_deleted_db;

    XapianTermGenerator    indexer;
    string                 lang = "russian";

    int[ string ] key2slot;

    long   counter                         = 0;
    long   last_counter_after_timed_commit = 0;

    ulong  last_size_key2slot = 0;

    Tid    tid_subject_manager;
    Tid    tid_acl_manager;
    Tid    key2slot_accumulator;
    string thread_name;

    Ticket *ticket;

    void reload_index_schema()
    {
        if (iproperty !is null)
        {
            //writeln ("@@@1 RELOAD INDEX PROPERIES");
            iproperty.load(true);
            //writeln ("@@@2 iproperty=", iproperty);
        }
    }

    void index_msg(string msg, bool is_deleted, long op_id)
    {
        Individual indv;

        try
        {
            if (cbor2individual(&indv, msg) < 0)
            {
                log.trace("!ERR:invalid individual:[%s]", msg);
                return;
            }

            if (iproperty is null)
            {
                if (context is null)
                    context = new PThreadContext(node_id, thread_name, P_MODULE.fulltext_indexer);
                iproperty = new IndexerProperty(context);
            }

            iproperty.load(false);

            //writeln("prepare msg counter:", counter, ", subject:", ss.subject);

            if (indv.uri !is null && indv.resources.length > 0)
            {
                string actualVersion = indv.getFirstLiteral("v-s:actualVersion");

                if (is_deleted == false && actualVersion !is null && actualVersion != indv.uri)
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
                                            //writeln (tab, "@@@5.0 value = ", value);
                                            // ссылка на наследуемый индекс, переходим вниз
                                            Individual inhr_idx = iproperty.get_index(value.uri);
                                            //writeln (tab, "@@@5.1 inhr_idx = ", inhr_idx);
                                            if (inhr_idx != Individual.init)
                                            {
                                                Resources forProperties =
                                                    inhr_idx.getResources("vdi:forProperty");
                                                if (forProperties != Resources.init)
                                                {
                                                    foreach (forProperty; forProperties)
                                                    {
                                                        //writeln (tab, "@@@5.2 forProperty = ", forProperty);
                                                        Resources links =
                                                            inner_indv.getResources(forProperty.uri);
                                                        //writeln (tab, "@@@5.3 links = ", links);
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

                                                                if (rc.type == DataType.String)
                                                                {
                                                                    index_string(ln ~ "." ~ indexed_field.uri, rc);
                                                                }
                                                                else if (rc.type == DataType.Integer)
                                                                {
                                                                    index_integer(ln ~ "." ~ indexed_field.uri, rc);
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
                    	index_boolean(predicate ~ ".isExists", Resource (true));
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
                        {
                            index_boolean(predicate, oo);
                        }
                        else if (oo.type == DataType.Integer)
                        {
                            index_integer(predicate, oo);
                        }
                        else if (oo.type == DataType.Decimal)
                        {
                            index_double(predicate, oo);
                        }
                        else if (oo.type == DataType.String)
                        {
                            index_string(predicate, oo);
                        }
                        else if (oo.type == DataType.Uri)
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
                        else if (oo.type == DataType.Datetime)
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
                        store__key2slot(key2slot, tid_subject_manager);

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

        set_count_indexed(counter);
        //log.trace("@FT:commit=%d", counter);
    }
}

string node_id;

void xapian_indexer(string thread_name, string _node_id)
{
    node_id = _node_id;
    scope (exit)
    {
        log.trace("ERR! indexer thread dead (exit)");
    }

    IndexerContext ictx = new IndexerContext;
    ictx.thread_name = thread_name;
    // //////////////////////////////////

    core.thread.Thread.getThis().name = thread_name;

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

    try
    {
        mkdir(db_path_base);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(db_path_system);
    }
    catch (Exception ex)
    {
    }

    try
    {
        mkdir(db_path_deleted);
    }
    catch (Exception ex)
    {
    }
    // /////////// XAPIAN INDEXER ///////////////////////////
    XapianStem stemmer = new_Stem(cast(char *)ictx.lang, cast(uint)ictx.lang.length, &err);

    string     dummy;
    double     d_dummy;

    //bool       is_exist_db = exists(xapian_search_db_path);

    ictx.indexer_base_db = new_WritableDatabase(db_path_base.ptr, cast(uint)db_path_base.length, DB_CREATE_OR_OPEN, xapian_db_type, &err);
    if (err == 0)
    {
        ictx.indexer_system_db = new_WritableDatabase(db_path_system.ptr, cast(uint)db_path_system.length, DB_CREATE_OR_OPEN,
                                                      xapian_db_type, &err);
        if (err != 0)
        {
            writeln("!!!!!!! Err in new_WritableDatabase, err=", err);

            receive((Tid tid_response_reciever)
                    {
                        send(tid_response_reciever, false);
                    });
            return;
        }

        ictx.indexer_deleted_db = new_WritableDatabase(db_path_deleted.ptr, cast(uint)db_path_deleted.length, DB_CREATE_OR_OPEN,
                                                       xapian_db_type, &err);
        if (err != 0)
        {
            writeln("!!!!!!! Err in new_WritableDatabase, err=", err);

            receive((Tid tid_response_reciever)
                    {
                        send(tid_response_reciever, false);
                    });
            return;
        }
    }

    ictx.indexer = new_TermGenerator(&err);
    ictx.indexer.set_stemmer(stemmer, &err);

    ictx.commit_all_db();

    //ictx.xapian_enquire = ictx.indexer_base_db.new_Enquire(&err);
    //ictx.xapian_qp      = new_QueryParser(&err);
    //ictx.xapian_qp.set_stemmer(stemmer, &err);
    //ictx.xapian_qp.set_database(ictx.indexer_base_db, &err);


    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    while (true)
    {
        try
        {
            receive(
                    (CMD cmd, P_MODULE module_id, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.SET)
                        {
                            if (module_id == P_MODULE.subject_manager)
                            {
                                ictx.tid_subject_manager = tid_response_reciever;

                                if (ictx.key2slot.length == 0 && ictx.tid_subject_manager != Tid.init)
                                    ictx.key2slot = read_key2slot(ictx.tid_subject_manager);
                            }
                            else if (module_id == P_MODULE.acl_manager)
                            {
                                ictx.tid_acl_manager = tid_response_reciever;
                            }
                            else if (module_id == P_MODULE.xapian_thread_context)
                            {
                                ictx.key2slot_accumulator = tid_response_reciever;
                            }
//                        return;
                        }
                    },
                    (CMD cmd, string msg, Tid tid_response_reciever)
                    {
                        if (ictx.key2slot.length - ictx.last_size_key2slot > 0)
                        {
                            store__key2slot(ictx.key2slot, ictx.tid_subject_manager);
                            if (trace_msg[ 210 ] == 1)
                                log.trace("store__key2slot #1");
                            ictx.last_size_key2slot = ictx.key2slot.length;
                        }

                        ictx.commit_all_db();

                        if (cmd == CMD.BACKUP)
                        {
                            string new_path_backup_xapian = dbs_backup ~ "/" ~ msg ~ "/" ~ get_xapiab_db_path("base");
                            try
                            {
                                mkdir(new_path_backup_xapian);
                            }
                            catch (Exception ex)
                            {
                                writeln("ex!", ex.msg);
                            }

                            try
                            {
                                auto oFiles = dirEntries(get_xapiab_db_path("base"), "*.*", SpanMode.depth);
                                foreach (o; oFiles)
                                {
                                    string new_path;
                                    string[] tt = o.name.split("/");
                                    if (tt.length > 1)
                                        new_path = tt[ $ - 1 ];
                                    else
                                        new_path = o.name;

                                    new_path = new_path_backup_xapian ~ "/" ~ new_path;

                                    //writeln ("COPY TO:", new_path);
                                    copy(o.name, new_path);
                                    //writeln ("OK");
                                }
                            }
                            catch (Exception ex)
                            {
                                writeln("ex!", ex.msg);
                                send(tid_response_reciever, "");
                            }
                        }

                        send(tid_response_reciever, msg);
                    },
                    (CMD cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.RELOAD)
                        {
                            ictx.reload_index_schema();
                            send(tid_response_reciever, true);
                        }
                        else
                        {
                            //if (cmd == CMD.NOP)
                            //	log.trace ("@indexer: NOP #1");

                            // если ожидают окончания операции для indexer, то вероятнее всего собираются сразу-же читать из поиска
                            // следовательно нужно сделать коммит
                            if (ictx.key2slot.length - ictx.last_size_key2slot > 0)
                            {
                                store__key2slot(ictx.key2slot, ictx.tid_subject_manager);
                                if (trace_msg[ 210 ] == 1)
                                    log.trace("store__key2slot #2");
                                ictx.last_size_key2slot = ictx.key2slot.length;
                            }
                            ictx.commit_all_db();

                            ictx.last_counter_after_timed_commit = ictx.counter;

                            //if (cmd == CMD.NOP)
                            //	log.trace ("@indexer: NOP #2");

                            if (cmd == CMD.NOP)
                                send(tid_response_reciever, true);
                            else
                                send(tid_response_reciever, false);
                        }
                    },
                    (CMD cmd, string msg)
                    {
                        //writeln (cast(void*)indexer_base_db, " @1 cmd=", cmd, ", msg: ", msg);
                        if (cmd == CMD.COMMIT)
                        {
                            //writeln ("@@ XAPIAN:COMMIT");

                            if (ictx.counter - ictx.last_counter_after_timed_commit > 0)
                            {
                                if (trace_msg[ 210 ] == 1)
                                    log.trace("counter: %d, timer: commit index..", ictx.counter);
                                if (ictx.key2slot.length - ictx.last_size_key2slot > 0)
                                {
                                    store__key2slot(ictx.key2slot, ictx.tid_subject_manager);
                                    if (trace_msg[ 210 ] == 1)
                                        log.trace("store__key2slot");
                                    ictx.last_size_key2slot = ictx.key2slot.length;
                                }

                                ictx.commit_all_db();

                                //indexer_base_db.close (&err);
                                //indexer_base_db = new_WritableDatabase(xapian_search_db_path.ptr, xapian_search_db_path.length, DB_CREATE_OR_OPEN, &err);
                                ictx.last_counter_after_timed_commit = ictx.counter;
                                send(ictx.key2slot_accumulator, CMD.PUT, CNAME.LAST_UPDATE_TIME, "");
                                //core.memory.GC.collect ();
                                //writeln ("GC COLLECT");
                            }
                        }
                    },
                    (CMD cmd, string msg, long op_id)
                    {
                        //writeln ("@@XAPIAN INDEXER START op_id=", op_id);
                        if (cmd == CMD.PUT)
                        {
                            ictx.index_msg(msg, false, op_id);
                        }
                        else if (cmd == CMD.DELETE)
                        {
                            ictx.index_msg(msg, true, op_id);
                        }
                        //writeln ("@@XAPIAN INDEXER END op_id=", op_id);
                    },
                    (CMD cmd, int arg, bool arg2)
                    {
                        if (cmd == CMD.SET_TRACE)
                            set_trace(arg, arg2);
                    },
                    (Variant v) { writeln(thread_name, "::xapian_indexer::Received some other type.", v); printPrettyTrace(stderr); });
        }
        catch (Exception ex)
        {
            log.trace("^^^^indexer# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
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
//        send (key2slot_accumulator, PUT, data);
    }

    return slot;
}


