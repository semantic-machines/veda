/**
 * загрузка индивидов в базу данных из *.ttl
 * генерация doc/onto
 */
module io.file_reader;

import core.stdc.stdio, core.stdc.errno, core.stdc.string, core.stdc.stdlib;
import std.conv, std.digest.ripemd, std.bigint, std.datetime, std.concurrency, std.json, std.file, std.outbuffer, std.string, std.path, std.utf,
       std.stdio : writeln;
import type;
import util.container, util.cbor, util.utils, util.logger, util.raptor2individual, util.cbor8individual;
import onto.individual, onto.resource;
import veda.core.context, veda.core.thread_context, veda.core.define, veda.core.know_predicates, veda.core.log_msg;

logger log;

static this()
{
    log = new logger("pacahon", "log", "file_reader");
}


BigInt[ string ] ontohashes_2_filename;

private void add_to_onto_hash(Individual *indv, string name, ref BigInt[ string ] accum)
{
    string content = individual2cbor(indv);

    ubyte[ 20 ] hash = ripemd160Of(content);
    BigInt msg_hash  = "0x" ~ toHexString(hash);
    BigInt prev_summ = accum.get(name, BigInt.init);
    accum[ name ] = prev_summ + msg_hash;
//    return toHex(summ_hash_this_db);
}

string path = "./ontology";

/// процесс отслеживающий появление новых файлов и добавление их содержимого в базу данных
void file_reader_thread(P_MODULE id, string node_id, int checktime)
{
    core.thread.Thread tr = core.thread.Thread.getThis();
    tr.name = std.conv.text(id);

    try
    {
        mkdir("ontology");
    }
    catch (Exception ex)
    {
    }

    ubyte[] out_data;

    Context context = new PThreadContext(node_id, "file_reader", id);
    processed(context);

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    core.thread.Thread.sleep(dur!("msecs")(1500));

    while (true)
    {
        processed(context);

        if (checktime > 0)
            core.thread.Thread.sleep(dur!("seconds")(checktime));
    }
}

SysTime[ string ] file_modification_time;

void processed(Context context)
{
    if (log is null)
        log = new logger("pacahon", "log", "file_reader");

    Set!string files_to_load;

    auto oFiles = dirEntries(path, SpanMode.depth);

    if (trace_msg[ 29 ] == 1)
        log.trace("load directory sequence");

    foreach (o; oFiles)
    {
        if (extension(o.name) == ".ttl")
        {
            string fname = o.name.dup;
            if ((fname in file_modification_time) !is null)
            {
                if (o.timeLastModified != file_modification_time[ fname ])
                {
                    if (trace_msg[ 29 ] == 1)
                        log.trace("look modifed file=%s", fname);

                    file_modification_time[ fname ] = o.timeLastModified;
                    files_to_load ~= fname;
                }
            }
            else
            {
                file_modification_time[ fname ] = o.timeLastModified;
                files_to_load ~= fname;

                if (trace_msg[ 29 ] == 1)
                    log.trace("look new file=%s", fname);
            }
        }
    }

    string[ string ] filename_2_prefix;
    string[] order_in_load = [ "vdi:", "v-a:", "vsrv:", "rdf:", "rdfs:", "owl:", "*", "td:" ];
    Individual *[ string ][ string ] individuals_2_filename;
    BigInt[ string ] cur_ontohashes_2_filename;
    bool[ string ] modifed_2_file;

    foreach (filename; files_to_load)
    {
        log.trace("prepare_file %s", filename);

        auto individuals = ttl2individuals(filename, context);

        foreach (indv; individuals)
        {
            add_to_onto_hash(indv, filename, cur_ontohashes_2_filename);
            if (indv.isExist(rdf__type, owl__Ontology))
            {
                filename_2_prefix[ indv.uri ] = filename;
            }
        }

        individuals_2_filename[ filename ] = individuals;
    }


    foreach (key, value; cur_ontohashes_2_filename)
    {
        BigInt prev_hash = ontohashes_2_filename.get(key, BigInt.init);
        if (prev_hash != value)
        {
            //writeln("onto changed=", key);
            ontohashes_2_filename[ key ] = value;
            modifed_2_file[ key ]        = true;
        }
    }

    //writeln("@@1 ontohashes_2_filename=", ontohashes_2_filename);
    //writeln("@@2 filename_2_prefix=", filename_2_prefix);
    //writeln("@@3 modifed_2_file=", modifed_2_file);

    // load index onto
    foreach (pos; order_in_load)
    {
//        log.trace("load directory sequence 2...pos=%s", pos);
        if (pos == "*")
        {
            foreach (ont; filename_2_prefix.keys)
            {
                bool ff = false;
                foreach (pos1; order_in_load)
                {
                    if (ont == pos1)
                    {
                        ff = true;
                        break;
                    }
                }
                if (ff == false)
                {
                    string filename = filename_2_prefix.get(ont, null);
                    if (filename !is null)
                    {
                        if (modifed_2_file.get(filename, false) == true)
                        {
                            auto rr = individuals_2_filename.get(filename, null);
                            prepare_list(rr.values, context, filename);
                        }
                    }
                }
            }
        }
        else
        {
            string filename = filename_2_prefix.get(pos, null);
            if (filename !is null)
            {
                if (modifed_2_file.get(filename, false) == true)
                {
                    if (pos == "td:")
                    {
                        Tid tid_condition_manager = context.getTid(P_MODULE.condition);
                        if (tid_condition_manager != Tid.init)
                        {
                            core.thread.Thread.sleep(dur!("seconds")(1));
                            send(tid_condition_manager, CMD.RELOAD, thisTid);
                            receive((bool res) {});
                        }
                    }

                    auto rr = individuals_2_filename.get(filename, null);
                    prepare_list(rr.values, context, filename);
                }
            }
        }
    }
}

import util.individual2html;

private void prepare_list(Individual *[] ss_list, Context context, string file_name)
{
    context.reopen_ro_subject_storage_db();
    context.reopen_ro_fulltext_indexer_db();

    long count_individuals = context.count_individuals();


    // 1. сравнивает owl:versionInfo с версией в хранилище, для всех rdf:type == owl:Ontology,
    //    запоминает несуществующие или отличающиеся версией, для последующей загрузки
    // 2. попутно находит системный аккаунт (veda)
    try
    {
        if (trace_msg[ 30 ] == 1)
            log.trace("ss_list.count=%d", ss_list.length);

        if (trace_msg[ 30 ] == 1)
            log.trace("prefix_map=%s", context.get_prefix_map);

        bool       is_load = false;

        string     prefix;
        string     i_uri;

        Individual *onto_info;

        foreach (ss; ss_list)
        {
            //log.trace("ss=%s", *ss);
            //if (ss.uri[ $ - 1 ] == '#')
            //    ss.uri.length = ss.uri.length - 1;

            //if (trace_msg[ 31 ] == 1)
//                log.trace("prepare uri=%s", ss.uri);

            i_uri  = ss.uri;
            prefix = context.get_prefix_map.get(ss.uri, null);

            if (prefix !is null)
            {
                if (trace_msg[ 31 ] == 1)
                    log.trace("found prefix=%s ss=%s", prefix, *ss);

                if (ss.isExist(rdf__type, owl__Ontology))
                {
                    onto_info = ss;
                    string version_onto = ss.getFirstLiteral(owl__versionInfo);
                    if (version_onto is null)
                    {
                        log.trace("%s, owl: versionInfo is not found, the file will be reloaded with every update.", prefix);
                        is_load = true;
                        break;
                    }
                    else
                    {
                        if (trace_msg[ 32 ] == 1)
                            log.trace("%s, readed version=%s", prefix, version_onto);

                        // проверить какая версия данной онтологии в хранилище
                        //writeln("look in storage[", ss.uri, "]");
                        Individual sss;

                        if (count_individuals > 0)
                            sss = context.get_individual(null, ss.uri);

                        if (trace_msg[ 33 ] == 1)
                            log.trace("look in storage=%s, found=%s", ss.uri, sss);

                        if (sss.getStatus() == ResultCode.OK)
                        {
                            Resources aaa = sss.resources.get(owl__versionInfo, Resources.init);
                            if (aaa != Resources.init)
                            {
                                if (aaa.anyExist(version_onto))
                                {
                                    //writeln("@ This version [", version_onto, "] onto[", prefix, "] already exist");
                                }
                                else
                                {
                                    //writeln("@ 1 This version [", version_onto, "] onto[", prefix, "] not exist in store");
                                    is_load = true;
                                    break;
                                }
                            }
                        }
                        else
                        {
                            is_load = true;
                            //writeln("@ 2 This version [", version_onto, "] onto[", prefix, "] not exist in store");
                            break;
                        }
                    }
                }
            }
        }

        if (is_load == true && (prefix is null || prefix.length < 2))
        {
            log.trace_log_and_console("prefix is empty:%s", i_uri);
            log.trace_log_and_console("prefix map:%s", context.get_prefix_map);
        }

        if (is_load)
        {
            string doc_filename = docs_onto_path ~ "/" ~ onto_info.uri[ 0..$ - 1 ] ~ ".html";

            try
            {
                remove(doc_filename);
            }
            catch (Exception ex)
            {
            }

            append(
                   doc_filename,
                   "<html><body><head><meta charset=\"utf-8\"/><link href=\"css/bootstrap.min.css\" rel=\"stylesheet\"/><style=\"padding: 0px 0px 30px;\"></head>\n");


            log.trace_log_and_console("Onto for load:[%s]", prefix);

            foreach (ss; ss_list)
            {
                if (ss.isExist(veda_schema__login, "veda"))
                {
                    //writeln("FOUND SYSTEM ACCOUNT = ", ss);
                    context.push_signal("43", ss.getFirstLiteral(veda_schema__password));
                }
                if (ss.isExist(rdf__type, owl__Ontology))
                {
                    prefix = context.get_prefix_map.get(ss.uri, null);
                    Resources ress = Resources.init;
                    ress ~= Resource(prefix);
                    ss.resources[ veda_schema__fullUrl ] = ress;
                }

                ss.addResource("rdfs:isDefinedBy", Resource(DataType.Uri, onto_info.uri));

                append(doc_filename, individual2html(ss));

                long pos_path_delimiter = indexOf(ss.uri, '/');

                if (pos_path_delimiter < 0)
                {
                    long pos = indexOf(ss.uri, ':');
                    if (pos >= 0)
                    {
                        prefix = ss.uri[ 0..pos + 1 ];

                        //if (for_load.get(prefix, false) == true)
                        {
                            Individual indv_in_storage = context.get_individual(null, ss.uri);
                            bool       apply           = false;
                            if (indv_in_storage.getStatus() == ResultCode.OK)
                            {
                                bool is_type_indv_in_storage = ("rdf:type" in indv_in_storage.resources) !is null;
                                bool is_type_ss              = ("rdf:type" in ss.resources) !is null;

                                if ((is_type_indv_in_storage == true && is_type_ss == false) ||
                                    (is_type_indv_in_storage == false && is_type_ss == true) ||
                                    (is_type_indv_in_storage == false && is_type_ss == false))
                                    apply = true;
                            }

                            if (apply)
                            {
                                // обьеденить данные: ss = ss + indv_in_storage
                                auto       ss1 = ss.apply(indv_in_storage);

                                ResultCode res = context.put_individual(null, ss.uri, ss1.repare_unique("rdf:type"), false);
                                if (trace_msg[ 33 ] == 1)
                                    log.trace("file_reader:apply, uri=%s %s", ss.uri, ss1);
                                if (res != ResultCode.OK)
                                    log.trace("individual =%s, not store, errcode =%s", ss1.uri, text(res));
                            }
                            else
                            {
                                ResultCode res = context.put_individual(null, ss.uri, (*ss).repare_unique("rdf:type"), false);
                                if (trace_msg[ 33 ] == 1)
                                    log.trace("file_reader:store, uri=%s %s", ss.uri, *ss);
                                if (res != ResultCode.OK)
                                    log.trace("individual =%s, not store, errcode =%s", ss.uri, text(res));
                            }
                        }
                    }
                }
                else
                {
                    //if (for_load.get(ss.uri, false) == true)
                    {
                        if (trace_msg[ 33 ] == 1)
                            log.trace("file_reader:store, uri=%s %s", ss.uri, *ss);
                        context.put_individual(null, ss.uri, (*ss).repare_unique("rdf:type"), false);
                    }
                }
            }

            append(doc_filename, "\n</body></html>");

            context.set_reload_signal_to_local_thread("search");
        }
        //context.reopen_ro_subject_storage_db ();
        //writeln ("file_reader::prepare_file end");
    }
    catch (Exception ex)
    {
        writeln("file_reader:Exception!", ex);
    }
}
