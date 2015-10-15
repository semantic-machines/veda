/**
 * загрузка индивидов в базу данных из *.ttl
 * генерация doc/onto
 */
module io.file_reader;

import core.stdc.stdio, core.stdc.errno, core.stdc.string, core.stdc.stdlib;
import std.conv, std.digest.ripemd, std.bigint, std.datetime, std.concurrency, std.json, std.file, std.outbuffer, std.string, std.path, std.utf,
       std.stdio : writeln;
import type;
import util.container, util.cbor, util.utils, util.logger, util.raptor2individual, veda.core.util.cbor8individual;
import veda.onto.individual, veda.onto.resource;
import veda.core.context, veda.core.thread_context, veda.core.define, veda.core.know_predicates, veda.core.log_msg;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("pacahon", "log", "search");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

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

    long    count_individuals = context.count_individuals();

    processed(context, count_individuals < 2);

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    core.thread.Thread.sleep(dur!("msecs")(10000));

    while (true)
    {
    	try
    	{
        	processed(context, true);
        }
    	catch (Throwable thw)
    	{
    		log.trace("file_reader_thread Ex:", thw.msg);
    	}

        if (checktime > 0)
        {
        	//writeln (checktime); 
            core.thread.Thread.sleep(dur!("seconds")(checktime));
        }    
    }
}

SysTime[ string ] file_modification_time;

void processed(Context context, bool is_load)
{
    Individual[ string ] individuals;
    string[] order_in_load = [ "vdi:", "v-a:", "vsrv:", "rdf:", "rdfs:", "owl:", "v-s:", "v-wf:", "v-ui:", "*", "td:" ];
    string[ string ] filename_2_prefix;
    Individual *[ string ][ string ] individuals_2_filename;
    Set!string files_to_load;

    auto oFiles = dirEntries(path, SpanMode.depth);

    if (trace_msg[ 29 ] == 1)
        log.trace("load directory sequence");

    bool is_reload = false;

    foreach (o; oFiles)
    {
        if (extension(o.name) == ".ttl")
        {
            string fname = o.name.dup;
            if ((fname in file_modification_time) !is null)
            {
                if (o.timeLastModified != file_modification_time[ fname ])
                {
                    file_modification_time[ fname ] = o.timeLastModified;

                    is_reload = true;

                    if (trace_msg[ 29 ] == 1)
                        log.trace("look modifed file=%s", fname);
                }
            }
            else
            {
                file_modification_time[ fname ] = o.timeLastModified;

                is_reload = true;

                if (trace_msg[ 29 ] == 1)
                    log.trace("look new file=%s", fname);
            }

            files_to_load ~= fname;
        }
    }

    if (is_reload && is_load)
    {
        foreach (filename; files_to_load)
        {
            log.trace("prepare_file %s", filename);

            auto l_individuals = ttl2individuals(filename, context);

            foreach (uri, indv; l_individuals)
            {
                if (indv.isExist(rdf__type, owl__Ontology))
                {
                    filename_2_prefix[ indv.uri ] = filename;
                    break;
                }
            }

            individuals_2_filename[ filename ] = l_individuals;
        }

        int idx = 0;
        foreach (pos; order_in_load)
        {
            foreach (onto_name, filename; filename_2_prefix)
            {
                bool is_next = false;
                if (pos == "*")
                {
                    is_next = true;
                    for (int i = idx; i < order_in_load.length; i++)
                    {
                        if (onto_name == order_in_load[ i ])
                        {
                            is_next = false;
                            break;
                        }
                    }
                }
                else
                {
                    if (onto_name == pos)
                        is_next = true;
                }

                if (is_next == true)
                {
                    auto indvs = individuals_2_filename.get(filename, null);
                    if (indvs !is null)
                        prepare_list(individuals, indvs.values, context, onto_name);
                }
            }
        }

        //writeln("@@1 ontohashes_2_filename=", ontohashes_2_filename);
        //writeln("@@2 filename_2_prefix=", filename_2_prefix);
        //writeln("@@3 modifed_2_file=", modifed_2_file);

        // load index onto
        idx = 0;
        foreach (pos; order_in_load)
        {
            foreach (uri, indv; individuals)
            {
                if (indv != Individual.init)
                {
                    string isDefinedBy = indv.getFirstLiteral("rdfs:isDefinedBy");
//        log.trace("load directory sequence 2...pos=%s", pos);
                    bool   is_next = false;
                    if (pos == "*")
                    {
                        is_next = true;
                        for (int i = idx; i < order_in_load.length; i++)
                        {
                            if (isDefinedBy == order_in_load[ i ])
                            {
                                is_next = false;
                                break;
                            }
                        }
                    }
                    else
                    {
                        if (isDefinedBy == pos)
                            is_next = true;
                    }

                    if (is_next == true)
                    {
                        individuals[ uri ] = Individual.init;

                        Individual indv_in_storage = context.get_individual(null, uri);
                        //log.trace("in storage, uri=%s \n%s", indv_in_storage.uri, text(indv_in_storage));

                        if (indv_in_storage == Individual.init || indv.compare(indv_in_storage) == false)
                        {
                            ResultCode res = context.put_individual(null, indv.uri, indv, false);
                            if (trace_msg[ 33 ] == 1)
                                log.trace("store, uri=%s %s", indv.uri, indv);
                                
                            //log.trace("store, uri=%s %s \n%s \n%s", indv.uri, uri, text(indv), text(indv_in_storage));
                            if (res != ResultCode.OK)
                                log.trace("individual =%s, not store, errcode =%s", indv.uri, text(res));
                        }
                    }
                }
            }
            idx++;

            //    context.reopen_ro_subject_storage_db();
			//    context.reopen_ro_fulltext_indexer_db();

            Tid tid_condition_manager = context.getTid(P_MODULE.condition);
            if (tid_condition_manager != Tid.init)
            {
                core.thread.Thread.sleep(dur!("seconds")(1));
                send(tid_condition_manager, CMD.RELOAD, thisTid);
                receive((bool res) {});
            }
        }
    }

    context.set_reload_signal_to_local_thread("search");

    core.memory.GC.collect();

    if (trace_msg[ 29 ] == 1)
        log.trace("file_reader::processed end");
}

import util.individual2html;

private void prepare_list(ref Individual[ string ] individuals, Individual *[] ss_list, Context context, string onto_name)
{
    // 2. попутно находит системный аккаунт (veda)
    try
    {
        if (trace_msg[ 30 ] == 1)
            log.trace("ss_list.count=%d", ss_list.length);


        string prefix;
        string i_uri;

        string doc_filename = docs_onto_path ~ "/" ~ onto_name[ 0..$ - 1 ] ~ ".html";

        try
        {
            remove(doc_filename);

            append(
                   doc_filename,
                   "<html><body><head><meta charset=\"utf-8\"/><link href=\"css/bootstrap.min.css\" rel=\"stylesheet\"/><style=\"padding: 0px 0px 30px;\"></head>\n");
        }
        catch (Exception ex)
        {
        }

        foreach (ss; ss_list)
        {
            if (ss.isExist(rdf__type, owl__Ontology))
            {
                prefix = context.get_prefix_map.get(ss.uri, null);
                Resources ress = Resources.init;
                ress ~= Resource(prefix);
                ss.resources[ veda_schema__fullUrl ] = ress;
            }

            if (("rdfs:isDefinedBy" in ss.resources) is null)
            {
                ss.addResource("rdfs:isDefinedBy", Resource(DataType.Uri, onto_name));
            }

            append(doc_filename, individual2html(ss));

            long       pos_path_delimiter = indexOf(ss.uri, '/');

            Individual indv_in_storage = individuals.get(ss.uri, Individual.init);

            // обьеденить данные: ss = ss + indv_in_storage
            Individual ss1 = ss.apply(indv_in_storage);

            individuals[ ss.uri ] = ss1.repare_unique("rdf:type");
            if (trace_msg[ 33 ] == 1)
                log.trace("apply, uri=%s %s", ss.uri, ss1);
        }

        append(doc_filename, "\n</body></html>");

        //context.reopen_ro_subject_storage_db ();
        if (trace_msg[ 33 ] == 1)
            log.trace("prepare_list end");
        //writeln ("file_reader::prepare_file end");
    }
    catch (Exception ex)
    {
        writeln("file_reader:Exception!", ex);
    }
}
