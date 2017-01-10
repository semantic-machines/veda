/**
 * загрузка индивидов в базу данных из *.ttl
 * генерация doc/onto
 */
module veda.file_reader;

import libasync, libasync.watcher, libasync.threads;
import core.stdc.stdio, core.stdc.errno, core.stdc.string, core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;
import std.conv, std.digest.ripemd, std.bigint, std.datetime, std.concurrency, std.json, std.file, std.outbuffer, std.string, std.path,
       std.digest.md, std.utf, std.path, core.thread, core.memory, std.stdio : writeln, writefln, File;
import veda.util.container, veda.util.cbor, veda.core.util.utils, veda.common.logger, veda.util.cbor8individual, veda.util.raptor2individual;
import veda.common.type, veda.onto.individual, veda.onto.resource, veda.core.common.context, veda.core.impl.thread_context, veda.core.common.define,
       veda.core.common.know_predicates,
       veda.core.common.log_msg;

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
    {
        process_name = "ttl_reader";
        _log         = new Logger("veda-core-" ~ process_name, "log", "FILE");
    }
    return _log;
}
// ////// ////// ///////////////////////////////////////////
bool f_listen_exit = false;

shared static ~this() { destroyAsyncThreads(); }
extern (C) void handleTermination1(int _signal)
{
    log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));
    log.close();
    writeln("!SYS: ", process_name, ": exit");
    f_listen_exit = true;
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination1);
    process_name = "ttl_reader";
}

Ticket sticket;

/// процесс отслеживающий появление новых файлов и добавление их содержимого в базу данных
void main(char[][] args)
{
    bool need_remove_ontology = false;
    bool need_reload_ontology = false;
    foreach (arg; args)
    {
        if (arg == "remove-ontology")
            need_remove_ontology = true;
        if (arg == "reload-ontology")
            need_reload_ontology = true;
    }

    string parent_url = "tcp://127.0.0.1:9112\0";

    Thread.sleep(dur!("seconds")(2));
//	int checktime = 30;

    try { mkdir("ontology"); } catch (Exception ex) {}

    ubyte[] out_data;

    Context context = new PThreadContext(process_name, "file_reader", log, parent_url);
    sticket = context.sys_ticket();

    string[] uris = context.get_individuals_ids_via_query(&sticket, "'rdfs:isDefinedBy.isExists' == true", null, null, 0, 100000, 100000).result;
    log.tracec("INFO: found %d individuals containing [rdfs:isDefinedBy]", uris.length);

    if (need_remove_ontology)
    {
        OpResult res;

        //context.freeze();

        log.tracec("WARN: ALL INDIVIDUALS containing [rdfs:isDefinedBy] WILL BE REMOVED");

        foreach (uri; uris)
        {
            log.tracec("WARN: [%s] WILL BE REMOVED", uri);
            context.remove_individual(&sticket, uri, true, "ttl-reader", true, false);
        }

        uris = context.get_individuals_ids_via_query(&sticket, "'rdf:type' == 'v-s:TTLFile'", null, null, 0, 1000, 1000).result;
        foreach (uri; uris)
        {
            log.tracec("WARN: [%s] WILL BE REMOVED", uri);
            res = context.remove_individual(&sticket, uri, true, "ttl-reader", true, false);
        }

        bool complete_ft      = false;
        bool complete_script  = false;
        bool complete_subject = false;

        while (true)
        {
    	    core.thread.Thread.sleep(dur!("seconds")(1));

            long cur_opid;

            cur_opid = context.get_operation_state(P_MODULE.fulltext_indexer, false);
            log.tracec("INFO: res.op_id=%d, ft_opid=%d", res.op_id, cur_opid);                        
            if (cur_opid >= res.op_id)
                complete_ft = true;

            cur_opid = context.get_operation_state(P_MODULE.scripts_main, false);
            log.tracec("INFO: res.op_id=%d, script_opid=%d", res.op_id, cur_opid);
            if (cur_opid >= res.op_id)
                complete_script = true;

            cur_opid = context.get_operation_state(P_MODULE.subject_manager, false);
            log.tracec("INFO: res.op_id=%d, subject_opid=%d", res.op_id, cur_opid);
            if (cur_opid >= res.op_id)
                complete_subject = true;

		    if (complete_subject && complete_script && complete_ft)
				break;
        }

        log.tracec("WARN: !!!! VEDA SYSTEM NEED RESTART");


        //kill(pid, SIGKILL);

        return;
    }

    auto oFiles = dirEntries(onto_path, SpanMode.depth);

//    long    count_individuals = context.count_individuals();
    if (uris.length == 0 || need_reload_ontology)
    {
        string[] files;

        foreach (o; oFiles)
        {
            if (extension(o.name) == ".ttl")
            {
                files ~= o.name.dup;
            }
        }

        processed(files, context);
    }

    // ? now variable [oFiles] is empty, reinit
    oFiles = dirEntries(onto_path, SpanMode.depth);

    auto ev_loop = getThreadEventLoop();
    auto watcher = new AsyncDirectoryWatcher(ev_loop);

    DWChangeInfo[ 512 ] change_buf;

    watcher.run(
                {
                    log.trace("Enter Handler (directory event captured), path=%s", onto_path);
                    DWChangeInfo[] changes = change_buf[];
                    uint cnt;
                    do
                    {
                        cnt = watcher.readChanges(changes);

                        string[] _files;

                        foreach (i; 0 .. cnt)
                        {
                            string file_name = changes[ i ].path.dup;

                            if (file_name.indexOf(".#") > 0)
                                continue;

                            _files ~= file_name;
                        }

//                        processed(files, context);
                        if (_files.length > 0)
                        {
                            processed(_files, context);
                        }
/*
                        if (_files.length > 0)
                        {
                            auto oFiles = dirEntries(onto_path, SpanMode.depth);
                            string[] files;

                            foreach (o; oFiles)
                            {
                                if (extension(o.name) == ".ttl")
                                {
                                    files ~= o.name.dup;
                                }
                            }

                            processed(files, context);
                        }
 */
                    } while (cnt > 0);
                });

    watcher.watchDir(onto_path);
    foreach (o; oFiles)
    {
        if (o.isDir)
            watcher.watchDir(o.name);
    }

    while (ev_loop.loop())
    {
        if (f_listen_exit)
            break;
        else
            continue;
    }
}

//SysTime[ string ] file_modification_time;
long[ string ]    prefix_2_priority;
string[ string ] filename_2_prefix;

// Digests a file and prints the result.
string digestFile(Hash) (string filename) if (isDigest!Hash)
{
    auto   file   = File(filename);
    auto   result = digest!Hash(file.byChunk(4096 * 1024));

    string str_res = toHexString(result);

    return str_res.dup;
}

Individual[ string ] check_and_read_changed(string[] changes, Context context)
{
    Individual[ string ] individuals;
    Individual *[ string ][ string ] individuals_2_filename;
    string[] files_to_load;
    bool     is_reload = false;

    foreach (fname; changes)
    {
        if (extension(fname) == ".ttl" && fname.indexOf("#") < 0)
        {
            log.trace("change file %s", fname);

            string     file_uri       = "d:" ~ baseName(fname);
            Individual indv_ttrl_file = context.get_individual(&sticket, file_uri);

            if (indv_ttrl_file is Individual.init)
            {
                is_reload = true;
                files_to_load ~= fname;
                log.trace("file is new, %s", fname);
            }
            else
            {
                string new_hash = digestFile!MD5(fname);
                string old_hash = indv_ttrl_file.getFirstLiteral("v-s:hash");

                if (new_hash != old_hash)
                {
                    log.trace("file is modifed (hash), %s", fname);
                }
                files_to_load ~= fname;
                is_reload = true;
            }
        }
    }

    if (is_reload)
    {
        log.trace("load files: %s", files_to_load);

        foreach (filename; files_to_load)
        {
            string[ string ] prefixes;

            if (context !is null)
                prefixes = context.get_prefix_map();

            auto l_individuals = ttl2individuals(filename, prefixes, prefixes, log);

            bool f_onto = false;

            foreach (uri, indv; l_individuals)
            {
                if (indv.isExists(rdf__type, owl__Ontology))
                {
                    string o_file = filename_2_prefix.get(indv.uri, null);
                    if (o_file !is null && o_file != filename)
                    {
                        log.trace("ERR! onto[%s] already define in file [%s], this file=%s", indv.uri, o_file, filename);
                        continue;
                    }

                    filename_2_prefix[ indv.uri ] = filename;
                    long loadPriority = indv.getFirstInteger("v-s:loadPriority", -1);

                    if (loadPriority >= 0)
                        prefix_2_priority[ indv.uri ] = loadPriority;

                    f_onto = true;

                    break;
                }
            }

            if (!f_onto)
            {
                log.trace("WARN! file [%s] does not contain an instance of type owl:Ontology", filename);
                filename_2_prefix[ filename ] = filename;
                prefix_2_priority[ filename ] = 90;
                prefixes[ filename ]          = filename;
            }

            if (context !is null)
                context.add_prefix_map(prefixes);

            individuals_2_filename[ filename ] = l_individuals;
        }

        for (int priority = 0; priority < 100; priority++)
        {
            string prepared_filename;

            foreach (onto_name, filename; filename_2_prefix)
            {
                long cur_priority = prefix_2_priority.get(onto_name, 99);
                if (priority == cur_priority)
                {
                    log.trace("prepare_file %s, priority=%d", filename, priority);

                    auto indvs = individuals_2_filename.get(filename, null);
                    if (indvs !is null)
                    {
                        prepare_list(individuals, indvs.values, context, filename, onto_name);
                    }
                    prepared_filename = filename;
                }
            }
            filename_2_prefix.remove(prepared_filename);
        }
    }

    return individuals;
}

void processed(string[] changes, Context context)
{
    Ticket sticket = context.sys_ticket();

    log.trace("processed:find systicket [%s]", sticket.id);

    Individual[ string ] individuals = check_and_read_changed(changes, context);

    log.trace("processed:check_and_read_changed, count individuals=[%d]", individuals.length);

    if (individuals.length > 0)
    {
        for (int priority = 0; priority < 100; priority++)
        {
            bool is_loaded = false;

            foreach (uri, indv; individuals)
            {
                if (indv != Individual.init)
                {
                    string isDefinedBy = indv.getFirstLiteral("rdfs:isDefinedBy");

                    long   cur_priority = prefix_2_priority.get(isDefinedBy, 99);

                    if (priority == cur_priority)
                    {
                        individuals[ uri ] = Individual.init;

                        Individual indv_in_storage = context.get_individual(&sticket, uri);
                        indv_in_storage.removeResource("v-s:updateCounter");
                        indv_in_storage.removeResource("v-s:previousVersion");
                        indv_in_storage.removeResource("v-s:actualVersion");

                        indv.removeResource("v-s:updateCounter");
                        indv.removeResource("v-s:previousVersion");
                        indv.removeResource("v-s:actualVersion");
//                        log.trace("in storage, uri=%s \n%s", indv_in_storage.uri, text(indv_in_storage));

                        if (indv_in_storage == Individual.init || indv.compare(indv_in_storage) == false)
                        {
                            if (indv.getResources("rdf:type").length > 0)
                            {
                                if (trace_msg[ 33 ] == 1)
                                    log.trace("store, uri=%s %s \n--- prev ---\n%s \n--- new ----\n%s", indv.uri, uri, text(indv),
                                              text(indv_in_storage));

                                ResultCode res = context.put_individual(&sticket, indv.uri, indv, true, null, false, false).result;
                                if (trace_msg[ 33 ] == 1)
                                log.trace("file reader:store, uri=%s", indv.uri);

                                if (res != ResultCode.OK)
                                    log.trace("individual [%s], not store, errcode =%s", indv.uri, text(res));

                                is_loaded = true;
                            }
                            else
                            {
                                log.trace("individual [%s], not contain rdf:type", indv.uri);
                            }
                        }
                    }
                }
            }
        }
    }

    GC.collect();

    if (trace_msg[ 29 ] == 1)
        log.trace("file_reader::processed end");
}

//import util.individual2html;

private void prepare_list(ref Individual[ string ] individuals, Individual *[] ss_list, Context context, string filename, string onto_name)
{
    try
    {
        if (trace_msg[ 30 ] == 1)
            log.trace("[%s]ss_list.count=%d", filename, ss_list.length);

        Ticket     sticket = context.sys_ticket();

        string     hash = digestFile!MD5(filename);
        Individual indv_ttl_file;

        string     base_name = baseName(filename);
        string     dir_name  = dirName(filename);

        indv_ttl_file.uri = "d:" ~ base_name;
        indv_ttl_file.addResource("rdf:type", Resource(DataType.Uri, "v-s:TTLFile"));
        indv_ttl_file.addResource("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));
        indv_ttl_file.addResource("v-s:hash", Resource(hash));
        indv_ttl_file.addResource("v-s:filePath", Resource(dir_name));
        indv_ttl_file.addResource("v-s:fileUri", Resource(base_name));

        string prefix;
        string i_uri;

//        string doc_filename = docs_onto_path ~ "/" ~ onto_name[ 0..$ - 1 ] ~ ".html";

//        if (context !is null)
//            try
//            {
//                remove(doc_filename);
//                append(
//                       doc_filename,
//                       "<html><body><head><meta charset=\"utf-8\"/><link href=\"css/bootstrap.min.css\" rel=\"stylesheet\"/><style=\"padding: 0px 0px 30px;\"></head>\n");
//            }
//            catch (Exception ex) {}

        foreach (ss; ss_list)
        {
            //log.trace ("prepare [%s] from file [%s], onto [%s]", ss.uri, filename, onto_name);

            if (ss.isExists(rdf__type, owl__Ontology) && context !is null)
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

            indv_ttl_file.addResource("v-s:resource", Resource(DataType.Uri, ss.uri));

            Resources type = ss.getResources(rdf__type);

            if (type is Resources.init)
            {
                log.trace("Skip invalid individual (not content type), [%s]", *ss);
                continue;
            }
//            if (context !is null)
//                try
//                {
//                    append(doc_filename, individual2html(ss));
//                }
//                catch (Exception ex) {}

            long       pos_path_delimiter = indexOf(ss.uri, '/');

            Individual indv_in_storage = individuals.get(ss.uri, Individual.init);

            if (indv_in_storage !is Individual.init)
            {
                log.trace("Skip individual (already defined), [%s]", *ss);
                continue;
            }

            individuals[ ss.uri ] = *ss;
/*
            // обьеденить данные: ss = ss + indv_in_storage
            Individual ss1 = ss.apply(indv_in_storage);

            individuals[ ss.uri ] = ss1.repare_unique("rdf:type");
            if (trace_msg[ 33 ] == 1)
                log.trace("apply, uri=%s %s", ss.uri, ss1);
 */
        }

//        if (context !is null)
//            try
//            {
//                append(doc_filename, "\n</body></html>");
//            }
//            catch (Exception ex) {}

        OpResult orc = context.put_individual(&sticket, indv_ttl_file.uri, indv_ttl_file, true, null, false, false);

        //context.reopen_ro_subject_storage_db ();
        if (trace_msg[ 33 ] == 1)
            log.trace("[%s] prepare_list end", filename);
    }
    catch (Exception ex)
    {
        writeln("file_reader:Exception!", ex);
    }
}
