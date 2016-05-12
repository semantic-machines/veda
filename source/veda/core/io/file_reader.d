/**
 * загрузка индивидов в базу данных из *.ttl
 * генерация doc/onto
 */
module veda.core.io.file_reader;

import libasync, libasync.watcher, libasync.threads;
import core.stdc.stdio, core.stdc.errno, core.stdc.string, core.stdc.stdlib;
import std.conv, std.digest.ripemd, std.bigint, std.datetime, std.concurrency, std.json, std.file, std.outbuffer, std.string, std.path,
       std.digest.md, std.utf, std.path, std.stdio : writeln, File;
import veda.util.container, veda.util.cbor, veda.core.util.utils, util.logger, veda.core.util.raptor2individual, veda.util.cbor8individual;
import veda.type, veda.onto.individual, veda.onto.resource, veda.core.common.context, veda.core.impl.thread_context, veda.core.common.define,
       veda.core.common.know_predicates,
       veda.core.log_msg;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "FILE");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

shared static ~this() { destroyAsyncThreads(); }

/// процесс отслеживающий появление новых файлов и добавление их содержимого в базу данных
void file_reader_thread(P_MODULE id, string node_id, int checktime)
{
    core.thread.Thread tr = core.thread.Thread.getThis();
    tr.name = std.conv.text(id);

    try { mkdir("ontology"); } catch (Exception ex) {}

    ubyte[] out_data;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    core.thread.Thread.sleep(dur!("msecs")(3000));

    Context context = new PThreadContext(node_id, "file_reader", id);

    auto    oFiles = dirEntries(onto_path, SpanMode.depth);

    long    count_individuals = context.count_individuals();
    if (count_individuals < 2)
    {
        bool all_modules_ready = false;
        while (all_modules_ready == false)
        {
            all_modules_ready = veda.core.threads.dcs_manager.examine_modules();
            if (all_modules_ready == false)
                core.thread.Thread.sleep(dur!("msecs")(2000));
        }

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

    auto ev_loop = getThreadEventLoop();
    auto watcher = new AsyncDirectoryWatcher(ev_loop);

    DWChangeInfo[ 512 ] change_buf;

    watcher.run(
                {
                    //writeln("Enter Handler (directory event captured), path=", path);
                    DWChangeInfo[] changes = change_buf[];
                    uint cnt;
                    do
                    {
                        cnt = watcher.readChanges(changes);
                        string[] files;

                        foreach (i; 0 .. cnt)
                        {
                            files ~= changes[ i ].path.dup;
                        }

                        processed(files, context);
                    } while (cnt > 0);
                });

    watcher.watchDir(onto_path);
    foreach (o; oFiles)
    {
        if (o.isDir)
            watcher.watchDir(o.name);
    }

    while (ev_loop.loop())
        continue;
}

//SysTime[ string ] file_modification_time;
long[ string ]    prefix_2_priority;

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
    string[ string ] filename_2_prefix;
    Individual *[ string ][ string ] individuals_2_filename;
    string[] files_to_load;
    bool     is_reload = false;

    Ticket   sticket = context.sys_ticket();

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
                    is_reload = true;
                    files_to_load ~= fname;

                    log.trace("file is modifed (hash), %s", fname);
                }
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

            auto l_individuals = ttl2individuals(filename, prefixes, prefixes);

            if (context !is null)
                context.add_prefix_map(prefixes);

            foreach (uri, indv; l_individuals)
            {
                if (indv.isExists(rdf__type, owl__Ontology))
                {
                    filename_2_prefix[ indv.uri ] = filename;
                    long loadPriority = indv.getFirstInteger("v-s:loadPriority", -1);

                    if (loadPriority >= 0)
                        prefix_2_priority[ indv.uri ] = loadPriority;

                    break;
                }
            }

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
    string guest_ticket = context.get_ticket_from_storage("guest");

    if (guest_ticket is null)
        context.create_new_ticket("cfg:Guest", "4000000", "guest");

    Ticket sticket = context.sys_ticket();

    Individual[ string ] individuals = check_and_read_changed(changes, context);

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
//                        log.trace("in storage, uri=%s \n%s", indv_in_storage.uri, text(indv_in_storage));

                        if (indv_in_storage == Individual.init || indv.compare(indv_in_storage) == false)
                        {
                            ResultCode res = context.put_individual(&sticket, indv.uri, indv, true, null, false, false).result;
                            //if (trace_msg[ 33 ] == 1)
                                log.trace("file reader:store, uri=%s", indv.uri);

                            //log.trace("store, uri=%s %s \n%s \n%s", indv.uri, uri, text(indv), text(indv_in_storage));
                            if (res != ResultCode.OK)
                                log.trace("individual =%s, not store, errcode =%s", indv.uri, text(res));

                            is_loaded = true;
                        }
                    }
                }
            }
        }
    }

    core.memory.GC.collect();

    if (trace_msg[ 29 ] == 1)
        log.trace("file_reader::processed end");
}

//import util.individual2html;

private void prepare_list(ref Individual[ string ] individuals, Individual *[] ss_list, Context context, string filename, string onto_name)
{
    try
    {
        if (trace_msg[ 30 ] == 1)
            log.trace("ss_list.count=%d", ss_list.length);

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


//            if (context !is null)
//                try
//                {
//                    append(doc_filename, individual2html(ss));
//                }
//                catch (Exception ex) {}

            long       pos_path_delimiter = indexOf(ss.uri, '/');

            Individual indv_in_storage = individuals.get(ss.uri, Individual.init);

            // обьеденить данные: ss = ss + indv_in_storage
            Individual ss1 = ss.apply(indv_in_storage);

            individuals[ ss.uri ] = ss1.repare_unique("rdf:type");
            if (trace_msg[ 33 ] == 1)
                log.trace("apply, uri=%s %s", ss.uri, ss1);
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
            log.trace("prepare_list end");
    }
    catch (Exception ex)
    {
        writeln("file_reader:Exception!", ex);
    }
}
