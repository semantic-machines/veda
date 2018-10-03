/**

 * user modules manager
 */
module veda.ttlreader.user_modules_tool;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread, std.net.curl, std.algorithm;
private import url, std.uuid, std.json, std.process;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.util.raptor2individual;
private import veda.vmodule.vmodule, veda.search.isearch;

void user_modules_tool_thread()
{
    Thread.sleep(dur!("seconds")(3));

    auto p_module = new UserModulesTool(SUBSYSTEM.USER_MODULES_TOOL, MODULE.user_modules_tool, new Logger("veda-core-users-modules-tool", "log", ""));

    p_module.run();
}

const umt_event_id      = "user_module_tool";
const request_predicate = "v-s:RequestToModulesManager";

enum RequestCommand
{
    INSTALL,
    UNINSTALL,
    CHECK,
    NONE
}

enum OperationResult
{
    OK,
    FOUND_INVALID_VERSION,
    FOUND_ANOTHER_VERSION,
    FOUND_EQUAL_VERSION,
    FAIL,
    NONE
}

UserModuleInfo[ string ] tmp_installed_modules;

Individual create_request(string url, RequestCommand cmd)
{
    Individual indv;

    indv.uri = "d:" ~ randomUUID().toString();
    indv.setResources("rdf:type", [ Resource(DataType.Uri, request_predicate) ]);
    indv.setResources("v-s:moduleUrl", [ Resource(DataType.String, url) ]);
    indv.addResource("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));

    return indv;
}

class UserModuleInfo
{
    string         request_uri;
    string         uri;
    string         url;
    string         ver;
    UserModuleInfo[ string ] dependencies;
    UserModuleInfo parent;

    Individual *[ string ] module_individuals;
    string          project_name;
    string          project_owner;
    string          modile_temp_dir;
    string          module_file_path;
    string          unpacked_module_folder_name;
    string          ghrl_url;
    string          releases_path;
    string          install_id;

    string          prev_module_name;

    string[]        total_hash_indv_file;
    string[]        total_hash_indv_storage;

    OperationResult operation_result = OperationResult.NONE;

    Context         context;
    Ticket          sticket;
    //Logger          log;
    ArrayLogger     log;

    override string toString()
    {
        string operation_result = uri ~ ":" ~ ver;

        foreach (uid; dependencies.keys)
        {
            UserModuleInfo umi = dependencies.get(uid, null);
            if (umi !is null)
                operation_result ~= " { " ~ umi.toString() ~ " }";
        }
        return operation_result;
    }

    this(Context _context, Ticket _sticket, ref Individual module_indv)
    {
        context = _context;
        sticket = _sticket;

//        if (context !is null)
//            log = context.get_logger();
//        else
//            stderr.writeln("ERR! fail create object UserModuleInfo, log not initalized");
        log = new ArrayLogger();

        uri = module_indv.uri;
        url = module_indv.getFirstLiteral("v-s:moduleUrl");
        ver = module_indv.getFirstLiteral("v-s:moduleVersion");
    }

    this(Context _context, Ticket _sticket, string _install_id)
    {
        install_id = _install_id;
        context    = _context;
        sticket    = _sticket;

//        if (context !is null)
//            log = context.get_logger();
//        else
//            stderr.writeln("ERR! fail create object UserModuleInfo, log not initalized");
        log = new ArrayLogger();
    }

    public void store_log()
    {
        Individual indv;

        indv.uri = uri;
        indv.setResources("rdfs:comment", [ Resource(DataType.String, log.raw()) ]);

        OpResult rs = context.update(null, -1, &sticket, INDV_OP.SET_IN, &indv, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

        if (rs.result != ResultCode.OK)
        {
            log.trace("ERR! fail store log [%s], err=[%s]", uri, rs.result);
        }
    }

    public UserModuleInfo[ string ] get_installed_modules()
    {
        UserModuleInfo[ string ] result;
        int[ string ] link_count_2_module_uid;

        context.reopen_ro_fulltext_indexer_db();

        SearchResult sr =
            context.get_individuals_ids_via_query(sticket.user_uri, "'rdf:type' === 'v-s:Module'", "'rdf:type' asc", "base,system", 0, 100000,
                                                  10000,
                                                  null, OptAuthorize.NO,
                                                  false);

        Individual[ string ] mmsrc;

        foreach (uid; sr.result)
        {
            Individual module_info_ist = context.get_individual(&sticket, uid, OptAuthorize.NO);
            result[ uid ] = new UserModuleInfo(context, sticket, module_info_ist);
            mmsrc[ uid ]  = module_info_ist;
        }

        foreach (uid; mmsrc.keys)
        {
            Individual     mm_indv = mmsrc[ uid ];
            UserModuleInfo umi     = result[ uid ];

            Resources      deps = mm_indv.getResources("v-s:dependency");

            int            clnk = link_count_2_module_uid.get(uid, -1);
            if (clnk == -1)
                link_count_2_module_uid[ uid ] = 0;

            foreach (dep; deps)
            {
                UserModuleInfo dumi = result.get(dep.uri, null);
                if (dumi !is null)
                {
                    umi.dependencies[ dep.uri ] = dumi;
                    dumi.parent                 = umi;
                }

                clnk                               = link_count_2_module_uid.get(dep.uri, 0);
                link_count_2_module_uid[ dep.uri ] = clnk + 1;
            }
        }

        // context.get_logger().trace("@result=[%s]", result);
        // context.get_logger().trace("@link_count_2_module_uid=[%s]", link_count_2_module_uid);

        //foreach (key; link_count_2_module_uid.keys)
        //{
        //    if (link_count_2_module_uid[ key ] == 0)
        //        context.get_logger().trace("FOUND MODULE [%s]", result[ key ]);
        //}

        return result;
    }

    void uninstall(UserModuleInfo root_uninstall_module)
    {
        //log.trace("*****");
        //log.trace("@root_uninstall_module=%s", root_uninstall_module);
        try
        {
            string module_id;

            if (prev_module_name !is null && module_id != "")
                module_id = prev_module_name;
            else
                module_id = uri;

            if (module_id is null || module_id == "")
            {
                operation_result = OperationResult.OK;
                return;
            }

            if (operation_result != OperationResult.FOUND_ANOTHER_VERSION && operation_result != OperationResult.NONE)
            {
                operation_result = OperationResult.OK;

                return;
            }

            operation_result = OperationResult.NONE;

            // проверить, используется ли модуль другими модулями

            if (tmp_installed_modules.length == 0)
                tmp_installed_modules = get_installed_modules();

            //log.trace("@this=%s", this);
            foreach (im; tmp_installed_modules)
            {
                if (im.uri != root_uninstall_module.uri)
                {
                    //log.trace("@im=%s", im);
                    foreach (dep; im.dependencies)
                    {
                        //log.trace("@dep=%s", dep);
                        if (dep.uri == uri)
                        {
                            //log.trace ("im.uri=%s uri=%s parent.uri=%s", im.uri, uri, parent.uri);
                            if (im.uri != root_uninstall_module.uri)
                            {
                                if (dep.ver == ver)
                                {
                                    log.trace("MODULE [%s][%s] USED IN [%s][%s], SKIP UNINSTALL", dep.uri, dep.ver, im.uri, im.ver);
                                    operation_result = OperationResult.FAIL;
                                    return;
                                }
                                else
                                {
                                    log.trace("ERR! [%s][%s] requires [%s][%s], but already installed [%s][%s] used [%s][%s]",
                                              root_uninstall_module.uri, root_uninstall_module.ver, uri, ver, im.uri, im.ver, dep.uri, dep.ver);
                                    operation_result = OperationResult.FAIL;
                                    return;
                                }
                            }
                        }
                    }
                }
            }

            //log.trace("@1 prev_module_name=%s", prev_module_name);
            //log.trace("@2 uri=%s", uri);

            log.trace("UNINSTALL MODULE [%s][%s], check result=%s", module_id, ver, operation_result);

            SearchResult sr =
                context.get_individuals_ids_via_query(sticket.user_uri, "'rdfs:isDefinedBy' === '" ~ module_id ~ "'", "'rdfs:isDefinedBy' asc",
                                                      "base,system,deleted", 0, 100000,
                                                      10000,
                                                      null, OptAuthorize.NO,
                                                      false);

            //log.trace("found %s ", sr.result);

            bool is_success = true;

            foreach (uid; sr.result)
            {
                log.trace("[%s][%s] REMOVE %s", module_id, ver, uid);

                Individual individual;
                individual.uri = uid;
                OpResult   operation_result = context.update(null, -1, &sticket, INDV_OP.REMOVE, &individual, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

                if (operation_result.result != ResultCode.OK)
                {
                    log.trace("ERR! fail remove [%s], err=[%s]", uid, operation_result.result);
                    is_success = false;
                    break;
                }
            }

            if (is_success == true)
            {
                string dest_image_name = replace(uri, ":", "_") ~ "-" ~ "image.jpeg";

                try
                {
                    remove("./data/files/" ~ dest_image_name);
                } catch (Throwable tr)
                {
                    log.trace("WARN! can not remove %s, err=%s", dest_image_name, tr.msg);
                }

                Individual mindv = context.get_individual(&sticket, module_id, OptAuthorize.NO);
                if (mindv.getStatus() == ResultCode.OK)
                {
                    Individual indv;
                    indv.uri = module_id;
                    indv.setResources("v-s:deleted", [ Resource(DataType.Boolean, true) ]);

                    OpResult rs = context.update(null, -1, &sticket, INDV_OP.SET_IN, &indv, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

                    if (rs.result != ResultCode.OK)
                    {
                        log.trace("ERR! fail deactivate [%s], err=[%s]", module_id, rs.result);

                        operation_result = OperationResult.FAIL;

                        return;
                    }
                    else
                    {
                        log.trace("success deactivate [%s]", module_id);
                        if (tmp_installed_modules.length > 0)
                            tmp_installed_modules.remove(module_id);
                    }
                }

                operation_result = OperationResult.OK;

                return;
            }
            else
                operation_result = OperationResult.FAIL;

            return;
        }
        finally
        {
            log.trace("@ finally, operation_result=[%s]", operation_result);

            if (operation_result == OperationResult.OK)
            {
                log.trace("@ finally #1 dependencies");
                foreach (dep; dependencies)
                {
                    log.trace("@ finally, dep =%s", dep);
                    dep.uninstall(root_uninstall_module);
                    if (dep.operation_result == OperationResult.FAIL)
                        break;
                    //dep.uninstall(root_uninstall_module);
                }
            }
        }
    }

    void store_module_individ()
    {
        //log.trace("STORE MODULE INFO [%s][%s]", uri, ver);
        Individual module_indv;

        module_indv.uri = uri;
        module_indv.setResources("rdf:type", [ Resource(DataType.Uri, "v-s:Module") ]);
        module_indv.setResources("v-s:moduleUrl", [ Resource(DataType.String, url) ]);
        module_indv.setResources("v-s:moduleVersion", [ Resource(DataType.String, ver) ]);
        foreach (dep; dependencies)
            module_indv.addResource("v-s:dependency", Resource(DataType.Uri, dep.uri));

        module_indv.setResources("rdfs:comment", [ Resource(DataType.String, log.raw()) ]);

        string module_image_path = unpacked_module_folder_name ~ "/image.jpeg";
        string dest_image_name   = replace(uri, ":", "_") ~ "-" ~ "image.jpeg";

        try
        {
            copy(module_image_path, "./data/files/" ~ dest_image_name);

            Individual image_indv;
            image_indv.uri = uri ~ "-image";

            image_indv.setResources("rdf:type", [ Resource(DataType.Uri, "v-s:File") ]);
            image_indv.setResources("rdfs:isDefinedBy", [ Resource(DataType.Uri, uri) ]);
            image_indv.setResources("v-s:fileName", [ Resource(DataType.String, "image.jpeg") ]);
            image_indv.setResources("v-s:filePath", [ Resource(DataType.String, "/") ]);
            image_indv.setResources("v-s:fileUri", [ Resource(DataType.String, dest_image_name) ]);

            OpResult orc = context.update(null, -1, &sticket, INDV_OP.PUT, &image_indv, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

            if (orc.result != ResultCode.OK)
            {
                log.trace("WARN! can not install %s, err=%s", image_indv.uri, orc.result);
            }
            else
                module_indv.addResource("v-s:hasImage", Resource(DataType.Uri, image_indv.uri));
        }
        catch (Throwable tr)
        {
            log.trace("WARN! can not install %s, err=%s", dest_image_name, tr.msg);
        }

        OpResult orc = context.update(null, -1, &sticket, INDV_OP.PUT, &module_indv, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);
    }

    bool install()
    {
        foreach (dep; dependencies)
        {
            if (dep.install() == false)
                return false;
        }


        if (operation_result == OperationResult.FOUND_EQUAL_VERSION)
        {
            log.trace("MODULE [%s][%s] ALREADY INSTALLED", uri, ver);

            Individual mindv = context.get_individual(&sticket, uri, OptAuthorize.NO);
            if (mindv.getStatus() != ResultCode.OK)
                this.store_module_individ();
            else if (mindv.isExists("v-s:deleted", true))
                this.store_module_individ();

            return true;
        }
        else
            log.trace("INSTALL MODULE [%s][%s] %d", uri, ver, module_individuals.keys.length);

        bool[ string ] installed;
        bool is_sucess = true;

        foreach (uid; module_individuals.keys)
        {
            OpResult orc = context.update(null, -1, &sticket, INDV_OP.PUT, module_individuals[ uid ], umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

            log.trace("[%s][%s] INSERT %s", uri, ver, uid);

            if (orc.result == ResultCode.OK)
                installed[ uid ] = true;
            else
            {
                log.trace("ERR! fail store indvidual %s, errcode=%s", *module_individuals[ uid ], orc.result);
                is_sucess = false;
                break;
            }
        }

        if (is_sucess == false)
        {
            log.trace("fail installation, remove installed individuals");

            foreach (uid; installed.keys)
            {
                Individual individual;
                individual.uri = uid;
                OpResult   operation_result = context.update(null, -1, &sticket, INDV_OP.REMOVE, &individual, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);
            }

            return false;
        }
        else
        {
            store_module_individ();
            // log.trace("@module_indv=%s", module_indv);
            // log.trace("@orc=%s", orc);
            log.trace("installation module [%s][%s] was success", url, ver);
        }

        return true;
    }

    void check()
    {
        foreach (dep; dependencies)
        {
            dep.check();
        }
        // check onto

        log.trace("CHECK MODULE [%s]", uri);

        foreach (uid; module_individuals.keys)
        {
            Individual *indv_0        = module_individuals[ uid ];
            string     hash_indv_file = indv_0.get_CRC32();

            //log.trace("check individual [%s]", uid);
            Individual indv_in_storage = context.get_individual(&sticket, uid, OptAuthorize.NO);

            if (indv_in_storage.getStatus() == ResultCode.OK)
            {
                string hash_indv_storage = indv_in_storage.get_CRC32();
                total_hash_indv_storage ~= hash_indv_storage;

                string is_defined_by_in_storage = indv_in_storage.getFirstLiteral("rdfs:isDefinedBy");

                if (is_defined_by_in_storage != uri)
                {
                    // [rdfs:isDefinedBy] is not equal to the installed uid module, we check the possibility of replacement
                    Individual indv_module = context.get_individual(&sticket, is_defined_by_in_storage, OptAuthorize.NO);
                    if (indv_module.getStatus() == ResultCode.OK)
                    {
                        if (indv_module.isExists("rdf:type", "v-s:Module") == true)
                        {
                            log.trace("[%s] already exist, and found another installation %s, break check", uid, is_defined_by_in_storage);
                            prev_module_name = is_defined_by_in_storage;
                            operation_result = OperationResult.FOUND_ANOTHER_VERSION;
                            return;
                        }
                    }
                }
                else
                {
                    if (hash_indv_storage != hash_indv_file)
                    {
                        log.trace("hash in storage %s, hash in file %s", hash_indv_storage, hash_indv_file);

                        log.trace("[%s] already exist, and rdfs:isDefinedBy=%s, break check", uid, is_defined_by_in_storage);
                        prev_module_name = is_defined_by_in_storage;
                        operation_result = OperationResult.FOUND_ANOTHER_VERSION;
                        return;
                    }
                }
            }
            else if (indv_in_storage.getStatus() != ResultCode.Not_Found && indv_in_storage.getStatus() != ResultCode.Unprocessable_Entity)
            {
                log.trace("ERR! [%s] already exist, but not read, errcode=%s", uid, indv_in_storage.getStatus());
                operation_result = OperationResult.FOUND_INVALID_VERSION;
                return;
            }
        }

        total_hash_indv_storage.sort();
        total_hash_indv_file.sort();

        if (total_hash_indv_storage.length > 0 && total_hash_indv_file.length > 0)
        {
            if (text(total_hash_indv_file) == text(total_hash_indv_storage))
            {
                log.trace("module [%s][%s] already installed", uri, ver);
                operation_result = OperationResult.FOUND_EQUAL_VERSION;
            }
            else
            {
                log.trace("module [%s][%s] not equal installed in storage %d, in file %d", uri, ver, total_hash_indv_storage.length,
                          total_hash_indv_file.length);
                operation_result = OperationResult.FOUND_ANOTHER_VERSION;
            }
        }

        log.trace("module [%s][%s] check individuals %d", uri, ver, module_individuals.length);
    }


    void get_and_unpack(ref Individual module_indv)
    {
        log.trace("GET AND UNPACK MODULE [%s]", module_indv.uri);

        uri = module_indv.uri;
        url = module_indv.getFirstLiteral("v-s:moduleUrl");
        ver = module_indv.getFirstLiteral("v-s:moduleVersion");

        string module_url;
        string[ string ] ver_2_url;
        string[ int ] pos_2_ver;

        auto     o_url = url.parseURL;
        string[] pp    = o_url.path.split('/');
        if (o_url.host == "bitbucket.org")
        {
            // https://[your_user_name]:[app_password]@bitbucket.org/[your_user_name]/[repo_name]/get/[tag].zip
            stderr.writefln("@pp=%s", pp);

            if (pp.length != 5 || (pp.length == 5 && pp[ 3 ] != "get"))
            {
                log.trace("ERR! unknown url format [%s], get_and_unpack", url);
                operation_result = OperationResult.FAIL;
                return;
            }

            project_owner = pp[ 1 ];
            project_name  = pp[ 2 ];

            stderr.writefln("@project_owner=%s", project_owner);
            stderr.writefln("@project_name=%s", project_name);

            long pt = pp[ 4 ].indexOf(".zip");
            if (pt > 0)
            {
                ver = pp[ 4 ][ 0 .. pt ];
                stderr.writefln("@ver=%s", ver);

                ver_2_url[ ver ] = url;
                stderr.writefln("@ver_2_url=%s", ver_2_url);
            }
        }
        else if (o_url.host == "github.com")
        {
            if (pp.length != 3 && pp.length != 6)
            {
                log.trace("ERR! unknown url format [%s], get_and_unpack", url);
                operation_result = OperationResult.FAIL;
                return;
            }

            project_owner = pp[ 1 ];
            project_name  = pp[ 2 ];

            foreach (idx, pi; pp)
            {
                if (pp.length >= idx + 2 && pi == "releases" && pp[ idx + 1 ] == "tag")
                {
                    ver = pp[ idx + 2 ];
                    break;
                }
            }


            modile_temp_dir  = tempDir() ~ "/" ~ install_id ~ "/" ~ project_owner ~ "-" ~ project_name;
            module_file_path = modile_temp_dir ~ "/module.zip";

            try
            {
                mkdirRecurse(modile_temp_dir);
            }
            catch (Throwable tr)
            {
                log.trace("ERR! %s can't create tmp folder %s, get_and_unpack", tr.msg, modile_temp_dir);
                operation_result = OperationResult.FAIL;
                return;
            }

            if (ver is null)
                log.trace("version not specified");

            // get releases
            ghrl_url      = "http://api.github.com/repos/" ~ project_owner ~ "/" ~ project_name ~ "/releases";
            releases_path = modile_temp_dir ~ "/releases.json";

            download(ghrl_url, releases_path);

            string js_releases = readText(releases_path);
            if (js_releases is null || js_releases == "")
            {
                log.trace("ERR! fail read json file of releases [%s], get_and_unpack", releases_path);
                operation_result = OperationResult.FAIL;
                return;
            }

            try
            {
                JSONValue jva = parseJSON(js_releases);

                int       pos = 0;
                foreach (jv; jva.array())
                {
                    string tag_name = jv[ "tag_name" ].str;
                    ver_2_url[ tag_name ] = jv[ "zipball_url" ].str;
                    pos_2_ver[ pos ]      = tag_name;
                    pos++;
                }
            }
            catch (Throwable tr)
            {
                log.trace("ERR! [%s] fail parse release.json [%s] ", tr.msg, js_releases);

                log.trace("ERR! fail parse json file of releases [%s], get_and_unpack", releases_path);
                operation_result = OperationResult.FAIL;
                return;
            }

            if (ver is null && pos_2_ver.length > 0)
            {
                string[] found_tags_in_project = pos_2_ver.values;
                found_tags_in_project.sort();

                ver = found_tags_in_project[ $ - 1 ];
            }
        }

        if (ver is null)
            log.trace("ERR! not found releases");
        else
        {
            module_url = ver_2_url.get(ver, string.init);

            //log.trace("@ ver_2_url=%s pos_2_ver=%s", ver_2_url, pos_2_ver);
            //log.trace("@ module_url=[%s]", module_url);
            //log.trace("@ ver=[%s]", ver);

            if (module_url is null)
            {
                log.trace("ERR! fail read module url from json file [%s], get_and_unpack", releases_path);
                operation_result = OperationResult.FAIL;
                return;
            }

            log.trace("found version %s", ver);
            log.trace("download module %s", module_url);
            download(module_url, module_file_path);

            // unpack module
            string unpack_cmd = "unzip -d " ~ modile_temp_dir ~ " " ~ module_file_path;
            auto   ps         = executeShell(unpack_cmd);
            if (ps.status != 0)
            {
                log.trace("ERR! fail unpack module [%s], get_and_unpack", unpack_cmd);
                operation_result = OperationResult.FAIL;
                return;
            }

            string[] unpacked_file_list = ps.output.splitLines;
            foreach (line; unpacked_file_list)
            {
                if (line.indexOf(" creating: ") > 0)
                {
                    auto ll = line.split(' ');
                    foreach (li; ll)
                    {
                        if (li.indexOf(modile_temp_dir) >= 0)
                        {
                            unpacked_module_folder_name = li.strip();
                            break;
                        }
                    }
                    if (unpacked_module_folder_name !is null)
                        break;
                }
            }
        }

        if (unpacked_module_folder_name is null)
        {
            log.trace("ERR! fail unpack module [%s], get_and_unpack", module_file_path);
            operation_result = OperationResult.FAIL;
            return;
        }

        if (unpacked_module_folder_name[ $ - 1 ] == '/')
            unpacked_module_folder_name = unpacked_module_folder_name[ 0..$ - 1 ];

        // found module.ttl
        string[ string ] prefixes;
        string root_indv;
        string module_ttl_path = unpacked_module_folder_name ~ "/module.ttl";
        auto   l_individuals   = ttl2individuals(module_ttl_path, prefixes, prefixes, context.get_logger());

        // found root individual of file module.ttl
        foreach (uid; l_individuals.keys)
        {
            Individual *indv = l_individuals[ uid ];
            auto       t_url = indv.getFirstLiteral("v-s:moduleUrl").parseURL;
            if (t_url.path == o_url.path)
            {
                root_indv = uid;

                if (uri != root_indv)
                {
                    if (module_indv.isExists("rdf:type", request_predicate) == false)
                    {
                        log.trace("WARN! prepared module [%s] not equal loaded module [%s]", uri, root_indv);
                        log.trace("WARN! remove prepared module [%s]", uri);

                        Individual individual;
                        individual.uri = uri;
                        OpResult   operation_result = context.update(null, -1, &sticket, INDV_OP.REMOVE, &individual, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);
                    }

                    uri = root_indv;
                }

                break;
            }
        }

        if (root_indv is null)
        {
            log.trace("ERR! not found root element [v-s:moduleUrl=%s] in [%s], get_and_unpack", url, module_ttl_path);
            operation_result = OperationResult.FAIL;
            return;
        }

        //log.trace("@root indv=%s", root_indv);
        auto onto_files = dirEntries(unpacked_module_folder_name ~ "/onto", SpanMode.depth);
        foreach (file; onto_files)
        {
            log.trace("[%s] prepare %s", uri, file);

            auto tmp_individuals = ttl2individuals(file, prefixes, prefixes, context.get_logger());

            foreach (uid; tmp_individuals.keys)
            {
                Individual *indv_0 = tmp_individuals[ uid ];

                Resources  type = indv_0.getResources("rdf:type");
                if (type is Resources.init)
                {
                    log.trace("Skip invalid individual (not content type), [%s]", *indv_0);
                    continue;
                }

                indv_0.setResources("rdfs:isDefinedBy", [ Resource(DataType.Uri, uri) ]);

                string hash_indv_file = indv_0.get_CRC32();
                total_hash_indv_file ~= hash_indv_file;
                module_individuals[ uid ] = indv_0;
            }
        }

        log.trace("module [%s][%s] found individuals %d", uri, ver, module_individuals.length);

        // go tree
        Resources deps = l_individuals[ root_indv ].getResources("v-s:dependency");

        //log.trace("@l_individuals=%s", l_individuals);
        //log.trace("@uri=%s", uri);
        //log.trace("@url=%s", url);
        //log.trace("@root_indv=%s", root_indv);
        //log.trace("@deps=%s", deps);

        foreach (dep; deps)
        {
            //log.trace("@dep=%s", dep);

            Individual *dep_indv = l_individuals.get(dep.uri, null);

            if (dep_indv is null)
            {
                log.trace("ERR! not found dependency element [%s] in [%s], get_and_unpack", dep.uri, module_ttl_path);
                operation_result = OperationResult.FAIL;
                return;
            }

            UserModuleInfo duim = new UserModuleInfo(context, sticket, install_id);
            duim.get_and_unpack(*dep_indv);
            dependencies[ dep_indv.uri ] = duim;
            duim.parent                  = this;

            if (duim.operation_result != OperationResult.OK)
            {
                operation_result = duim.operation_result;
                //return;
            }
        }

        operation_result = OperationResult.OK;
        return;
    }
}

enum PreparedType
{
    MODULE,
    REQUEST,
    NONE
}


class UserModulesTool : VedaModule
{
    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        super(_subsystem_id, _module_id, log);
    }

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id, long transaction_id, long op_id, long count_pushed, long count_popped)
    {
        if (event_id == umt_event_id /*|| user_uri == "cfg:VedaSystem"*/) // принимаем команды только от пользователей, umt_event_id игнорируется
            return ResultCode.OK;

        tmp_installed_modules = (UserModuleInfo[ string ]).init;

        try
        {
            ResultCode   operation_result = ResultCode.OK;

            Resources    types = new_indv.getResources("rdf:type");
            PreparedType ptype = PreparedType.NONE;

            foreach (type; types)
            {
                if (type.uri == "v-s:Module")
                {
                    ptype = PreparedType.MODULE;
                    break;
                }
                if (type.uri == request_predicate)
                {
                    ptype = PreparedType.REQUEST;
                    break;
                }

                // else if (context.get_onto().isSubClasses(type.uri, ["v-s:Module", request_predicate]))
//                {
//                    need_prepare = true;
//                    break;
//                }
            }

            if (ptype == PreparedType.NONE)
                return ResultCode.OK;

            log.trace("[%s]: prepare, event_id=%s, user_id=%s", new_indv.uri, event_id, user_uri);

            if (ptype == PreparedType.MODULE)
                log.trace("[%s]: v-s:Module individual changed", new_indv.uri);

            if (ptype == PreparedType.REQUEST)
                log.trace("[%s]: %s individual changed", request_predicate, new_indv.uri);

            bool new_is_deleted = new_indv.isExists("v-s:deleted", true);

            if (prev_indv is Individual.init)
            {
                // New

                if (new_is_deleted == true)
                {
                    log.trace("individual [%s] already deleted, nothing", new_indv.uri);
                    return ResultCode.OK;
                }

                if (ptype == PreparedType.REQUEST)
                {
                    install_user_module(new_indv);
                }
                else
                {
                    Individual request = create_request(new_indv.getFirstLiteral("v-s:moduleUrl"), RequestCommand.INSTALL);

                    OpResult   orc = context.update(null, -1, &sticket, INDV_OP.PUT, &request, "", ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

                    if (orc.result != ResultCode.OK)

                        log.trace("ERR! can not store request %s, err=%s", request.uri, orc.result);
                    else
                        log.trace("store request %s", request.uri);
                }

                return ResultCode.OK;
            }
            else
            {
                // Exist
                bool prev_is_deleted = prev_indv.isExists("v-s:deleted", true);

                if (new_is_deleted == true && prev_is_deleted == false)
                {
                    if (ptype == PreparedType.MODULE)
                    {
                        if (uninstall_user_module(new_indv.uri) == false)
                        {
                            OpResult orc = context.update(null, -1, &sticket, INDV_OP.PUT, &prev_indv, umt_event_id, ALL_MODULES, OptFreeze.NONE, OptAuthorize.NO);

                            if (orc.result != ResultCode.OK)
                                log.trace("ERR! can not restore %s, err=%s", prev_indv.uri, orc.result);
                            else
                                log.trace("restore %s", prev_indv.uri);
                        }
                    }

                    return ResultCode.OK;
                }
                else
                if (new_is_deleted == false && prev_is_deleted == true)
                {
                    if (ptype == PreparedType.MODULE)
                        install_user_module(new_indv);

                    return ResultCode.OK;
                }
                else
                if (new_is_deleted == true && prev_is_deleted == true)
                {
                    if (ptype == PreparedType.MODULE)
                        log.trace("module already deleted, nothing");
                    return ResultCode.OK;
                }
                else
                if (new_is_deleted == false && prev_is_deleted == false)
                {
                    if (ptype == PreparedType.MODULE)
                        log.trace("module changed, nothing");
                    return ResultCode.OK;
                }
            }
        }
        catch (Throwable tr)
        {
            log.trace("ERR! %s %s", tr.msg, tr.info);
        }

        committed_op_id = op_id;
        return ResultCode.OK;
    }

    override void thread_id()
    {
    }

    override void receive_msg(string msg)
    {
    }

    override Context create_context()
    {
        return null;
    }


    override bool open()
    {
        return true;
    }

    override bool configure()
    {
        log.trace("use configuration: %s", node);
        return true;
    }

    override bool close()
    {
        return true;
    }

    override void event_of_change(string uri)
    {
        configure();
    }

    private void install_user_module(ref Individual new_indv)
    {
        Ticket         sticket = context.sys_ticket();

        string         install_id = "veda-install-" ~ randomUUID().toString();

        UserModuleInfo im = new UserModuleInfo(context, sticket, install_id);

        log.trace("--- 1 GET AND UNPACK MODULES ---");
        im.get_and_unpack(new_indv);

        if (im.operation_result == OperationResult.OK)
        {
            log.trace("--- 2 CHECK MODULES ---");
            im.check();

            log.trace("--- 3 UNINSTALL ---");
            im.uninstall(im);
            if (im.operation_result == OperationResult.OK)
            {
                log.trace("--- 4 INSTALL ---");
                im.install();
            }
            log.trace("--- 5 FINISH ---");
        }
        else if (im.operation_result == OperationResult.FAIL)
            log.trace("installation of module [%s][%s] failed", im.url, im.ver);
    }

    private bool uninstall_user_module(string module_id)
    {
        Ticket         sticket = context.sys_ticket();

        UserModuleInfo im = new UserModuleInfo(context, sticket, null);

        UserModuleInfo[ string ] installer_modules = im.get_installed_modules();

        auto target_module = installer_modules.get(module_id, null);
        if (target_module !is null)
        {
            target_module.uninstall(target_module);

            if (target_module.operation_result != OperationResult.OK)
                return false;
            else
                return true;
        }
        return false;
    }
}


