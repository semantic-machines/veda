/**
 * user modules manager
 */
module veda.ttlreader.user_modules_tool;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread, std.net.curl, std.algorithm;
private import backtrace.backtrace, Backtrace = backtrace.backtrace, url, std.uuid, std.json, std.process;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.util.raptor2individual;
private import veda.vmodule.vmodule;

void user_modules_tool_thread()
{
    Thread.sleep(dur!("seconds")(3));

    auto p_module = new UserModulesTool(SUBSYSTEM.USER_MODULES_TOOL, MODULE.user_modules_tool, new Logger("veda-core-users-modules-tool", "log", ""));

    p_module.run();
}

const umt_event_id = "user_module_tool";

enum CheckResult
{
    OK,
    FOUND_INVALID_VERSION,
    FOUND_ANOTHER_VERSION,
    FOUND_EQUAL_VERSION,
    FAIL,
    NONE
}

class UserModuleInfo
{
    string uri;
    string url;
    string ver;
    UserModuleInfo[ string ] dependecies;

    Individual *[ string ] module_individuals;
    string      project_name;
    string      project_owner;
    string      modile_temp_dir;
    string      module_file_path;
    string      unpacked_module_folder_name;
    string      ghrl_url;
    string      releases_path;
    string      install_id;

    string      prev_module_name;

    string[]    total_hash_indv_file;
    string[]    total_hash_indv_storage;

    CheckResult check_res = CheckResult.NONE;

    Context     context;
    Ticket      sticket;
    Logger      log;

    override string toString()
    {
        string check_res = uri ~ ":" ~ ver;

        foreach (uid; dependecies.keys)
        {
            UserModuleInfo umi = dependecies.get(uid, null);
            if (umi !is null)
                check_res ~= " { " ~ umi.toString() ~ " }";
        }
        return check_res;
    }

    this(Context _context, Ticket _sticket, ref Individual module_indv)
    {
        context = _context;
        sticket = _sticket;

        if (context !is null)
            log = context.get_logger();
        else
            stderr.writeln("ERR! fail create object UserModuleInfo, log not initalized");

        uri = module_indv.uri;
        url = module_indv.getFirstLiteral("v-s:moduleUrl");
        ver = module_indv.getFirstLiteral("v-s:moduleVersion");
    }

    this(Context _context, Ticket _sticket)
    {
        context = _context;
        sticket = _sticket;

        if (context !is null)
            log = context.get_logger();
        else
            stderr.writeln("ERR! fail create object UserModuleInfo, log not initalized");
    }

    this(Context _context, Ticket _sticket, string _install_id)
    {
        install_id = _install_id;
        context    = _context;
        sticket    = _sticket;

        if (context !is null)
            log = context.get_logger();
        else
            stderr.writeln("ERR! fail create object UserModuleInfo, log not initalized");
    }

    public static UserModuleInfo[ string ] get_installed_modules_info(Context context, Ticket sticket)
    {
        UserModuleInfo[ string ] result;
        int[ string ] link_count_2_module_uid;

        SearchResult sr =
            context.get_individuals_ids_via_query(&sticket, "'rdf:type' === 'v-s:Module'", "'rdf:type' asc", "base,system", 0, 100000,
                                                  10000,
                                                  null,
                                                  false);

        foreach (uid; sr.result)
        {
            int clnk = link_count_2_module_uid.get(uid, -1);
            if (clnk == -1)
                link_count_2_module_uid[ uid ] = 0;

            Individual     module_info_ist = context.get_individual(&sticket, uid);
            UserModuleInfo umi             = new UserModuleInfo(context, sticket, module_info_ist);

            Resources      deps = module_info_ist.getResources("v-s:dependency");

            foreach (dep; deps)
            {
                UserModuleInfo dumi = result.get(dep.uri, null);
                if (dumi !is null)
                    umi.dependecies[ dep.uri ] = dumi;

                clnk                               = link_count_2_module_uid.get(dep.uri, 0);
                link_count_2_module_uid[ dep.uri ] = clnk + 1;
            }

            result[ uid ] = umi;
        }

        //context.get_logger().trace("link_count_2_module_uid=[%s]", link_count_2_module_uid);

        foreach (key; link_count_2_module_uid.keys)
        {
            if (link_count_2_module_uid[ key ] == 0)
                context.get_logger().trace("FOUND MODULE [%s]", result[ key ]);
        }

        return result;
    }

    bool uninstall()
    {
        foreach (dep; dependecies)
        {
            if (dep.uninstall() == false)
                return false;
        }

//        log.trace("@1 prev_module_name=%s", prev_module_name);

        string module_id = prev_module_name;

        if (module_id is null)
            return true;

        if (check_res != CheckResult.FOUND_ANOTHER_VERSION && check_res != CheckResult.NONE)
            return true;

        log.trace("UNINSTALL MODULE [%s], check result=%s", module_id, check_res);

        SearchResult sr =
            context.get_individuals_ids_via_query(&sticket, "'rdfs:isDefinedBy' === '" ~ module_id ~ "'", "'rdfs:isDefinedBy' asc",
                                                  "base,system,deleted", 0, 100000,
                                                  10000,
                                                  null,
                                                  false);

        //log.trace("found %s ", sr.result);

        bool is_success = true;

        foreach (uid; sr.result)
        {
            log.trace("REMOVE [%s] %s", prev_module_name, uid);
            OpResult check_res = context.remove_individual(&sticket, uid, umt_event_id, -1, ALL_MODULES, OptFreeze.NONE,
                                                           OptAuthorize.NO);
            if (check_res.result != ResultCode.OK)
            {
                log.trace("ERR! fail remove [%s], err=[%s]", uid, check_res.result);
                is_success = false;
                break;
            }
        }

        if (is_success == true)
        {
            OpResult check_res = context.remove_individual(&sticket, module_id, umt_event_id, -1, ALL_MODULES, OptFreeze.NONE,
                                                           OptAuthorize.NO);
            if (check_res.result != ResultCode.OK)
            {
                log.trace("ERR! fail remove [%s], err=[%s]", module_id, check_res.result);
                return false;
            }

            return true;
        }
        else
            return false;
    }

    bool install()
    {
        foreach (dep; dependecies)
        {
            if (dep.install() == false)
                return false;
        }

        if (check_res == CheckResult.FOUND_EQUAL_VERSION)
        {
            log.trace("MODULE [%s][%s] ALREADY INSTALLED", uri, ver);
            return true;
        }
        else
            log.trace("INSTALL MODULE [%s][%s] %d", uri, ver, module_individuals.keys.length);

        bool[ string ] installed;
        bool is_sucess = true;

        foreach (uid; module_individuals.keys)
        {
            OpResult orc = context.put_individual(&sticket, uid, *module_individuals[ uid ], umt_event_id, -1, ALL_MODULES, OptFreeze.NONE,
                                                  OptAuthorize.NO);
            log.trace("INSERT [%s][%s] %s", uri, ver, uid);

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
                context.remove_individual(&sticket, uid, umt_event_id, -1, ALL_MODULES, OptFreeze.NONE,
                                          OptAuthorize.NO);
            }

            return false;
        }
        else
            log.trace("installation module [%s][%s] was success", url, ver);

        Individual module_indv;
        module_indv.uri = uri;
        module_indv.setResources("rdf:type", [ Resource(DataType.Uri, "v-s:Module") ]);
        module_indv.setResources("v-s:moduleUrl", [ Resource(DataType.Uri, url) ]);
        module_indv.setResources("v-s:moduleVersion", [ Resource(DataType.Uri, ver) ]);
        foreach (dep; dependecies)
            module_indv.addResource("v-s:dependency", Resource(DataType.Uri, dep.uri));

        OpResult orc = context.put_individual(&sticket, uri, module_indv, umt_event_id, -1, ALL_MODULES, OptFreeze.NONE,
                                              OptAuthorize.NO);

        return true;
    }

    void check()
    {
        foreach (dep; dependecies)
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
            Individual indv_in_storage = context.get_individual(&sticket, uid);

            if (indv_in_storage.getStatus() == ResultCode.OK)
            {
                string hash_indv_storage = indv_in_storage.get_CRC32();
                total_hash_indv_storage ~= hash_indv_storage;

                string is_defined_by_in_storage = indv_in_storage.getFirstLiteral("rdfs:isDefinedBy");

                if (is_defined_by_in_storage != uri)
                {
                    // [rdfs:isDefinedBy] is not equal to the installed uid module, we check the possibility of replacement
                    Individual indv_module = context.get_individual(&sticket, is_defined_by_in_storage);
                    if (indv_module.getStatus() == ResultCode.OK)
                    {
                        if (indv_module.exists("rdf:type", "v-s:Module") == true)
                        {
                            log.trace("[%s] already exist, and found another installation %s, break check", uid, is_defined_by_in_storage);
                            prev_module_name = is_defined_by_in_storage;
                            check_res        = CheckResult.FOUND_ANOTHER_VERSION;
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
                        check_res        = CheckResult.FOUND_ANOTHER_VERSION;
                        return;
                    }
                }
            }
            else if (indv_in_storage.getStatus() != ResultCode.Not_Found && indv_in_storage.getStatus() != ResultCode.Unprocessable_Entity)
            {
                log.trace("ERR! [%s] already exist, but not read, errcode=%s", uid, indv_in_storage.getStatus());
                check_res = CheckResult.FOUND_INVALID_VERSION;
                return;
            }
        }

        total_hash_indv_storage.sort();
        total_hash_indv_file.sort();

        if (text(total_hash_indv_file) == text(total_hash_indv_storage))
        {
            log.trace("module [%s][%s] already installed", uri, ver);
            check_res = CheckResult.FOUND_EQUAL_VERSION;
        }
        else
        {
            log.trace("module [%s][%s] not equal installed", uri, ver);
            check_res = CheckResult.FOUND_ANOTHER_VERSION;
        }

        log.trace("module [%s][%s] check individuals %d", uri, ver, module_individuals.length);
    }


    void get_and_unpack(ref Individual module_indv)
    {
        log.trace("GET AND UNPACK MODULE [%s]", module_indv.uri);

        uri = module_indv.uri;
        url = module_indv.getFirstLiteral("v-s:moduleUrl");
        ver = module_indv.getFirstLiteral("v-s:moduleVersion");

        auto o_url = url.parseURL;
        if (o_url.host == "github.com")
        {
            string[] pp = o_url.path.split('/');

            if (pp.length != 3)
            {
                log.trace("ERR! unknown url format [%s], get_and_unpack", url);
                check_res = CheckResult.FAIL;
                return;
            }

            project_owner = pp[ 1 ];
            project_name  = pp[ 2 ];

            //log.trace("this project [%s][%s] on github.com", project_owner, project_name);
            modile_temp_dir  = tempDir() ~ "/" ~ install_id ~ "/" ~ project_owner ~ "-" ~ project_name;
            module_file_path = modile_temp_dir ~ "/module.zip";

            try
            {
                mkdirRecurse(modile_temp_dir);
            }
            catch (Throwable tr)
            {
                log.trace("ERR! %s can't create tmp folder %s, get_and_unpack", tr.msg, modile_temp_dir);
                check_res = CheckResult.FAIL;
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
                check_res = CheckResult.FAIL;
                return;
            }

            string module_url;
            string[ string ] ver_2_url;
            string[ int ] pos_2_ver;

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
                check_res = CheckResult.FAIL;
                return;
            }

            if (ver is null)
                ver = pos_2_ver[ 0 ];

            module_url = ver_2_url.get(ver, string.init);

            //log.trace("@ ver_2_url=%s pos_2_ver=%s", ver_2_url, pos_2_ver);
            //log.trace("@ module_url=[%s]", module_url);
            //log.trace("@ ver=[%s]", ver);

            if (module_url is null)
            {
                log.trace("ERR! fail read module url from json file [%s], get_and_unpack", releases_path);
                check_res = CheckResult.FAIL;
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
                check_res = CheckResult.FAIL;
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
            check_res = CheckResult.FAIL;
            return;
        }

        if (unpacked_module_folder_name[ $ - 1 ] == '/')
            unpacked_module_folder_name = unpacked_module_folder_name[ 0..$ - 1 ];

        // found module.ttl
        string[ string ] prefixes;
        string root_indv;
        string module_ttl_path = unpacked_module_folder_name ~ "/module.ttl";
        auto   l_individuals   = ttl2individuals(module_ttl_path, prefixes, prefixes, log);

        // found root individual of file module.ttl
        foreach (uid; l_individuals.keys)
        {
            Individual *indv = l_individuals[ uid ];

            if (module_indv.getFirstLiteral("v-s:moduleUrl") == url)
            {
                root_indv = uid;
                break;
            }
        }

        if (root_indv is null)
        {
            log.trace("ERR! not found root element [v-s:moduleUrl=%s] in [%s], get_and_unpack", url, module_ttl_path);
            check_res = CheckResult.FAIL;
            return;
        }

        //log.trace("@root indv=%s", root_indv);

        // go tree
        Resources deps = l_individuals[ root_indv ].getResources("v-s:dependency");

        foreach (dep; deps)
        {
            Individual *dep_indv = l_individuals.get(dep.uri, null);

            if (dep_indv is null)
            {
                log.trace("ERR! not found dependency element [%s] in [%s], get_and_unpack", dep.uri, module_ttl_path);
                check_res = CheckResult.FAIL;
                return;
            }

            UserModuleInfo duim = new UserModuleInfo(context, sticket, install_id);
            duim.get_and_unpack(*dep_indv);
            dependecies[ dep_indv.uri ] = duim;

            if (duim.check_res != CheckResult.OK)
            {
                check_res = duim.check_res;
                //return;
            }
            else
            {
                auto onto_files = dirEntries(unpacked_module_folder_name ~ "/onto", SpanMode.depth);
                foreach (file; onto_files)
                {
                    log.trace("[%s] prepare %s", uri, file);

                    auto tmp_individuals = ttl2individuals(file, prefixes, prefixes, log);

                    foreach (uid; tmp_individuals.keys)
                    {
                        Individual *indv_0 = tmp_individuals[ uid ];
                        indv_0.setResources("rdfs:isDefinedBy", [ Resource(DataType.Uri, uri) ]);

                        string hash_indv_file = indv_0.get_CRC32();
                        total_hash_indv_file ~= hash_indv_file;
                        module_individuals[ uid ] = indv_0;
                    }
                }

                log.trace("module [%s][%s] load individuals %d", uri, ver, module_individuals.length);
            }
        }

        check_res = CheckResult.OK;
        return;
    }
}

class UserModulesTool : VedaModule
{
    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        super(_subsystem_id, _module_id, log);
    }

    override ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id, long transaction_id, long op_id)
    {
        //log.trace("[%s]: prepare, event_id=%s", new_indv.uri, event_id);

        if (event_id == umt_event_id) // принимаем команды только от пользователей, umt_event_id игнорируется
            return ResultCode.OK;

        try
        {
            ResultCode check_res = ResultCode.OK;

            Resources  types        = new_indv.getResources("rdf:type");
            bool       need_prepare = false;

            foreach (type; types)
            {
                if (type.uri == "v-s:Module")
                {
                    need_prepare = true;
                    break;
                }
                else if (context.get_onto().isSubClasses(type.uri, []))
                {
                    need_prepare = true;
                    break;
                }
            }

            if (!need_prepare)
                return ResultCode.OK;

            log.trace("[%s]: v-s:Module individual changed", new_indv.uri);

            bool new_is_deleted = new_indv.exists("v-s:deleted", true);

            if (prev_indv is Individual.init)
            {
                if (new_is_deleted == true)
                {
                    log.trace("module [%s] already deleted, nothing", new_indv.uri);
                    return ResultCode.OK;
                }

                log.trace("is new module, install");
                install_user_module(new_indv);
                return ResultCode.OK;
            }
            else
            {
                bool prev_is_deleted = prev_indv.exists("v-s:deleted", true);

                if (new_is_deleted == true && prev_is_deleted == false)
                {
                    uninstall_user_module(new_indv.uri);
                    return ResultCode.OK;
                }
                else
                if (new_is_deleted == false && prev_is_deleted == true)
                {
                    uninstall_user_module(new_indv.uri);
                    return ResultCode.OK;
                }
                else

                if (new_is_deleted == true && prev_is_deleted == true)
                {
                    log.trace("module already deleted, nothing");
                    return ResultCode.OK;
                }
                else

                if (new_is_deleted == false && prev_is_deleted == false)
                {
                    log.trace("module changed");
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
        log.trace("--- 1 GET INSTALLED MODULES ---");

        Ticket sticket = context.sys_ticket();

        UserModuleInfo.get_installed_modules_info(context, sticket);

        string         install_id = "veda-install-" ~ randomUUID().toString();

        UserModuleInfo im = new UserModuleInfo(context, sticket, install_id);

        log.trace("--- 2 GET AND UNPACK MODULES ---");
        im.get_and_unpack(new_indv);

        if (im.check_res == CheckResult.OK)
        {
            log.trace("--- 3 CHECK MODULES ---");
            im.check();

            log.trace("--- 4 UNINSTALL ---");
            im.uninstall();

            log.trace("--- 5 INSTALL ---");
            im.install();
        }
        else if (im.check_res == CheckResult.FAIL)
            log.trace("installation of module [%s][%s] failed", im.url, im.ver);
    }

    private void uninstall_user_module(string module_id)
    {
        Ticket         sticket = context.sys_ticket();
        UserModuleInfo im      = new UserModuleInfo(context, sticket, module_id);

        im.prev_module_name = module_id;
        im.uninstall();
    }
}


