/**
 * user modules manager
 */
module veda.ttlreader.user_modules_tool;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread, std.net.curl;
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

enum ErrCode
{
    OK,
    FOUND_ANOTHER_VERSION,
    FAIL
}

class UserModuleInfo
{
    Individual *[ string ] module_individuals;
    string     url;
    string     ver;
    string     project_name;
    string     project_owner;
    string     modile_temp_dir;
    string     module_file_path;
    string     unpacked_module_folder_name;
    string     ghrl_url;
    string     releases_path;
    string     install_id;
    Individual module_indv;

    UserModuleInfo[ string ] dependecies;

    ErrCode res;

    Context context;
    Ticket  sticket;

    this(Context _context, Ticket _sticket, string _install_id, Individual _module_indv)
    {
        module_indv = _module_indv;
        install_id  = _install_id;
        context     = _context;
        sticket     = _sticket;
    }

    void get_check_and_unpack_module()
    {
        log.trace("\nprepare %s", module_indv);

        url = module_indv.getFirstLiteral("v-s:moduleUrl");
        ver = module_indv.getFirstLiteral("v-s:moduleVersion");

        auto o_url = url.parseURL;
        if (o_url.host == "github.com")
        {
            string[] pp = o_url.path.split('/');

            if (pp.length != 3)
            {
                log.trace("ERR! unknown url format [%s], break installation", url);
                res = ErrCode.FAIL;
                return;
            }

            project_owner = pp[ 1 ];
            project_name  = pp[ 2 ];

            log.trace("this project [%s][%s] on github.com", project_owner, project_name);
            modile_temp_dir  = tempDir() ~ "/" ~ install_id ~ "/" ~ project_owner ~ "-" ~ project_name;
            module_file_path = modile_temp_dir ~ "/module.zip";

            try
            {
                mkdirRecurse(modile_temp_dir);
            }
            catch (Throwable tr)
            {
                log.trace("ERR! %s can't create tmp folder %s, break installation", tr.msg, modile_temp_dir);
                res = ErrCode.FAIL;
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
                log.trace("ERR! fail read json file of releases [%s], break installation", releases_path);
                res = ErrCode.FAIL;
                return;
            }

            JSONValue jva = parseJSON(js_releases);
            string[ string ] ver_2_url;
            string[ int ] pos_2_ver;
            string module_url;

            int    pos = 0;
            foreach (jv; jva.array())
            {
                string tag_name = jv[ "tag_name" ].str;
                ver_2_url[ tag_name ] = jv[ "zipball_url" ].str;
                pos_2_ver[ pos ]      = tag_name;
                pos++;
            }

            if (ver is null)
                ver = pos_2_ver[ 0 ];

            module_url = ver_2_url.get(ver, string.init);

            log.trace("@ ver_2_url=%s pos_2_ver=%s", ver_2_url, pos_2_ver);
            log.trace("@ module_url=[%s]", module_url);
            log.trace("@ ver=[%s]", ver);

            if (module_url is null)
            {
                log.trace("ERR! fail read module url from json file [%s], break installation", releases_path);
                res = ErrCode.FAIL;
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
                log.trace("ERR! fail unpack module [%s], break installation", unpack_cmd);
                res = ErrCode.FAIL;
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
            log.trace("ERR! fail unpack module [%s], break installation", module_file_path);
            res = ErrCode.FAIL;
            return;
        }

        if (unpacked_module_folder_name[ $ ] == '/')
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
            log.trace("ERR! not found root element [v-s:moduleUrl=%s] in [%s], break installation", url, module_ttl_path);
            res = ErrCode.FAIL;
            return;
        }

        // check onto

        auto onto_files = dirEntries(unpacked_module_folder_name ~ "/onto", SpanMode.depth);
        foreach (file; onto_files)
        {
            log.trace("check file=%s", file);

            auto tmp_individuals = ttl2individuals(file, prefixes, prefixes, log);

            foreach (uid; tmp_individuals.keys)
            {
                log.trace("check individual [%s]", uid);
                Individual indv_in_storage = context.get_individual(&sticket, uid);
                if (indv_in_storage.getStatus() == ResultCode.OK)
                {
                    string is_defined_by = indv_in_storage.getFirstLiteral("rdfs:isDefinedBy");

                    if (is_defined_by != module_indv.uri)
                    {
                        // [rdfs:isDefinedBy] is not equal to the installed uid module, we check the possibility of replacement
                        Individual indv_module = context.get_individual(&sticket, is_defined_by);
                        if (indv_module.getStatus() == ResultCode.OK)
                        {
                            if (indv_module.exists("rdf:type", "v-s:Module") == true)
                            {
                                log.trace("[%s] already exist, and rdfs:isDefinedBy=%s, break installation", uid, is_defined_by);
                                res = ErrCode.FOUND_ANOTHER_VERSION;
                                return;
                            }
                        }
                    }
                    else
                    {
                        res = ErrCode.FOUND_ANOTHER_VERSION;
                        return;
                    }
                }
                else if (indv_in_storage.getStatus() != ResultCode.Not_Found && indv_in_storage.getStatus() != ResultCode.Unprocessable_Entity)
                {
                    log.trace("ERR! [%s] already exist, but not read, errcode=%s", uid, indv_in_storage.getStatus());
                }

                Individual *indv_0 = tmp_individuals[ uid ];

                indv_0.setResources("rdfs:isDefinedBy", [ Resource(DataType.Uri, module_indv.uri) ]);
                module_individuals[ uid ] = indv_0;
            }
        }


        log.trace("@root indv=%s", root_indv);

        // go tree
        Resources deps = l_individuals[ root_indv ].getResources("v-s:dependency");

        foreach (dep; deps)
        {
            Individual *dep_indv = l_individuals.get(dep.uri, null);

            if (dep_indv is null)
            {
                log.trace("ERR! not found dependency element [%s] in [%s], break installation", dep.uri, module_ttl_path);
                res = ErrCode.FAIL;
                return;
            }

            UserModuleInfo duim = new UserModuleInfo(context, sticket, install_id, *dep_indv);
            duim.get_check_and_unpack_module();
            dependecies[ duim.module_indv.uri ] = duim;

            if (duim.res == ErrCode.FAIL)
            {
                res = ErrCode.FAIL;
                return;
            }
        }

        res = ErrCode.OK;
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
        //log.trace("[%s]: start prepare", module_indv.uri);

        //scope (exit)
        //{
        //    log.trace("[%s]: end prepare", module_indv.uri);
        //}

        ResultCode res = ResultCode.OK;

        Resources  types = new_indv.getResources("rdf:type");
        //log.trace("[%s]: types: %s", module_indv.uri, types);
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
                log.trace("module already deleted, nothing");
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
                log.trace("module marked as deleted, uninstall");
                install_user_module(new_indv);
                return ResultCode.OK;
            }
            else
            if (new_is_deleted == false && prev_is_deleted == true)
            {
                log.trace("module unmarked as deleted, install");
                uninstall_user_module(new_indv);
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

        if (res == ResultCode.OK)
        {
            committed_op_id = op_id;
            return ResultCode.OK;
        }
        else
            return ResultCode.Fail_Commit;
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
        Ticket sticket = context.sys_ticket();

        string install_id = "veda-install-" ~ randomUUID().toString();

        Individual *[ string ] module_individuals;

        UserModuleInfo installed_module = new UserModuleInfo(context, sticket, install_id, new_indv);
        installed_module.get_check_and_unpack_module();

        bool[ string ] installed;
        bool is_sucess = true;

        if (installed_module.res == ErrCode.OK)
        {
            log.trace("check module and dependency is Ok, now install it");

            foreach (uid; module_individuals.keys)
            {
                OpResult orc = context.put_individual(&sticket, uid, *module_individuals[ uid ], null, -1, ALL_MODULES, OptFreeze.NONE,
                                                      OptAuthorize.NO);

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
                //fail installation, remove installed individuals
            }
            else
                log.trace("installation module [%s][%s] is success", installed_module.url, installed_module.ver);


            // install
        }
        else if (installed_module.res == ErrCode.FAIL)
            log.trace("installation module [%s][%s] if fail", installed_module.url, installed_module.ver);
        else if (installed_module.res == ErrCode.FOUND_ANOTHER_VERSION)
        {
            log.trace("need uninstall module [%s][%s]", installed_module.url, installed_module.ver);
        }
    }

    private void uninstall_user_module(ref Individual new_indv)
    {
    }
}


