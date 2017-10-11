/**
 * user modules manager
 */
module veda.ttlreader.user_modules_tool;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule;

void user_modules_tool_thread()
{
    auto p_module = new UserModulesTool(SUBSYSTEM.USER_MODULES_TOOL, MODULE.user_modules_tool, new Logger("veda-core-users-modules-tool", "log", ""));

    p_module.run();
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
        //log.trace("[%s]: start prepare", new_indv.uri);

        //scope (exit)
        //{
        //    log.trace("[%s]: end prepare", new_indv.uri);
        //}

        ResultCode res = ResultCode.OK;

        Resources  types        = new_indv.getResources("rdf:type");
        bool       need_prepare = false;

        foreach (type; types)
        {
            if (context.get_onto().isSubClasses(type.uri, [ "v-s:Module" ]))
            {
                need_prepare = true;
                break;
            }
        }


        if (!need_prepare)
            return ResultCode.OK;

        log.trace("[%s]: v-s:Module individual changed", new_indv.uri);

        if (prev_indv is Individual.init)
            log.trace("is new module");
        else
        {
            bool new_is_deleted  = new_indv.exists("v-s:deleted", true);
            bool prev_is_deleted = prev_indv.exists("v-s:deleted", true);

            if (new_is_deleted == true && prev_is_deleted == false)
            {
                log.trace("module marked as deleted, uninstall");
            }
            else
            if (new_is_deleted == false && prev_is_deleted == true)
            {
                log.trace("module unmarked as deleted, install");
            }
            else

            if (new_is_deleted == true && prev_is_deleted == true)
            {
                log.trace("module is deleted, nothing");
            }
            else

            if (new_is_deleted == false && prev_is_deleted == false)
            {
                log.trace("module changed");
            }
        }

        Ticket sticket = context.sys_ticket();



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
}


