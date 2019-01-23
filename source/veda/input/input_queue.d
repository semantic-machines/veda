module veda.input.input_queue;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import veda.util.properd;
private import veda.common.type, veda.core.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual,
               veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context, veda.search.ft_query.ft_query_client;
private import veda.core.common.context;
private import veda.vmodule.vmodule;

void main(char[][] args)
{
    process_name = "input-queue";

    Thread.sleep(dur!("seconds")(1));

    InputQueueProcess p_input_queue = new InputQueueProcess(SUBSYSTEM.NONE, MODULE.input_queue, new Logger("veda-core-input-queue", "log", ""));

    p_input_queue.run();
}

class InputQueueProcess : VedaModule
{
    string main_queue_path;
    string my_consumer_path = "data/input-queue";
    ubyte  target_maks;

    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        string path = "./logs";

        string[ string ] properties;
        properties = readProperties("./veda.properties");

        main_queue_path = properties.as!(string)("input_queue_path");

        try
        {
            mkdir(my_consumer_path);
            stderr.writeln("create folder: ", my_consumer_path);
        }
        catch (Exception ex)
        {
        }

        super(_subsystem_id, _module_id, log, main_queue_path, my_consumer_path);
    }

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv,
                                string new_bin, ref Individual new_indv,
                                string event_id, long transaction_id, long op_id, long count_pushed,
                                long count_popped, long opid_on_start, long count_from_start)
    {
        string uri;

        if (count_from_start == 1)
        {
            log.trace("INFO: start opid=%d", opid_on_start);
            if (op_id - opid_on_start != 1)
            {
                log.trace("ERR: cur_opid (%d) != start opid (%d + 1)", op_id, opid_on_start);
                return ResultCode.InternalServerError;
            }
        }

        if (new_indv.uri is null)
            uri = prev_indv.uri;
        else
            uri = new_indv.uri;

        log.trace("INFO: opid=%d, cmd=%s, uri=%s", op_id, cmd, uri);

        auto sticket = context.sys_ticket();

        //cmd = INDV_OP.PUT;

        auto rc = context.update(null, transaction_id, &sticket, cmd, &new_indv, event_id, target_maks, OptFreeze.NONE, OptAuthorize.NO).result;

        if (rc != ResultCode.Ok)
        {
            log.trace("ERR! fail update [%s]", uri);
        }

        return ResultCode.Ok;
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
        //context.set_vql (new XapianSearch(context));
        context.set_vql(new FTQueryClient(context));

        return true;
    }

    override bool configure()
    {
        log.trace("use configuration: %s", node);

        if (main_queue_path is null || main_queue_path.length < 3)
        {
            log.trace("ERR! not set variable [input_queue_path] in veda.properties");
            return false;
        }

        string[ string ] properties;
        properties = readProperties("./veda.properties");

        string t_targets_for_input_flow = properties.as!(string)("targets_for_input_flow");
        if (t_targets_for_input_flow !is null && t_targets_for_input_flow.length > 2)
        {
            string[] els = t_targets_for_input_flow.split(",");
            foreach (el; els)
                target_maks = target_maks | get_subsystem_id_of_name(el);

            log.trace("INFO! target subsystems=%s", subsystem_byte_to_string(target_maks));
        }

        log.trace("use input_queue_path=%s", main_queue_path);

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
