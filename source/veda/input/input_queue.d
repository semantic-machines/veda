module veda.input.input_queue;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import veda.util.properd;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
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

    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        string path = "./logs";

        string[ string ] properties;
        properties = readProperties("./veda.properties");

        main_queue_path = properties.as!(string)("input_queue_path");
        log.trace("use input_queue_path=%s", main_queue_path);

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

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id, long transaction_id, long op_id, long count_pushed, long count_popped)
    {
        log.trace("[%s]: start prepare", new_indv.uri);

        auto sticket = context.sys_ticket();

        auto rc = context.update(null, transaction_id, &sticket, cmd, &new_indv, event_id, 0, OptFreeze.NONE, OptAuthorize.NO).result;

        if (rc != ResultCode.OK)
        {
            log.trace("ERR! fail store [%s]", new_indv.uri);
        }

        //scope (exit)
        //{
        //    log.trace("[%s]: end prepare", new_indv.uri);
        //}

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
}


