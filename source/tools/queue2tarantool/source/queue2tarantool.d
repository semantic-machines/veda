import std.stdio, core.stdc.stdlib, std.uuid;
import std.stdio, std.file, std.datetime.stopwatch, std.conv, std.digest.ripemd, std.bigint, std.string, std.uuid, core.memory;
alias core.thread.Thread core_thread;
import veda.core.common.define;
import veda.storage.tarantool.tarantool_driver, veda.storage.common, veda.common.type, veda.onto.individual;
import veda.util.properd, veda.util.queue;
import veda.common.logger;

Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("queue2tarantool", "log", "");
    return _log;
}

bool[ string ] opt;
double delta_to_print_count = 10000;

void main(string[] args)
{
//    if (args.length < 5)
//    {
//        stderr.writeln("use queue2tarantool [start_pos] [delta] [batch_size] [opt]");
//        return;
//    }

    if (args.length > 4)
    {
        for (int idx = 4; idx < args.length; idx++)
        {
            string el = args[ idx ];
            opt[ el ] = true;
        }
    }

    log.trace("opt: %s", opt);

    KeyValueDB individual_tt_storage;
    KeyValueDB ticket_tt_storage;

    string[ string ] properties;
    properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    log.trace("connect to tarantool");
    if (tarantool_url !is null)
    {
        individual_tt_storage = new TarantoolDriver(log, "INDIVIDUALS", 512);
        ticket_tt_storage     = new TarantoolDriver(log, "TICKETS", 513);
    }

    convert(individual_tt_storage, opt);
}


public long convert(KeyValueDB dest, bool[ string ] opt)
{
    long count;

    auto individual_queue = new Queue("./input/queue", "individuals", Mode.R, log);

    if (individual_queue.open() == false)
    {
        log.trace("ERR! fail open queue");
        return -1;
    }

    auto new_id        = "cs_0";
    auto individual_cs = new Consumer(individual_queue, "./", new_id ~ "", Mode.RW, log);
    if (individual_cs.open() == false)
    {
        log.trace("ERR! fail open consumer");
        return -1;
    }

    count = individual_cs.count_popped;
    auto sw = StopWatch(AutoStart.no);

    while (true)
    {
        string data = individual_cs.pop();

        if (individual_cs.isReady == false)
        {
            log.trace("ERR! consumer not ready");
            break;
        }

        if (data is null)
            break;

        if (count % delta_to_print_count == 0)
        {
            long tt = sw.peek.total !"msecs";
            sw.reset();

            auto cps = (delta_to_print_count / tt * 1000);
            log.trace("count=%d, cps=%s", count, cps);
        }

        count++;
        Individual indv;
        if (indv.deserialize(data) < 0)
        {
            log.trace("ERR! %d DATA=[%s]", count, data);
        }
        else
        {
            bool need_store = true;
            if (opt.get("check", false))
            {
                Individual indv1;

                sw.start();
                dest.get_individual(indv.uri, indv1);
                sw.stop();

                if (indv1.getStatus() != ResultCode.Ok)
                    need_store = true;
                else
                    need_store = false;

                if (opt.get("trace", false))
                {
                    log.trace("TRACE, %d KEY=[%s] INDV+[%s]", individual_cs.count_popped, indv.uri, indv1);
                }
            }

            if (need_store == true)
            {
                string new_bin = indv.serialize();
                dest.store(indv.uri, new_bin, -1);
                log.trace("OK, %d KEY=[%s]", individual_cs.count_popped, indv.uri);
            }
        }


        individual_cs.commit_and_next(true);
    }

    log.trace("count=%d", count);

    return count;
}

