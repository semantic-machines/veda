import std.stdio, core.stdc.stdlib, std.uuid;
import veda.util.queue, veda.common.logger;

/*
    COMMAND NAME PATH [OPTIONS..]

    COMMAND: check, cat, repair

    check

        EXAMPLE: vqueuectl check individuals-flow ./data/queue

 */

Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("vqueuectl", "log", "");
    return _log;
}


void main(string[] args)
{
    if (args.length < 3)
    {
        writeln("use COMMAND NAME DIR [OPTIONS..]");
        writeln("		EXAMPLE: vqueuectl check individuals-flow data/queue");
        return;
    }

    string command = args[ 1 ];
    string name    = args[ 2 ];
    string path    = args[ 3 ];

    writefln("cmd=%s, name=%s, path=%s", command, name, path);

    Queue queue = new Queue(path, name, Mode.R, log);
    queue.open();
    queue.get_info(0);

    writefln("QUEUE %s content %d elements ", name, queue.count_pushed);

    string   consumer_path = "./tmp";

    Consumer cs = new Consumer(queue, consumer_path, "cs-" ~ name, Mode.RW, log);
    cs.open();

    if (queue.count_pushed <= 0)
        return;

    double oprc = 100.0 / queue.count_pushed;
    long   count;

    Queue  queue_new;

    if (command == "repair")
    {
        queue_new = new Queue(path ~ "/repair", name, Mode.RW, log);
        queue_new.open();
        queue_new.get_info(0);
    }

    while (true)
    {
        string data = cs.pop();

        if (data is null)
            break;

        count++;

        auto prc = count * oprc;

        if (count % 100000 == 0)
            writefln("%s %3.2f%% %d", command, prc, count);

        cs.commit_and_next(true);

        queue_new.push(data);
    }

    queue_new.close();
    cs.close();
    queue.close();
}
