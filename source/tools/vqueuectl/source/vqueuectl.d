import std.stdio, core.stdc.stdlib, std.uuid;
import veda.util.queue;

/*
    COMMAND NAME PATH [OPTIONS..]

    COMMAND: check, cat


    check

        EXAMPLE: vqueuectl check individuals-flow ./data/queue

 */

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

    Queue queue = new Queue(path, name, Mode.R, null);
    queue.open();
    queue.get_info(0);

    writefln("QUEUE %s content %d elements ", name, queue.count_pushed);

    string   consumer_path = "./tmp";

    Consumer cs = new Consumer(queue, consumer_path, "cs-" ~ name, Mode.RW, null);
    cs.open();

    if (queue.count_pushed <= 0)
        return;

    double oprc = 100.0 / queue.count_pushed;
    writeln(oprc);
    long   count;

    while (true)
    {
        string data = cs.pop();

        if (data is null)
            break;

        count++;

        auto prc = count * oprc;

        if (count % 100000 == 0)
            writefln("check %3.2f%% %d", prc, count);

        cs.commit_and_next(true);
    }
}


