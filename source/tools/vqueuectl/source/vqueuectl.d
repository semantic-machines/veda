import std.stdio, core.stdc.stdlib, std.uuid;
import veda.util.queue, veda.common.logger, veda.onto.individual;

/*
    COMMAND NAME PATH [OPTIONS..]

    COMMAND: check, cat, repair, stat

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
        writeln("use %s COMMAND NAME DIR [OPTIONS..]", args[0]);
        writeln("		EXAMPLE: vqueuectl check individuals-flow data/queue");
        return;
    }

    string command = args[ 1 ];
    if (command != "check" && command != "cat" && command != "repair" && command != "stat")
    {
        writeln("use %s COMMAND NAME DIR [OPTIONS..]", args[0]);
        writeln("		COMMAND : [check/cat/repair/stat]");
        return;
	}
    
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
        queue_new = new Queue("./tmp/" ~ name ~ "/repair", name, Mode.RW, log);
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

        if (command == "repair")
            queue_new.push(data);
		else if (command == "cat")
            writeln(data);
		else if (command == "stat")
            collect_stat(data);
    }

    if (command == "repair")
        queue_new.close();

    cs.close();
    queue.close();
}

long[string] type_2_count;

private void collect_stat (string data)
{
            Individual imm;
            if (data !is null && imm.deserialize(data) < 0)
            {
                log.trace("ERR! read in queue: invalid individual:[%s]", data);
            }
            else
            {
				string new_bin             = imm.getFirstLiteral("new_state");
				
            if (new_bin !is null && new_indv.deserialize(new_bin) < 0)
            {
                log.trace("ERR! read in queue, new binobj is individual:[%s]", new_bin);
            }
            else
            {
				Resources[] types = new_indv.getFirstLiteral("rdf:type");
				
				foreach (type ; types)
				{
					string stype = type.data;
					long count = type_2_count.get (stype, 0);
					type_2_count[stype] = count + 1;
				}
			}				
			
			
			
			}	
}

private void print_stat ()
{
	
}
