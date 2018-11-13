import std.stdio, core.stdc.stdlib, std.uuid, std.algorithm, std.typecons, std.json, std.conv;
import veda.util.queue, veda.common.logger, veda.onto.individual, veda.onto.resource;
import veda.storage.lmdb.lmdb_driver, veda.storage.lmdb.lmdb_header, veda.storage.common, veda.common.type, veda.onto.bj8individual.individual8json;

/*
    COMMAND NAME PATH [OPTIONS..]

    COMMAND: check, message_to_json, repair, stat, check_links

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

double     oprc;
LmdbDriver individual_lmdb_driver;

void main(string[] args)
{
    bool[ string ] cmds;
    cmds[ "check" ]           = true;
    cmds[ "message_to_json" ] = true;
    cmds[ "repair" ]          = true;
    cmds[ "stat" ]            = true;
    cmds[ "check_links" ]     = true;

    if (args.length < 3)
    {
        writeln("use %s COMMAND NAME DIR [OPTIONS..]", args[ 0 ]);
        writeln("		EXAMPLE: vqueuectl check individuals-flow data/queue");
        return;
    }

    string command = args[ 1 ];

    if (cmds.get(command, false) == false)
    {
        writeln("use %s COMMAND NAME DIR [OPTIONS..]", args[ 0 ]);
        writeln("		COMMAND : [check/cat/repair/stat/check_links]");
        return;
    }

    string name = args[ 2 ];
    string path = args[ 3 ];

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

    oprc = 100.0 / queue.count_pushed;
    long  count;

    Queue queue_new;

    if (command == "check_links")
        individual_lmdb_driver = new LmdbDriver("./data/lmdb-individuals", DBMode.R, "vqueuectl", log);

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
        {
            writefln("%s %3.2f%% %d", command, prc, count);
        }

        cs.commit_and_next(true);

        if (command == "repair")
            queue_new.push(data);
        else if (command == "message_to_json")
            message_to_json(data);
        else if (command == "stat")
            collect_stat(data);
        else if (command == "check_links")
            check_links(data);
    }

    if (command == "stat")
        print_stat();

    if (command == "repair")
        queue_new.close();

    cs.close();
    queue.close();
}

long[ string ] type_2_count;

private void check_links(string data)
{
    Individual imm;

    if (data !is null && imm.deserialize(data) < 0)
    {
        log.trace("ERR! read in queue: invalid individual:[%s]", data);
    }
    else
    {
        string     new_bin = imm.getFirstLiteral("new_state");

        Individual new_indv;

        if (new_bin !is null && new_indv.deserialize(new_bin) < 0)
        {
            log.trace("ERR! read in queue, new binobj is individual:[%s]", new_bin);
        }
        else
        {
            Resources types = new_indv.getResources("rdf:type");

            string    stypes = "";
            foreach (type; types)
            {
                string stype = type.data;
                stypes ~= " " ~ stype;
            }

            int all_count_uri  = 0;
            int good_count_uri = 0;
            foreach (key, rss; new_indv.resources)
            {
                foreach (rs; rss)
                {
                    if (rs.type == DataType.Uri)
                    {
                        all_count_uri++;
                        string data = individual_lmdb_driver.get_binobj(rs.uri);
                        if (data is null)
                        {
//							log.trace ("uri %s not found in predicate %s, indv.uri=%s", rs, key, new_indv.uri);
                        }
                        else
                        {
                            good_count_uri++;
                        }
                    }
                }
            }

            if ((all_count_uri > 0 && all_count_uri - good_count_uri > 2) || (all_count_uri > 0 && good_count_uri == 0))
            {
                log.trace("ERR! %d, fail links %d, uri=[%s], type=[%s]", all_count_uri, all_count_uri - good_count_uri, new_indv.uri, stypes);
            }
        }
    }
}

private void collect_stat(string data)
{
    Individual imm;

    if (data !is null && imm.deserialize(data) < 0)
    {
        log.trace("ERR! read in queue: invalid individual:[%s]", data);
    }
    else
    {
        string     new_bin = imm.getFirstLiteral("new_state");

        Individual new_indv;

        if (new_bin !is null && new_indv.deserialize(new_bin) < 0)
        {
            log.trace("ERR! read in queue, new binobj is individual:[%s]", new_bin);
        }
        else
        {
            Resources types = new_indv.getResources("rdf:type");
            foreach (type; types)
            {
                string stype = type.data;
                long   count = type_2_count.get(stype, 0);
                type_2_count[ stype ] = count + 1;
            }
        }
    }
}

private void message_to_json(string data)
{
    Individual imm;

    if (data !is null && imm.deserialize(data) < 0)
    {
        log.trace("ERR! read in queue: invalid individual:[%s]", data);
    }
    else
    {
        string     new_bin = imm.getFirstLiteral("new_state");

        Individual new_indv;
        if (new_bin !is null && new_indv.deserialize(new_bin) < 0)
        {
            log.trace("ERR! read in queue, new binobj is individual:[%s]", new_bin);
        }
        else
        {
            JSONValue jj             = individual_to_json(new_indv);
            string    user           = imm.getFirstLiteral("user_uri");
            string    type           = new_indv.getFirstLiteral("rdf:type");
            long      update_counter = new_indv.getFirstInteger("v-s:updateCounter");
            bool      deleted        = new_indv.getFirstBoolean("v-s:deleted");

            log.trace("uri=%s, user=%s, type=%s, counter=%d, is_deleted=%s \n %s", new_indv.uri, user, type, update_counter, text (deleted), jj);
        }
    }
}

private void print_stat()
{
    auto sorted_list = aa_sort(type_2_count);

    writefln("STAT: ------------------------");
    foreach (el; sorted_list)
    {
        auto prc = el[ 1 ] * oprc;
        writefln("%10d %3.2f%% %s", el[ 1 ], prc, el[ 0 ]);
    }
}

private Tuple!(string, long)[] aa_sort(long[ string ] aa)
{
    typeof(return )r = [];
    foreach (k, v; aa)
        r ~= tuple(k, v);
    sort!q{ a[ 1 ] < b[ 1 ] } (r);
    return r;
}
