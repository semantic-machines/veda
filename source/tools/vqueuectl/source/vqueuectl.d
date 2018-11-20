import std.stdio, core.stdc.stdlib, std.uuid, std.algorithm, std.typecons, std.json, std.conv, std.string;
import veda.util.queue, veda.common.logger, veda.onto.individual, veda.onto.resource, veda.core.impl.app_context_creator_rlmdb;
import veda.core.common.context, veda.core.common.type;
import veda.storage.lmdb.lmdb_driver, veda.storage.lmdb.lmdb_header, veda.storage.common, veda.common.type, veda.onto.bj8individual.individual8json;
import filters.filter_00, filters.filter_01, filters.filter_02;

/*
    COMMAND QUEUE_NAME QUEUE_PATH [OPTIONS..]
 */

Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("vqueuectl", "log", "");
    return _log;
}

public double     oprc;
public LmdbDriver individual_lmdb_driver;
public Queue      queue_new;
string[ string ]  opt;

void main(string[] args)
{
    opt[ "--mstorage_url" ]  = "";
    opt[ "--lmdbsrv_url" ] = "";
    opt[ "--veda_user" ] = "";
    opt[ "--veda_pass" ] = "";
    opt[ "--v" ] = "";
	
    bool[ string ] cmds;
    cmds[ "check" ]                   = true;
    cmds[ "message_to_json" ]         = true;
    cmds[ "repair" ]                  = true;
    cmds[ "stat_by_type" ]            = true;
    cmds[ "push_count" ]              = true;
    cmds[ "consumer_unread_counter" ] = true;
    cmds[ "extract_uris" ]            = true;
    cmds[ "check_links" ]             = true;
    cmds[ "remove_from_veda" ]        = true;

    cmds[ "check_links_00" ] = true;
    cmds[ "check_links_01" ] = true;
    cmds[ "check_links_02" ] = true;

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
        return;
    }

    foreach (arg; args)
    {
        string[] aa = arg.split("=");
        writeln(aa);
        if (aa.length == 2)
        {
            if (opt.get(aa[ 0 ], null) !is null)
                opt[ aa[ 0 ] ] = aa[ 1 ];
        }
    }

    string name = args[ 2 ];
    string path = args[ 3 ];

    //writefln("cmd=%s, name=%s, path=%s", command, name, path);

    Queue queue = new Queue(path, name, Mode.R, log);
    queue.open();
    queue.get_info(0);

    Context ctx;
    Ticket  sticket;

    if (command == "remove_from_veda")
    {
        ctx     = create_new_ctx("vqueueuctl", log, opt.get ("--mstorage_url", null) ~ "\0", opt.get ("--lmdbsrv_url", null) ~ "\0");
        sticket = ctx.sys_ticket();
    }

    //writefln("QUEUE %s content %d elements ", name, queue.count_pushed);

    if (queue.count_pushed <= 0)
        return;

    if (command == "consumer_unread_counter")
    {
        if (args.length < 5)
        {
            writeln("use %s COMMAND NAME DIR CONSUMER_NAME", args[ 0 ]);
            writeln("		EXAMPLE: vqueuectl consumer_unread_counter individuals-flow data/queue fanout_email");
            return;
        }

        string   consumer_name = args[ 4 ];

        Consumer cs1 = new Consumer(queue, path, consumer_name, Mode.R, log);
        cs1.open();
        cs1.get_info();

        writeln(queue.count_pushed - cs1.count_popped);
        return;
    }

    string   consumer_path = "./tmp";

    Consumer cs = new Consumer(queue, consumer_path, "cs-" ~ name, Mode.RW, log);
    cs.open();

    oprc = 100.0 / queue.count_pushed;
    long count;

    if (command == "check_links")
    {
        try
        {
            individual_lmdb_driver = new LmdbDriver("./data/lmdb-individuals", DBMode.R, "vqueuectl", log);
        } catch (Throwable ex)
        {
            writefln("fail connect to LMDB, err=%s", ex.msg);
        }
    }

    if (command == "check_links_01" || command == "check_links_02")
    {
        try
        {
            individual_lmdb_driver = new LmdbDriver("./data/lmdb-individuals", DBMode.R, "vqueuectl", log);
        } catch (Throwable ex)
        {
            writefln("fail connect to LMDB, err=%s", ex.msg);
        }
    }

    if (command == "repair")
    {
        queue_new = new Queue("./tmp/" ~ name ~ "/repair", name, Mode.RW, log);
        queue_new.open();
        queue_new.get_info(0);
    }

    if (command == "extract_uris")
    {
        queue_new = new Queue("./tmp/uris", "uris", Mode.RW, log);
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
            log.trace("%s %3.2f%% %d", command, prc, count);
        }

        cs.commit_and_next(true);

        if (command == "repair")
            queue_new.push(data);
        else if (command == "extract_uris")
        {
            string uri_type = extract_uri_type(data);
            if (uri_type !is null)
                queue_new.push(uri_type);
        }
        else if (command == "message_to_json")
            message_to_json(data);
        else if (command == "stat_by_type")
            collect_stat_by_type(data);
        else if (command == "check_links")
            check_links(data);
        else if (command == "remove_from_veda")
            execute_cmd_on_veda(INDV_OP.REMOVE, ctx, &sticket, data);
        else if (command == "check_links_00")
            check_links_00(data, queue_new, individual_lmdb_driver, log);
        else if (command == "check_links_01")
            check_links_01(data, queue_new, individual_lmdb_driver, log);
        else if (command == "check_links_02")
            check_links_02(data, queue_new, individual_lmdb_driver, log);
    }

    if (command == "stat_by_type")
        print_stat_by_type();

    if (command == "repair")
        queue_new.close();

    cs.close();
    queue.close();
}

/*
   public enum SUBSYSTEM : ubyte
   {
    NONE              = 0,
    STORAGE           = 1,
    ACL               = 2,
    FULL_TEXT_INDEXER = 4,
    FANOUT_EMAIL      = 8,
    SCRIPTS           = 16,
    FANOUT_SQL        = 32,
    USER_MODULES_TOOL = 64
   }
 */

private void execute_cmd_on_veda(INDV_OP cmd, Context ctx, Ticket *ticket, string data)
{
    if (ctx is null)
    {
        log.trace("ERR! execute_cmd_on_veda: context not initalized");
        return;
    }

    Individual imm;
    Individual indv;
    string     new_bin;

    int        dsr;

    if (data !is null)
        dsr = imm.deserialize(data);
    if (dsr < 0 || imm.uri is null)
    {
        imm = ctx.get_individual(data);

        if (imm.getStatus() != ResultCode.Ok)
        {
            log.trace("ERR! execute_cmd_on_veda: code=%s, uri=%s", imm.getStatus(), data);
        }
    }
    else
    {
        new_bin = imm.getFirstLiteral("new_state");

        if (new_bin is null)
        {
            log.trace("execute_cmd_on_veda: binobj not found, uri=%s ", indv.uri);
        }

        if (new_bin !is null && indv.deserialize(new_bin) < 0)
        {
            log.trace("ERR! read in queue, new binobj is individual:[%s]", new_bin);
        }
        else
        {
            imm.setStatus(ResultCode.Ok);
        }
    }

    if (imm.getStatus() == ResultCode.Ok)
    {
        log.trace("execute_cmd_on_veda: cmd=%s, uri=%s", cmd, indv.uri);
        if (cmd == INDV_OP.REMOVE)
        {
            OpResult res = ctx.update(null, -1, ticket, cmd, &indv, null, 1, OptFreeze.NONE, OptAuthorize.NO);
        }
    }
}

long[ string ] type_2_count;
private string extract_uri_type(string data)
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
            string type = new_indv.getFirstLiteral("rdf:type");
            return new_indv.uri ~ ";" ~ type;
        }
    }
    return null;
}

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

private void collect_stat_by_type(string data)
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

            log.trace("uri=%s, user=%s, type=%s, counter=%d, is_deleted=%s \n %s", new_indv.uri, user, type, update_counter, text(deleted), jj);
        }
    }
}

private void print_stat_by_type()
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
