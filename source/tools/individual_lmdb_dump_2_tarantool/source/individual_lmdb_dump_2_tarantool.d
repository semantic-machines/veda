import std.stdio, core.stdc.stdlib;
import veda.storage.lmdb.lmdb_driver;
import veda.storage.tarantool.tarantool_driver, veda.storage.common, veda.common.type, veda.onto.individual;
import veda.util.properd;
import veda.common.logger;

Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("individual-lmdb-dump-2-tarantool", "log", "");
    return _log;
}

void main(string[] args)
{
    if (args.length < 3)
    {
        writefln("need command line argument: [dest db name] [path to lmdb dump file]");
        exit(-1);
        return;
    }

    string file_name    = args[ 2 ];
    string dest_db_name = args[ 1 ];


    KeyValueDB storage;

    string[ string ] properties;
    properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    if (tarantool_url !is null)
    {
        if (dest_db_name == "individuals")
            storage = new TarantoolDriver(log, dest_db_name, 512);
        else if (dest_db_name == "tickets")
            storage = new TarantoolDriver(log, dest_db_name, 513);
    }

    writefln("read file [%s]", file_name);
    auto   file = File(file_name, "r");

    bool   is_prepare = false;
    string line;

    long   counter = 0;

    while ((line = file.readln()) !is null)
    {
        if (line == " summ_hash_this_db\n")
            is_prepare = false;

        if (is_prepare)
        {
            counter++;
            string key   = line;
            string value = file.readln();

            key   = key[ 1..$ - 1 ];
            value = value[ 1..$ - 1 ];

            Individual indv;
            if (indv.deserialize(value) < 0)
            {
                writefln("ERR! %d KEY=[%s]", counter, key);
            }
            else
            {
                string new_bin = indv.serialize();
                storage.put(OptAuthorize.NO, null, key, new_bin, -1);
                writefln("OK, %d KEY=[%s]", counter, key);
            }

            //writefln("VALUE=[%s]", value);
        }
        if (line == "HEADER=END\n")
            is_prepare = true;
    }
}
