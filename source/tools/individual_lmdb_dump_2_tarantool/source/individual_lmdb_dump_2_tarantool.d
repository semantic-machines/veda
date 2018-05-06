import std.stdio;
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
    KeyValueDB storage;

    string[ string ] properties;
    properties = readProperties("./veda.properties");
    string tarantool_url = properties.as!(string)("tarantool_url");

    if (tarantool_url !is null)
    {
        storage = new TarantoolDriver(log, "lmdb-individuals", 512);
    }

    string file_name = args[ 1 ];
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
