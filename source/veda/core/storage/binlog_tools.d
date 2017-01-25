/**
 * binlog tools
 */
module veda.core.storage.binlog_tools;

import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
import veda.common.logger, veda.core.util.utils;
import veda.common.type, veda.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.common.log_msg, veda.onto.individual,
       veda.onto.resource;
import veda.core.storage.lmdb_storage;

bool check_binlog(string file_name)
{
    return false;
}

public string write_in_binlog(string msg, string new_hash, string bin_log_name, out int size_bin_log, int max_size_bin_log, string db_path)
{
    long      now = Clock.currTime().stdTime();
    OutBuffer oub = new OutBuffer();

    oub.write('\n');
    oub.write(cast(ubyte)0xff);
    oub.write(cast(ubyte)0x12);
    oub.write(cast(ubyte)0xff);
    oub.write(cast(ubyte)0x21);
    oub.write(cast(ubyte)0);

    oub.write(now);
    oub.write(cast(int)new_hash.length);
    oub.write(cast(int)msg.length);
    oub.write(new_hash);
    oub.write(msg);
    append(bin_log_name, oub.toString);
    size_bin_log += msg.length + 30;

    if (size_bin_log > max_size_bin_log)
    {
        size_bin_log = 0;
        bin_log_name = get_new_binlog_name(db_path);
    }
    return bin_log_name;
}
