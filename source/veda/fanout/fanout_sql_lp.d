/**
 * fanout sql module, low priority
 */
module veda.fanout.fanout_sql_lp;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
//private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import mysql.d;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule, veda.fanout.to_sql;

void main(string[] args)
{
    string priority = "low";

    process_name = "fanout-sql-lp";

    Thread.sleep(dur!("seconds")(1));

    FanoutProcess p_fanout = new FanoutProcess(SUBSYSTEM.FANOUT_SQL, MODULE.fanout_sql_lp, new Logger("veda-core-fanout-sql-lp", "log", ""), priority);

    p_fanout.run();
}


