/**
 * fanout sql module, normal priority
 */
module veda.fanout.fanout_sql;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
//private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import mysql.d;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule, veda.fanout.to_sql;

void main(string[] args)
{
    string priority = "normal";
    process_name = "fanout-sql";

    Thread.sleep(dur!("seconds")(1));

    FanoutProcess p_fanout = new FanoutProcess(text(P_MODULE.fanout_sql_np), new Logger("veda-core-fanout-sql", "log", ""), priority);

    p_fanout.run();
}


