/**
 * fanout sql module, normal priority
 */
module veda.fanout.fanout_sql;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import mysql.d;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context;
private import veda.util.tools;
private import veda.vmodule.vmodule, veda.fanout.to_sql;

void main(string[] args)
{
    string priority = "normal";

    process_name = "fanout-sql-np";

    Thread.sleep(dur!("seconds")(1));

    FanoutProcess p_fanout = new FanoutProcess(SUBSYSTEM.FANOUT_SQL, MODULE.fanout_sql_np, new Logger("veda-core-fanout-sql-np", "log", ""), priority);

    p_fanout.run();
}


