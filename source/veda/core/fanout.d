/**
 * fanout thread
 */

module veda.core.fanout;

private import std.concurrency, std.stdio, std.conv;
private import type, veda.core.context;
private import util.logger, util.cbor, veda.core.util.cbor8individual;
private import storage.lmdb_storage, veda.core.thread_context;
private import veda.core.define, veda.onto.resource, onto.lang, veda.onto.individual;
private import mysql.d;

Mysql      mysql_push_individual_by_event;
string     node_id;
Context    context;
Individual *node;

// ////// logger ///////////////////////////////////////////
private import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("pacahon", "log", "fanout");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

void fanout_thread(string thread_name, string _node_id)
{
    node_id = _node_id;
    scope (exit)
    {
        log.trace("ERR! indexer thread dead (exit)");
    }

    core.thread.Thread.getThis().name = thread_name;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    void check_context()
    {
        if (context is null)
            context = new PThreadContext(node_id, thread_name, P_MODULE.fanout);
    }

    while (true)
    {
        try
        {
            receive(
                    (CMD cmd, string msg)
                    {
                        check_context();

                        Individual indv;
                        if (cbor2individual(&indv, msg) < 0)
                        {
                            log.trace("!ERR:invalid individual:[%s]", msg);
                        }
                        else
                        {
                            if (node is null)
                                connect_to_mysql(context);

                            if (mysql_push_individual_by_event !is null)
                            {
                                try
                                {
                                    foreach (predicate, rss; indv.resources)
                                    {
                                        foreach (rs; rss)
                                        {
                                            //mysql_push_individual_by_event.query("insert into qlik_atts2 (uri, individual) values (?, ?);", "-", rr.toString());
                                        }
                                    }

                                    //            Json rr = Json.emptyObject;
                                    //             cbor2json(&rr, msg);



                                    //writeln ("@@@@7 rr", rr.toString());
                                    //writeln ("@@@@1 insert TO MYSQL IS OK ", text (mysql_push_individual_by_event));
                                }
                                catch (Exception ex)
                                {
                                    writeln("@@@@FAIL insert TO MYSQL ", ex.msg);
                                }


                                //writeln("@@fanout indv.uri=", indv.uri);
                            }
                        }
                    },
                    (Variant v) { writeln(thread_name, "::fanout_thread::Received some other type.", v); });
        }
        catch (Exception ex)
        {
            log.trace("^^^^fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}


private void connect_to_mysql(Context context)
{
    node = context.getConfiguration();
    Resources gates = node.resources.get("vsrv:push_individual_by_event", Resources.init);
    foreach (gate; gates)
    {
        Individual connection = context.get_individual(null, gate.uri);

        Resource   transport = connection.getFirstResource("vsrv:transport");
        if (transport != Resource.init)
        {
            if (transport.data() == "mysql")
            {
                //writeln ("@@@@6");
                try
                {
                    mysql_push_individual_by_event = new Mysql(connection.getFirstLiteral("vsrv:host"),
                                                               cast(uint)connection.getFirstInteger("vsrv:port"),
                                                               connection.getFirstLiteral("vsrv:login"),
                                                               connection.getFirstLiteral("vsrv:credentional"),
                                                               connection.getFirstLiteral("vsrv:sql_database"));

                    writeln("@@@@1 CONNECT TO MYSQL IS OK ", text(mysql_push_individual_by_event));
                }
                catch (Exception ex)
                {
                    writeln("@@@@FAIL CONNECT TO MYSQL ", ex.msg);
                }
            }
        }
    }
}

