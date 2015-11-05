/**
 * fanout thread
 */

module veda.core.fanout;

private import std.concurrency, std.stdio, std.conv, std.utf;
private import veda.type, veda.core.context;
private import util.logger, util.cbor, veda.core.util.cbor8individual;
private import storage.lmdb_storage, veda.core.thread_context;
private import veda.core.define, veda.onto.resource, onto.lang, veda.onto.individual;
private import mysql.d;

Mysql      mysql_conn;
string     node_id;
Context    context;
Individual node;
string     database_name;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "FANOUT");
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
                    (CMD cmd, string prev_state, string new_state)
                    {
                        check_context();

                        Individual prev_indv, new_indv;
                        if (cbor2individual(&new_indv, new_state) < 0)
                        {
                            log.trace("!ERR:invalid individual:[%s]", new_state);
                        }
                        else
                        {
                            if (prev_state !is null && cbor2individual(&prev_indv, prev_state) < 0)
                            {
                                log.trace("!ERR:invalid individual:[%s]", prev_state);
                            }
                            else
                            {
                                if (node == Individual.init)
                                    connect_to_mysql(context);

                                if (mysql_conn !is null)
                                {
                                    push_to_mysql(prev_indv, new_indv);
                                }
                            }
                        }
                    },
                    (Variant v) { writeln(thread_name, "::fanout_thread::Received some other type.", v); });
        }
        catch (Exception ex)
        {
            log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}

///////////////////////////////////////////// MYSQL FANOUT ///////////////////////////////////////////////////////////////////

bool[ string ] isExistsTable;

private void push_to_mysql(ref Individual prev_indv, ref Individual new_indv)
{
    try
    {
        Resources types        = new_indv.getResources("rdf:type");
        bool      need_prepare = false;

        foreach (type; types)
        {
            if (context.get_onto().isSubClasses(type.uri, [ "v-s:Exportable" ]))
            {
                need_prepare = true;
                break;
            }
        }


        if (need_prepare)
        {
            foreach (predicate, rss; prev_indv.resources)
            {
                try
                {
                    mysql_conn.query("DELETE FROM `?` WHERE doc_id = ?", predicate, prev_indv.uri);
                }
                catch (Exception ex)
                {
                    log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
                }
            }
            foreach (predicate, rss; new_indv.resources)
            {
                try
                {
                    if (rss.length > 0)
                    {
                        create_table_if_not_exists(predicate, rss[ 0 ]);

                        foreach (rs; rss)
                        {
                            mysql_conn.query("INSERT INTO `?` (doc_id, value, lang) VALUES (?, ?, ?)", predicate, new_indv.uri,
                                             rs.asString().toUTF8(), text(rs.lang));
                        }
                    }
                }
                catch (Exception ex)
                {
                    log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
                }
            }
        }

        //writeln ("@@@@1 insert TO MYSQL IS OK ", text (mysql_conn));
    }
    catch (Exception ex)
    {
        log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
    }

    //writeln("@@fanout indv.uri=", indv.uri);
}

private void create_table_if_not_exists(string predicate, Resource rs)
{
    if (isExistsTable.get(predicate, false) == true)
        return;

    auto rows = mysql_conn.query("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = ? AND table_name = ?;", database_name,
                                 predicate);

    if (rows.front()[ 0 ] == "0")
    {
        // create new table
        try
        {
            string sql_type;
            string sql_value_index = ", INDEX c3(`value`)";

            if (rs.type == DataType.Boolean)
            {
                sql_type = "BOOL";
            }
            else if (rs.type == DataType.Integer)
            {
                sql_type = "INTEGER";
            }
            else if (rs.type == DataType.Decimal)
            {
                sql_type = "DECIMAL";
            }
            else if (rs.type == DataType.String)
            {
                sql_type        = "TEXT";
                sql_value_index = "";
            }
            else if (rs.type == DataType.Uri)
            {
                sql_type = "CHAR(128)";
            }
            else if (rs.type == DataType.Datetime)
            {
                sql_type = "INTEGER";
            }

            mysql_conn.query(
                             "CREATE TABLE `veda_db`.`" ~ predicate ~
                             "` ( `ID` BIGINT NOT NULL AUTO_INCREMENT, `doc_id` CHAR(128) NOT NULL,  `value` " ~ sql_type ~
                             " NULL,  `lang` CHAR(2) NULL, PRIMARY KEY (`ID`),  INDEX c1(`doc_id`), INDEX c2(`lang`) " ~
                             sql_value_index ~ ") ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;");
            isExistsTable[ predicate ] = true;
        }
        catch (Exception ex)
        {
            log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
            throw ex;
        }
    }
    else
    {
        isExistsTable[ predicate ] = true;
    }
}

private void connect_to_mysql(Context context)
{
    Ticket sticket = context.sys_ticket();

    node = context.getConfiguration();
    Resources gates = node.resources.get("vsrv:push_individual_by_event", Resources.init);
    foreach (gate; gates)
    {
        Individual connection = context.get_individual(&sticket, gate.uri);

        Resource   transport = connection.getFirstResource("vsrv:transport");
        if (transport != Resource.init)
        {
            if (transport.data() == "mysql")
            {
                try
                {
                    database_name = connection.getFirstLiteral("vsrv:sql_database");
                    mysql_conn    = new Mysql(connection.getFirstLiteral("vsrv:host"),
                                              cast(uint)connection.getFirstInteger("vsrv:port"),
                                              connection.getFirstLiteral("vsrv:login"),
                                              connection.getFirstLiteral("vsrv:credentional"),
                                              database_name);

                    mysql_conn.query("SET NAMES utf8;");

                    //writeln("@@@@1 CONNECT TO MYSQL IS OK ", text(mysql_conn));
                }
                catch (Exception ex)
                {
                    log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
                }
            }
        }
    }
}

