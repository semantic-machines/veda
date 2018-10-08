/**
 * veda to sql, basic
 */
module veda.fanout.to_sql;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import mysql.d;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context, veda.search.ft_query.ft_query_client;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule;

public class FanoutProcess : VedaModule
{
    Mysql  mysql_conn;
    string database_name;
    string low_priority_user;
    string priority;

    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log, string _priority)
    {
        priority = _priority;
        super(_subsystem_id, _module_id, log);
    }

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id, long transaction_id, long op_id, long count_pushed, long count_popped)
    {
        ResultCode rc;

        if (priority == "low" && user_uri == low_priority_user || priority == "normal" && user_uri != low_priority_user)
        {
            try
            {
                rc = push_to_mysql(prev_indv, new_indv);
            }
            catch (Throwable ex)
            {
                log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
            }
        }

        committed_op_id = op_id;
        return ResultCode.OK;
    }

    override void thread_id()
    {
    }

    override void receive_msg(string msg)
    {
        //log.trace("receive_msg [%s]", msg);
        if (msg == "unload_batch")
        {
            prepare_batch();
        }
    }

    override Context create_context()
    {
        return null;
    }

    override bool open()
    {
        //context.set_vql (new XapianSearch(context));
        context.set_vql(new FTQueryClient(context));

        connect_to_mysql(context);
        return true;
    }

    override bool configure()
    {
        log.trace("use configuration: %s", node);

        return true;
    }

    override bool close()
    {
        if (mysql_conn !is null)
            mysql_conn.close();
        return true;
    }

    override void event_of_change(string uri)
    {
        configure();
    }

    bool[ string ] existsTable;

    private ResultCode push_to_mysql(ref Individual prev_indv, ref Individual new_indv)
    {
        if (mysql_conn is null)
            return ResultCode.Connect_Error;

        try
        {
            existsTable = existsTable.init;
            //log.trace("push_to_mysql: prev_indv=%s", prev_indv);
            //log.trace("push_to_mysql: new_indv=%s", new_indv);
//            bool   is_deleted = new_indv.isExists("v-s:deleted", true);

            string isDraftOf            = new_indv.getFirstLiteral("v-s:isDraftOf");
            string actualVersion        = new_indv.getFirstLiteral("v-s:actualVersion");
            string previousVersion_prev = prev_indv.getFirstLiteral("v-s:previousVersion");
            string previousVersion_new  = new_indv.getFirstLiteral("v-s:previousVersion");

            if (isDraftOf !is null)
            {
                log.trace("new_indv [%s] is draft, ignore", new_indv.uri);
                return ResultCode.OK;
            }

            if ((actualVersion !is null && actualVersion != new_indv.uri /*||
                                                                                                   (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)*/))
            {
                if (actualVersion !is null && actualVersion != new_indv.uri)
                    log.trace("new[%s].v-s:actualVersion[%s] != [%s], ignore", new_indv.uri, actualVersion, new_indv.uri);

//		if (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)
//          log.trace("prev[%s].v-s:previousVersion[%s] == new[%s].v-s:previousVersion[%s], ignore", prev_indv.uri, previousVersion_prev, new_indv.uri, previousVersion_new);

                return ResultCode.OK;
            }

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
                mysql_conn.startTransaction();

                // создаем таблицы если их не было
                foreach (predicate, rss; new_indv.resources)
                {
                    if (rss.length > 0)
                        create_table_if_not_exists(predicate, rss[ 0 ]);
                }

                // удаляем из всех таблиц по предикатам используя doc_id
                Resources[ string ] use_predicates = new_indv.resources.dup;
                foreach (predicate, rss; prev_indv.resources)
                {
                    use_predicates[ predicate ] = rss;
                }

                foreach (predicate, rss; use_predicates)
                {
                    try
                    {
                        mysql_conn.query("DELETE FROM `?` WHERE doc_id = ?", predicate, new_indv.uri);
                        log.trace("push_to_mysql: DELETE FROM `%s` WHERE doc_id = %s", predicate, new_indv.uri);
                    }
                    catch (Throwable ex)
                    {
                        log.trace("ERR! push_to_mysql LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
                    }
                }

                //if (is_deleted == false)
                {
                    foreach (predicate, rss; new_indv.resources)
                    {
                        try
                        {
                            insert_to_sql(predicate, rss, new_indv);
                        }
                        catch (Exception ex)
                        {
                            log.trace("ERR! push_to_mysql LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
                            if (ex.msg.indexOf("Unknown column 'deleted'") >= 0)
                            {
                                mysql_conn.query(
                                                 "ALTER TABLE `veda_db`.`" ~ predicate ~ "` " ~
                                                 "ADD `deleted` BOOL NULL");
                                log.trace("alter table [%s]", predicate);
                                insert_to_sql(predicate, rss, new_indv);
                            }
                        }
                    }
                }
            }
            mysql_conn.query("COMMIT");

            //writeln ("@@@@1 insert TO MYSQL IS OK ", text (mysql_conn));
        }
        catch (Throwable ex)
        {
            log.trace("ERR! push_to_mysql LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }

        return ResultCode.OK;
    }

    private void insert_to_sql(string predicate, Resources rss, ref Individual new_indv)
    {
        Resource  created    = new_indv.getFirstResource("v-s:created");
        Resources types      = new_indv.getResources("rdf:type");
        bool      is_deleted = new_indv.isExists("v-s:deleted", true);

        int       i_is_deleted = 0;

        if (is_deleted)
            i_is_deleted = 1;

        if (rss.length > 0)
        {
            create_table_if_not_exists(predicate, rss[ 0 ]);

            foreach (rs; rss)
            {
                foreach (rtype; types)
                {
                    MysqlResult res;

                    string      type = rtype.data;
                    mysql_conn.query("SET NAMES 'utf8'");

                    if (rs.type == DataType.Boolean)
                    {
                        int bval = 0;

                        if (rs.get!bool == true)
                            bval = 1;

                        res = mysql_conn.query("INSERT INTO `?` (doc_id, doc_type, created, value, deleted) VALUES (?, ?, ?, ?, ?)",
                                               predicate,
                                               new_indv.uri,
                                               type,
                                               created.asString(), bval, i_is_deleted);
                        log.trace(
                                  "push_to_mysql: INSERT INTO `%s` (doc_id, doc_type, created, value, deleted) VALUES (%s, %s, %s, %s %s), res=%s",
                                  predicate,
                                  new_indv.uri, type,
                                  created.asString(), bval, i_is_deleted, mysql_conn.error());
                    }
                    else
                    {
                        res = mysql_conn.query("INSERT INTO `?` (doc_id, doc_type, created, value, lang, deleted) VALUES (?, ?, ?, ?, ?, ?)",
                                               predicate,
                                               new_indv.uri, type,
                                               created.asString(), rs.asString().toUTF8(), text(rs.lang), i_is_deleted);

                        log.trace(
                                  "push_to_mysql: INSERT INTO `%s` (doc_id, doc_type, created, value, lang, deleted) VALUES (%s, %s, %s, %s, %s %s), res=%s",
                                  predicate,
                                  new_indv.uri, type,
                                  created.asString(), rs.asString().toUTF8(), text(rs.lang), i_is_deleted, mysql_conn.error());
                    }
                }
            }
        }
    }

    private bool create_table_if_not_exists(string predicate, Resource rs)
    {
        if (mysql_conn is null)
            return false;

        if (existsTable.get(predicate, false) == true)
            return true;

        auto rows = mysql_conn.query("SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = ? AND table_name = ?;", database_name,
                                     predicate);

        if (rows.front()[ 0 ] == "0")
        {
            // create new table
            try
            {
                string sql_type;
                string sql_value_index = ", INDEX civ(`value`)";

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
                    sql_type = "DECIMAL (14,4)";
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
                    sql_type = "DATETIME";
                }

                mysql_conn.query(
                                 "CREATE TABLE `veda_db`.`" ~ predicate ~ "` (" ~
                                 "`ID` BIGINT NOT NULL AUTO_INCREMENT, " ~
                                 "`doc_id` CHAR(128) NOT NULL, " ~
                                 "`doc_type` CHAR(128) NOT NULL, " ~
                                 "`created` DATETIME NULL, " ~
                                 "`value` " ~ sql_type ~ " NULL, " ~
                                 "`lang` CHAR(2) NULL, " ~
                                 "`deleted` BOOL NULL, " ~
                                 " PRIMARY KEY (`ID`), " ~
                                 " INDEX c1(`doc_id`), INDEX c2(`doc_type`), INDEX c3 (`created`), INDEX c4(`lang`) " ~ sql_value_index ~
                                 ") ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;");
                existsTable[ predicate ] = true;
                log.trace("create table [%s]", predicate);
            }
            catch (Exception ex)
            {
                log.trace("ERR! create_table_if_not_exists LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
                throw ex;
            }
            return false;
        }
        else
        {
            existsTable[ predicate ] = true;
            return true;
        }
    }

    private void connect_to_mysql(Context context)
    {
        try
        {
            Ticket    sticket = context.sys_ticket();

            Resources gates = node.resources.get("v-s:push_individual_by_event", Resources.init);
            log.trace("connect_to_mysql:found gates: %s", gates);
            foreach (gate; gates)
            {
                Individual connection = context.get_individual(&sticket, gate.uri, OptAuthorize.NO);
                log.trace("connect_to_mysql:connection: %s=[%s]", gate.uri, connection);
                //subscribe_on_prefetch(gate.uri);

                Resource transport = connection.getFirstResource("v-s:transport");
                if (transport != Resource.init)
                {
                    if (transport.data() == "mysql")
                    {
                        try
                        {
                            low_priority_user = connection.getFirstLiteral("cfg:low_priority_user");
                            database_name     = connection.getFirstLiteral("v-s:sql_database");
                            mysql_conn        = new Mysql(connection.getFirstLiteral("v-s:host"),
                                                          cast(uint)connection.getFirstInteger("v-s:port"),
                                                          connection.getFirstLiteral("v-s:login"),
                                                          connection.getFirstLiteral("v-s:password"),
                                                          database_name);

                            mysql_conn.query("SET NAMES 'utf8'");

                            log.trace("CONNECT TO MYSQL IS OK, %s", text(mysql_conn));
                            log.trace("low_priority_user = %s", low_priority_user);
                        }
                        catch (Throwable ex)
                        {
                            log.trace("EX! fanout.connect_to_mysql# EX:[%s], connection=[%s]", ex.toString(), connection);
                        }
                    }
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("EX! fanout.connect_to_mysql# EX:[%s]", ex.toString());
        }
    }
}


