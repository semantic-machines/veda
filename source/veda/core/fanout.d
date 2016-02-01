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
private import smtp.client, smtp.mailsender, smtp.message;

//////
Mysql      mysql_conn;
string     database_name;
//////
MailSender smtp_conn;
//////

string     node_id;
Context    context;
Individual node;

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
public void send_put(Context ctx, string cur_state, string prev_state, long op_id)
{
    Tid tid_fanout = ctx.getTid(P_MODULE.fanout);

    if (tid_fanout != Tid.init)
    {
        send(tid_fanout, CMD.PUT, prev_state, cur_state);
    }
}

void fanout_thread(string thread_name, string _node_id)
{
    node_id = _node_id;
    scope (exit)
    {
        log.trace("ERR! fanout_thread dead (exit)");
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
                                {
                                    connect_to_mysql(context);
                                    connect_to_smtp(context);
                                }

                                if (mysql_conn !is null)
                                    push_to_mysql(prev_indv, new_indv);

                                if (smtp_conn !is null)
                                    push_to_smtp(prev_indv, new_indv);
                            }
                        }
                    },
                    (Variant v) { writeln(thread_name, "::fanout_thread::Received some other type.", v); });
        }
        catch (Throwable ex)
        {
            log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }
}

///////////////////////////////////////// SMTP FANOUT ///////////////////////////////////////////////////////////////////
private Resources extract_email(ref Ticket sticket, string ap_uri)
{
    if (ap_uri is null || ap_uri.length < 1)
        return null;

    Individual ap = context.get_individual(&sticket, ap_uri);

    if (ap.getStatus() != ResultCode.OK)
        return null;

    string p_uri = ap.getFirstLiteral("v-s:employee");
    if (p_uri is null)
        return null;

    Individual p = context.get_individual(&sticket, p_uri);
    if (p.getStatus() != ResultCode.OK)
        return null;

    string ac_uri = p.getFirstLiteral("v-s:hasAccount");
    if (ac_uri is null)
        return null;

    Individual ac = context.get_individual(&sticket, ac_uri);
    if (ac.getStatus() != ResultCode.OK)
        return null;

    return ac.resources[ "v-s:mailbox" ];
}


private void push_to_smtp(ref Individual prev_indv, ref Individual new_indv)
{
    try
    {
        Ticket sticket = context.sys_ticket();

        bool   is_deleted = new_indv.isExist("v-s:deleted", true);

        string isDraftOf            = new_indv.getFirstLiteral("v-s:isDraftOf");
        string actualVersion        = new_indv.getFirstLiteral("v-s:actualVersion");
        string previousVersion_prev = prev_indv.getFirstLiteral("v-s:previousVersion");
        string previousVersion_new  = new_indv.getFirstLiteral("v-s:previousVersion");

        if (isDraftOf !is null)
            return;

        if (is_deleted == false && (actualVersion !is null && actualVersion != new_indv.uri || 
           	(previousVersion_prev !is null && previousVersion_prev == previousVersion_new)))
                return;

        Resources types        = new_indv.getResources("rdf:type");
        bool      need_prepare = false;

        foreach (type; types)
        {
            if (context.get_onto().isSubClasses(type.uri, [ "v-s:Deliverable" ]))
            {
                need_prepare = true;
                break;
            }
        }


        if (need_prepare)
        {
            if (is_deleted == false)
            {
                string from         = new_indv.getFirstLiteral("v-wf:from");
                string to           = new_indv.getFirstLiteral("v-wf:to");
                string subject      = new_indv.getFirstLiteral("v-s:subject");
                string reply_to     = new_indv.getFirstLiteral("v-wf:replyTo");
                string message_body = new_indv.getFirstLiteral("v-s:messageBody");

                if (from !is null && to !is null)
                {
                    string email_from     = extract_email(sticket, from).getFirstString();
                    string email_to       = extract_email(sticket, to).getFirstString();
                    string email_reply_to = extract_email(sticket, reply_to).getFirstString();

                    if (from.length > 0 && to.length > 0)
                    {
                        auto message = SmtpMessage(
                                                   Recipient(email_from, "From"),
                                                   [ Recipient(email_to, "To") ],
                                                   subject,
                                                   message_body,
                                                   email_reply_to,
                                                   );

                        smtp.reply.SmtpReply res = smtp_conn.send(message);
                        
                        if (!res.success)
                        	log.trace ("FAIL send email: %s, result %s", message, text (res));
                    }
                }
            }
        }
    }
    catch (Exception ex)
    {
        log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
    }

    //writeln("@@fanout indv.uri=", indv.uri);
//            sender.quit();
}


///////////////////////////////////////////// MYSQL FANOUT ///////////////////////////////////////////////////////////////////

bool[ string ] isExistsTable;

private void push_to_mysql(ref Individual prev_indv, ref Individual new_indv)
{
    try
    {
        bool   is_deleted = new_indv.isExist("v-s:deleted", true);

        string isDraftOf            = new_indv.getFirstLiteral("v-s:isDraftOf");
        string actualVersion        = new_indv.getFirstLiteral("v-s:actualVersion");
        string previousVersion_prev = prev_indv.getFirstLiteral("v-s:previousVersion");
        string previousVersion_new  = new_indv.getFirstLiteral("v-s:previousVersion");

        if (isDraftOf !is null)
            return;

        if (is_deleted == false && (actualVersion !is null && actualVersion != new_indv.uri || 
           	(previousVersion_prev !is null && previousVersion_prev == previousVersion_new)))
                return;

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

            if (is_deleted == false)
            {
                foreach (predicate, rss; new_indv.resources)
                {
                    try
                    {
                        if (rss.length > 0)
                        {
                            create_table_if_not_exists(predicate, rss[ 0 ]);

                            foreach (rs; rss)
                            {
                                mysql_conn.query("SET NAMES 'utf8'");

                                if (rs.type == DataType.Boolean)
                                {
                                    if (rs.get!bool == true)
                                        mysql_conn.query("INSERT INTO `?` (doc_id, value) VALUES (?, ?)", predicate, new_indv.uri, 1);
                                    else
                                        mysql_conn.query("INSERT INTO `?` (doc_id, value) VALUES (?, ?)", predicate, new_indv.uri, 0);
                                }
                                else
                                {
                                    mysql_conn.query("INSERT INTO `?` (doc_id, value, lang) VALUES (?, ?, ?)", predicate, new_indv.uri,
                                                     rs.asString().toUTF8(), text(rs.lang));
                                }
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
                    }
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

private void connect_to_smtp(Context context)
{
    Ticket sticket = context.sys_ticket();

    node = context.getConfiguration();
    Resources gates = node.resources.get("v-s:send_an_email_individual_by_event", Resources.init);
    foreach (gate; gates)
    {
        Individual connection = context.get_individual(&sticket, gate.uri);

        Resource   transport = connection.getFirstResource("v-s:transport");
        if (transport != Resource.init)
        {
            if (transport.data() == "smtp")
            {
                try
                {
                    smtp_conn = new MailSender(connection.getFirstLiteral("v-s:host"), cast(ushort)connection.getFirstInteger("v-s:port"));

					if (smtp_conn is null)
						return;

                    string login = connection.getFirstLiteral("v-s:login");
                    string pass  = connection.getFirstLiteral("v-s:password");

                    if (login !is null && login.length > 0)
                        smtp_conn.authenticate(SmtpAuthType.PLAIN, login, pass);

                    // Connecting to smtp server
                    auto result = smtp_conn.connect();
                    if (!result.success)
                        smtp_conn = null;
                    else
                        log.trace("success connection to SMTP server: [%s]", connection);
                }
                catch (Throwable ex)
                {
                    log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s], connection=[%s]", ex.line, ex.file, ex.msg, connection);
                }
            }
        }
    }
}

private void connect_to_mysql(Context context)
{
    Ticket sticket = context.sys_ticket();

    node = context.getConfiguration();
    Resources gates = node.resources.get("v-s:push_individual_by_event", Resources.init);
    foreach (gate; gates)
    {
        Individual connection = context.get_individual(&sticket, gate.uri);

        Resource   transport = connection.getFirstResource("v-s:transport");
        if (transport != Resource.init)
        {
            if (transport.data() == "mysql")
            {
                try
                {
                    database_name = connection.getFirstLiteral("v-s:sql_database");
                    mysql_conn    = new Mysql(connection.getFirstLiteral("v-s:host"),
                                              cast(uint)connection.getFirstInteger("v-s:port"),
                                              connection.getFirstLiteral("v-s:login"),
                                              connection.getFirstLiteral("v-s:password"),
                                              database_name);

                    mysql_conn.query("SET NAMES 'utf8'");

                    //writeln("@@@@1 CONNECT TO MYSQL IS OK ", text(mysql_conn));
                }
                catch (Throwable ex)
                {
                    log.trace("fanout# EX! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
                }
            }
        }
    }
}

