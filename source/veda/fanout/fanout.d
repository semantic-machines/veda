/**
 * fanout module
 */
module veda.fanout;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import mysql.d;
private import smtp.client, smtp.mailsender, smtp.message, smtp.attachment, smtp.reply;
private import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule;

void main(char[][] args)
{
    process_name = "fanout";

    core.thread.Thread.sleep(dur!("seconds")(1));

    FanoutProcess p_fanout = new FanoutProcess(P_MODULE.fanout, "127.0.0.1", 8091);

    p_fanout.run();
}

class FanoutProcess : VedaModule
{
    Mysql      mysql_conn;
    string     database_name;

    MailSender smtp_conn;

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        super(_module_name, _host, _port);
    }

    override bool prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id)
    {
        try
        {
            if (mysql_conn !is null)
                push_to_mysql(prev_indv, new_indv);
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }


        try
        {
            if (smtp_conn !is null)
                push_to_smtp(prev_indv, new_indv);
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }

        committed_op_id = op_id;
        return true;
    }

    override void thread_id()
    {
    }

    override Context create_context()
    {
        return null;
    }


    override bool configure()
    {
        log.trace("use configuration: %s", node);

        connect_to_mysql(context);
        connect_to_smtp(context);

        return true;
    }

///////////////////////////////////////// SMTP FANOUT ///////////////////////////////////////////////////////////////////
    private Resources get_email_from_appointment(ref Ticket sticket, ref Individual ap)
    {
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

    private Resources extract_email(ref Ticket sticket, string ap_uri)
    {
        if (ap_uri is null || ap_uri.length < 1)
            return null;

        Resources  res;

        Individual indv = context.get_individual(&sticket, ap_uri);

        if (indv.getStatus() != ResultCode.OK)
            return null;

        if (indv.isExists("rdf:type", Resource(DataType.Uri, "v-s:Appointment")))
        {
            res = get_email_from_appointment(sticket, indv);
        }
        else if (indv.isExists("rdf:type", Resource(DataType.Uri, "v-s:Position")))
        {
            Individual[] l_individuals = context.get_individuals_via_query(
                                                                           &sticket,
                                                                           "'rdf:type' == 'v-s:Appointment' && 'v-s:occupation' == '" ~ indv.uri ~
                                                                           "'", true, 10000, 10000);

            foreach (individual; l_individuals)
            {
                Resources tmp_res = get_email_from_appointment(sticket, individual);

                foreach (rr; tmp_res)
                    res ~= rr;
            }
        }

        return res;
    }

    string scptn = "src=\"cid:";

    private string uri_2_cid(string src)
    {
        return src.replace(":", "_").replace("-", "_");
    }

    private string extract_cids(string _src, out string[] attachment_ids)
    {
        char[] src = (cast(char[])_src).dup;

        long   b_pos = 0;
        long   e_pos = 0;

        while (b_pos < src.length && e_pos < src.length)
        {
            b_pos = src.indexOf(scptn, e_pos);

            if (b_pos <= 0)
                break;

            b_pos += scptn.length;

            e_pos = src.indexOf("\"", b_pos);
            if (e_pos <= 0)
                break;

            char[] attachment_id = src[ b_pos..e_pos ];
            attachment_ids ~= attachment_id.dup;

            foreach (idx, ch; attachment_id)
            {
                if (ch == ':' || ch == '-')
                    attachment_id[ idx ] = '_';
            }

            if (b_pos == e_pos)
                break;
        }

        return cast(string)src;
    }

    private void push_to_smtp(ref Individual prev_indv, ref Individual new_indv)
    {
        SmtpMessage message;

        try
        {
            Ticket sticket = context.sys_ticket();

            bool   is_deleted = new_indv.isExists("v-s:deleted", true);

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
                 log.trace("send email: prepare %s", new_indv.uri);
            	
            	
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
                            string[] attachment_ids;
                            message_body = extract_cids(message_body, attachment_ids);

                            message = SmtpMessage(
                                                  Recipient(email_from, "From"),
                                                  [ Recipient(email_to, "To") ],
                                                  subject,
                                                  message_body,
                                                  email_reply_to
                                                  );

                            foreach (attachment_id; attachment_ids)
                            {
                                try
                                {
                                    //writeln("@1 attachment_id=", attachment_ids);
                                    Individual file_info = context.get_individual(&sticket, attachment_id);
                                    if (file_info !is Individual.init)
                                    {
                                        string path     = file_info.getFirstResource("v-s:filePath").get!string;
                                        string file_uri = file_info.getFirstResource("v-s:fileUri").get!string;
                                        if (path !is null && file_uri !is null && file_uri.length > 0)
                                        {
                                            if (path.length > 0)
                                                path = path ~ "/";

                                            string full_path = attachments_db_path ~ "/" ~ path ~ file_uri;

                                            auto   bytes = cast(ubyte[]) read(full_path);

                                            auto   attachment = SmtpAttachment(file_uri, bytes, uri_2_cid(attachment_id));
                                            message.attach(attachment);
                                        }
                                    }
                                }
                                catch (Exception ex)
                                {
                                    log.trace("#EX! fail prepare attachment for e-mail [%s], LINE:[%s], FILE:[%s], MSG:[%s]", new_indv.uri, ex.line,
                                              ex.file,
                                              ex.msg);
                                }
                            }

                            SmtpReply res = smtp_conn.send(message);
                            log.trace("send email: %s, %s, %s, result %s", new_indv.uri, message.sender, message.recipients, res);
                            if (!res.success || res.code == 451)
                            {
                                // reconnect and retry
                                connect_to_smtp(context);
                                res = smtp_conn.send(message);
                                log.trace("send email (retry): %s, %s, %s, result %s", new_indv.uri, message.sender, message.recipients, res);
                            }
                            else
                            {
                                new_indv.addResource("v-s:isSuccess", Resource(true));
                            }

                            new_indv.addResource("v-s:infoOfExecuting", Resource(text(res)));
                            new_indv.addResource("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));
                            new_indv.addResource("rdfs:label", Resource("email, from:" ~ email_from ~ ", to:" ~ email_to));
                            //context.put_individual(&sticket, new_indv.uri, new_indv, false, "fanout");
                        }
                    }
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("#EX! fail send e-mail[%s]=%s", ex.msg, message.toString());
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
            isExistsTable = isExistsTable.init;
            //log.trace ("push_to_mysql: prev_indv=%s", prev_indv);
            //log.trace ("push_to_mysql: new_indv=%s", new_indv);

            bool   is_deleted = new_indv.isExists("v-s:deleted", true);

            string isDraftOf            = new_indv.getFirstLiteral("v-s:isDraftOf");
            string actualVersion        = new_indv.getFirstLiteral("v-s:actualVersion");
            string previousVersion_prev = prev_indv.getFirstLiteral("v-s:previousVersion");
            string previousVersion_new  = new_indv.getFirstLiteral("v-s:previousVersion");

            if (isDraftOf !is null)
                return;

            if (is_deleted == false && (actualVersion !is null && actualVersion != new_indv.uri ||
                                        (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)))
                return;

            Resource  created = new_indv.getFirstResource("v-s:created");

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
                    if (rss.length > 0)
                    {
                        if (create_table_if_not_exists(predicate, rss[ 0 ]) == true)
                        {
                            try
                            {
                                mysql_conn.query("DELETE FROM `?` WHERE doc_id = ?", predicate, prev_indv.uri);
                            }
                            catch (Throwable ex)
                            {
                                log.trace("ERR! push_to_mysql LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
                            }
                        }
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
                                            mysql_conn.query("INSERT INTO `?` (doc_id, created, value) VALUES (?, ?, ?)", predicate, new_indv.uri,
                                                             created.asString(), 1);
                                        else
                                            mysql_conn.query("INSERT INTO `?` (doc_id, created, value) VALUES (?, ?, ?)", predicate, new_indv.uri,
                                                             created.asString(), 0);
                                    }
                                    else
                                    {
                                        mysql_conn.query("INSERT INTO `?` (doc_id, created, value, lang) VALUES (?, ?, ?, ?)", predicate,
                                                         new_indv.uri,
                                                         created.asString(),
                                                         rs.asString().toUTF8(), text(rs.lang));
                                    }

                                    log.trace("push_to_mysql: INSERT INTO `%s` (doc_id, created, value, lang) VALUES (%s, %s, %s, %s)", predicate,
                                              new_indv.uri,
                                              created.asString(), rs.asString().toUTF8(), text(rs.lang));
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            log.trace("ERR! push_to_mysql LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
                        }
                    }
                }
            }

            //writeln ("@@@@1 insert TO MYSQL IS OK ", text (mysql_conn));
        }
        catch (Throwable ex)
        {
            log.trace("ERR! push_to_mysql LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }

        //writeln("@@fanout indv.uri=", indv.uri);
    }

    private bool create_table_if_not_exists(string predicate, Resource rs)
    {
        if (isExistsTable.get(predicate, false) == true)
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
                    sql_type = "DECIMAL (10,2)";
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
                                 "CREATE TABLE `veda_db`.`" ~ predicate ~ "` ("
                                 "`ID` BIGINT NOT NULL AUTO_INCREMENT, "
                                 "`doc_id` CHAR(128) NOT NULL, "
                                 "`created` DATETIME NULL, "
                                 "`value` " ~ sql_type ~ " NULL, "
                                 "`lang` CHAR(2) NULL, "
                                 " PRIMARY KEY (`ID`), "
                                 " INDEX c1(`doc_id`), INDEX c3 (`created`), INDEX c2(`lang`) " ~ sql_value_index ~
                                 ") ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;");
                isExistsTable[ predicate ] = true;
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
            isExistsTable[ predicate ] = true;
            return true;
        }
    }

    private void connect_to_smtp(Context context)
    {
        try
        {
            Ticket    sticket = context.sys_ticket();

            Resources gates = node.resources.get("v-s:send_an_email_individual_by_event", Resources.init);
            log.trace("connect_to_smtp:found gates: %s", gates);
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
                            log.trace("found connect to smtp [%s]", connection);
                            auto host = connection.getFirstLiteral("v-s:host");
                            auto port = cast(ushort)connection.getFirstInteger("v-s:port");
                            smtp_conn = new MailSender(host, port);
                            if (smtp_conn is null)
                            {
                                log.trace("fail create connection [%s] %s:%d", connection.uri, host, port);
                                continue;
                            }

                            auto addr = new InternetHost;
                            if (addr.getHostByName(host))
                            {
                                // Connecting to smtp server
                                SmtpReply result = smtp_conn.connect();
                                if (!result.success)
                                {
                                    log.trace("fail connection to smtp server [%s] %s:%d", connection.uri, host, port);
                                    smtp_conn = null;
                                    continue;
                                }
                                else
                                    log.trace("success connection to SMTP server: [%s]", connection);


                                string login = connection.getFirstLiteral("v-s:login");
                                string pass  = connection.getFirstLiteral("v-s:password");

                                if (login !is null && login.length > 0)
                                    smtp_conn.authenticate(SmtpAuthType.PLAIN, login, pass);

                                if (!result.success)
                                {
                                    log.trace("fail authenticate to smtp [%s] %s:%d", connection.uri, host, port);
                                    smtp_conn = null;
                                    continue;
                                }
                            }
                            else
                              log.trace("smtp server unavailable [%s] %s:%d", connection.uri, host, port);
                        }
                        catch (Throwable ex)
                        {
                            smtp_conn = null;
                            log.trace("EX! fanout.connect_to_smtp# LINE:[%s], FILE:[%s], MSG:[%s], connection=[%s]", ex.line, ex.file, ex.msg,
                                      connection);
                        }
                    }
                }
                else
                {
                    log.trace("WARN:connect_to_smtp:connection [%s] no content [v-s:transport]", connection);
                }
            }
        }
        catch (Throwable ex)
        {
            printPrettyTrace(stdout);
            log.trace("ERR! fanout.connect_to_smtp# LINE:[%s], FILE:[%s], MSG:[%s], %s", ex.line, ex.file, ex.msg, ex.toString);
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

                            log.trace("CONNECT TO MYSQL IS OK, %s", text(mysql_conn));
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


