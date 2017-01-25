/**
 * fanout module
 */
module veda.fanout_email;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import smtp.client, smtp.mailsender, smtp.message, smtp.attachment, smtp.reply;
private import veda.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
private import veda.vmodule.vmodule;

void main(char[][] args)
{
    process_name = "fanout-email";

    Thread.sleep(dur!("seconds")(1));

    FanoutProcess p_fanout = new FanoutProcess(text(P_MODULE.fanout_email), new Logger("veda-core-fanout-email", "log", ""));

    p_fanout.run();
}

class FanoutProcess : VedaModule
{
    string     database_name;

    MailSender smtp_conn;

    this(string _module_name, Logger log)
    {
        super(_module_name, log);
    }

    override ResultCode prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                                string event_id,
                                long op_id)
    {
        //log.trace("[%s]: start prepare", new_indv.uri);

        //scope (exit)
        //{
        //    log.trace("[%s]: end prepare", new_indv.uri);
        //}

        ResultCode res;

        try
        {
            if (smtp_conn !is null)
            {
                res = push_to_smtp(prev_indv, new_indv);
                if (res == ResultCode.Connect_Error)
                {
                    connect_to_smtp(context);
                    return ResultCode.Connect_Error;
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }

        if (res == ResultCode.OK)
        {
            committed_op_id = op_id;
            return ResultCode.OK;
        }
        else
            return ResultCode.Fail_Commit;
    }

    override void thread_id()
    {
    }

    override void receive_msg(string msg)
    {
    }

    override Context create_context()
    {
        return null;
    }


    override bool open()
    {
        connect_to_smtp(context);
        return true;
    }

    override bool configure()
    {
        log.trace("use configuration: %s", node);
        return true;
    }

    override bool close()
    {
        delete smtp_conn;
        return true;
    }

    override void event_of_change(string uri)
    {
        configure();
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

        return ac.getResources("v-s:mailbox");
    }

    private Resources extract_email(ref Ticket sticket, string ap_uri, out string label)
    {
        if (ap_uri is null || ap_uri.length < 1)
            return null;

        Resources  res;

        Individual indv = context.get_individual(&sticket, ap_uri);

        if (indv.getStatus() != ResultCode.OK)
            return null;

        label = indv.getFirstLiteral("rdfs:label");

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

    private ResultCode push_to_smtp(ref Individual prev_indv, ref Individual new_indv)
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
                return ResultCode.OK;

            if (is_deleted == false && (actualVersion !is null && actualVersion != new_indv.uri ||
                                        (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)))
                return ResultCode.OK;

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


            if (!need_prepare)
                return ResultCode.OK;

            ResultCode rc;
            bool       is_send = false;

            if (is_deleted == false)
            {
                delete smtp_conn;
                connect_to_smtp(context);

                string from         = new_indv.getFirstLiteral("v-wf:from");
                string to           = new_indv.getFirstLiteral("v-wf:to");
                string subject      = new_indv.getFirstLiteral("v-s:subject");
                string reply_to     = new_indv.getFirstLiteral("v-wf:replyTo");
                string message_body = new_indv.getFirstLiteral("v-s:messageBody");

                if (from !is null && to !is null)
                {
                    string from_label;
                    string email_from = extract_email(sticket, from, from_label).getFirstString();

                    if (from_label is null || from_label.length == 0)
                        from_label = "Veda System";

                    string      label;
                    Recipient[] rr_email_to;
                    foreach (Resource el; extract_email(sticket, to, label))
                        rr_email_to ~= Recipient(el.data(), label);

                    string str_email_reply_to = "";
                    foreach (Resource el; extract_email(sticket, reply_to, label))
                        str_email_reply_to ~= el.data() ~ ";";

                    if (from.length > 0 && to.length > 0)
                    {
                        string[] attachment_ids;
                        message_body = extract_cids(message_body, attachment_ids);

                        message = SmtpMessage(
                                              Recipient(email_from, from_label),
                                              rr_email_to,
                                              subject,
                                              message_body,
                                              str_email_reply_to
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
                        log.trace("push_to_smtp: %s, %s, %s, result.msg=%s result.code=%d", new_indv.uri, message.sender, message.recipients,
                                  res.message,
                                  res.code);

                        if (!res.success)
                        {
                            is_send = false;
                            if (res.code == 451)
                                rc = ResultCode.Connect_Error;
                            if (res.code == 501)
                                rc = ResultCode.Bad_Request;
                        }
                        else
                        {
                            rc      = ResultCode.OK;
                            is_send = true;
                            //new_indv.addResource("v-s:isSuccess", Resource(true));
                        }

                        //new_indv.addResource("v-s:infoOfExecuting", Resource(text(res)));
                        //new_indv.addResource("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));
                        //new_indv.addResource("rdfs:label", Resource("email, from:" ~ email_from ~ ", to:" ~ email_to));
                        //context.put_individual(&sticket, new_indv.uri, new_indv, false, "fanout");
                    }
                    else
                    {
                        log.trace("WARN: push_to_smtp[%s]: fail extract_email from field from[%s] or to[%s]", new_indv.uri, from, to);
                    }
                }
                else
                {
                    log.trace("WARN: push_to_smtp[%s]: invalid field from[%s] or to[%s]", new_indv.uri, from, to);
                }
            }

            if (is_send == false)
                log.trace("WARN: push_to_smtp[%s]: not send", new_indv.uri);

            return rc;
        }
        catch (Throwable ex)
        {
            log.trace("ERR! push_to_smtp[%s], \n%s", new_indv.uri, ex.info);
            return ResultCode.Internal_Server_Error;
        }
    }

    private void connect_to_smtp(Context context)
    {
        try
        {
            Ticket    sticket = context.sys_ticket();

            Resources gates = node.resources.get("v-s:send_an_email_individual_by_event", Resources.init);
            //log.trace("connect_to_smtp:found gates: %s", gates);
            foreach (gate; gates)
            {
                Individual connection = context.get_individual(&sticket, gate.uri);
                subscribe_on_prefetch(gate.uri);
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
                            log.trace("ERR! fanout.connect_to_smtp# LINE:[%s], FILE:[%s], MSG:[%s], connection=[%s]", ex.line, ex.file, ex.msg,
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
}


