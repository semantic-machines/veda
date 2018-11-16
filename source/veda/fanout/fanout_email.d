/**
 * fanout email module
 */
module veda.fanout.fanout_email;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.array, std.socket, core.thread;
private import smtp.client, smtp.mailsender, smtp.message, smtp.attachment, smtp.reply;
private import veda.common.type, veda.core.common.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import veda.common.logger, veda.core.impl.thread_context, veda.search.ft_query.ft_query_client;
private import veda.core.common.context;
private import veda.vmodule.vmodule;

void main(char[][] args)
{
    process_name = "fanout-email";

    Thread.sleep(dur!("seconds")(1));

    FanoutProcess p_fanout = new FanoutProcess(SUBSYSTEM.FANOUT_EMAIL, MODULE.fanout_email, new Logger("veda-core-fanout-email", "log", ""));

    p_fanout.run();
}

class FanoutProcess : VedaModule
{
    MailSender smtp_conn;
    string     default_mail_sender;
    bool       always_use_mail_sender;
    string     host;
    ushort     port;

    this(SUBSYSTEM _subsystem_id, MODULE _module_id, Logger log)
    {
        super(_subsystem_id, _module_id, log);
    }

    override ResultCode prepare(string queue_name, string src, INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin,
                                ref Individual new_indv,
                                string event_id, long transaction_id, long op_id, long count_pushed,
                                long count_popped)
    {
        //log.trace("[%s]: start prepare", new_indv.uri);

        //scope (exit)
        //{
        //    log.trace("[%s]: end prepare", new_indv.uri);
        //}

        if (host is null)
        {
            // addr to smpt server not set, skip message

            committed_op_id = op_id;
            return ResultCode.Ok;
        }

        ResultCode res;

        try
        {
            if (host !is null && smtp_conn is null)
            {
                log.trace("ERR! connect to smtp server %s:%d not exist, reconnect", host, port);
                connect_to_smtp(context);
            }

            if (smtp_conn !is null)
            {
                res = push_to_smtp(prev_indv, new_indv);
                if (res == ResultCode.ConnectError)
                {
                    connect_to_smtp(context);
                    return ResultCode.ConnectError;
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }

        if (res == ResultCode.Ok)
        {
            committed_op_id = op_id;
            return ResultCode.Ok;
        }
        else
        {
            return ResultCode.FailCommit;
        }
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
        //context.set_vql (new XapianSearch(context));
        context.set_vql(new FTQueryClient(context));

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
        smtp_conn.destroy();
        return true;
    }

    override void event_of_change(string uri)
    {
        configure();
    }


///////////////////////////////////////// SMTP FANOUT ///////////////////////////////////////////////////////////////////
    private Resources get_email_from_appointment(ref Ticket sticket, string hasMessageType, ref Individual ap)
    {
        string p_uri = ap.getFirstLiteral("v-s:employee");

        if (p_uri is null)
            return null;

        Individual prs = context.get_individual(p_uri);
        if (prs.getStatus() != ResultCode.Ok || prs.isExists("v-s:deleted", true) == true)
            return null;

        string preference_uri = prs.getFirstLiteral("v-ui:hasPreferences");
        if (preference_uri !is null)
        {
            Individual preference = context.get_individual(preference_uri);
            if (preference.getStatus() == ResultCode.Ok)
            {
                string receiveMessageType = prs.getFirstLiteral("v-ui:receiveMessageType");

                if (receiveMessageType !is null)
                {
                    if ((hasMessageType !is null && receiveMessageType == hasMessageType) == false)
                        return null;

                    if ((hasMessageType is null && receiveMessageType == "v-s:OtherNotification") == false)
                        return null;
                }
            }
        }

        string ac_uri = prs.getFirstLiteral("v-s:hasAccount");
        if (ac_uri is null)
            return null;

        Individual ac = context.get_individual(ac_uri);
        if (ac.getStatus() != ResultCode.Ok || ac.isExists("v-s:deleted", true) == true)
            return null;

        return ac.getResources("v-s:mailbox");
    }

    private Resources extract_email(ref Ticket sticket, string hasMessageType, string ap_uri, out string label)
    {
        if (ap_uri is null || ap_uri.length < 1)
            return null;

        Resources  res;

        Individual indv = context.get_individual(ap_uri);

        if (indv.getStatus() != ResultCode.Ok)
            return null;

        label = indv.getFirstLiteral("rdfs:label");

        if (indv.isExists("rdf:type", Resource(DataType.Uri, "v-s:Appointment")))
        {
            res = get_email_from_appointment(sticket, hasMessageType, indv);
        }
        else if (indv.isExists("rdf:type", Resource(DataType.Uri, "v-s:Position")))
        {
            Individual[] l_individuals = context.get_individuals_via_query(
                                                                           sticket.user_uri,
                                                                           "'rdf:type' == 'v-s:Appointment' && 'v-s:occupation' == '" ~ indv.uri ~
                                                                           "'", OptAuthorize.NO, 10000, 10000);

            foreach (individual; l_individuals)
            {
                if (individual.isExists("v-s:deleted", true) == false)
                {
                    Resources tmp_res = get_email_from_appointment(sticket, hasMessageType, individual);

                    foreach (rr; tmp_res)
                        res ~= rr;
                }
            }
        }
        else
        {
            log.trace("ERR! extract_email: fail extract email from [%s], this not appointment or position", ap_uri);
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
            {
                log.trace("new_indv [%s] is draft, ignore", new_indv.uri);
                return ResultCode.Ok;
            }

            if (is_deleted == false && (actualVersion !is null && actualVersion != new_indv.uri /*||
                                                                                                   (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)*/))
            {
                if (actualVersion !is null && actualVersion != new_indv.uri)
                    log.trace("new[%s].v-s:actualVersion[%s] != [%s], ignore", new_indv.uri, actualVersion, new_indv.uri);

//				if (previousVersion_prev !is null && previousVersion_prev == previousVersion_new)
//                  log.trace("prev[%s].v-s:previousVersion[%s] == new[%s].v-s:previousVersion[%s], ignore", prev_indv.uri, previousVersion_prev, new_indv.uri, previousVersion_new);

                return ResultCode.Ok;
            }

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
                return ResultCode.Ok;

            ResultCode rc;
            bool       is_send = false;

            if (is_deleted == false)
            {
                smtp_conn.destroy();
                connect_to_smtp(context);

                string    hasMessageType = new_indv.getFirstLiteral("v-s:hasMessageType");

                string    from         = new_indv.getFirstLiteral("v-wf:from");
                Resources to           = new_indv.getResources("v-wf:to");
                string    subject      = new_indv.getFirstLiteral("v-s:subject");
                Resources reply_to     = new_indv.getResources("v-wf:replyTo");
                string    message_body = new_indv.getFirstLiteral("v-s:messageBody");

                string    senderMailbox    = new_indv.getFirstLiteral("v-s:senderMailbox");
                Resources recipientMailbox = new_indv.getResources("v-s:recipientMailbox");
                Resources attachments      = new_indv.getResources("v-s:attachment");

                if ((from !is null || senderMailbox !is null || default_mail_sender !is null) && (to !is null || recipientMailbox !is null))
                {
                    string from_label;
                    string email_from;

                    if (always_use_mail_sender == true && senderMailbox !is null && senderMailbox.length > 5)
                    {
                        email_from = extract_email(sticket, hasMessageType, default_mail_sender, from_label).getFirstString();
                    }
                    else
                    {
                        email_from = extract_email(sticket, hasMessageType, from, from_label).getFirstString();

                        if ((email_from is null || email_from.length < 5) && default_mail_sender !is null)
                            email_from = extract_email(sticket, hasMessageType, default_mail_sender, from_label).getFirstString();

                        if ((email_from is null || email_from.length < 5) && senderMailbox !is null)
                            email_from = senderMailbox;
                    }

                    if (from_label is null || from_label.length == 0)
                        from_label = "Veda System";

                    string      label;
                    Recipient[] rr_email_to;
                    Recipient[ string ] rr_email_to_hash;

                    foreach (Resource elt; to)
                    {
                        foreach (Resource el; extract_email(sticket, hasMessageType, elt.uri(), label))
                            rr_email_to_hash[ el.data() ] = Recipient(el.data(), label);
                    }

                    foreach (Resource el; recipientMailbox)
                        rr_email_to_hash[ el.data() ] = Recipient(el.data(), "");

                    // !!!! если ставить в отправителя (to) rr_email_to_hash.values, то письма дублируются, причина неизвестна
                    foreach (el; rr_email_to_hash)
                        rr_email_to ~= el;

                    string str_email_reply_to = "";
                    foreach (Resource elt; reply_to)
                    {
                        foreach (Resource el; extract_email(sticket, hasMessageType, elt.uri(), label))
                            str_email_reply_to ~= el.data() ~ ";";
                    }

                    if (email_from.length > 0 && rr_email_to.length > 0)
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

                        foreach (el; attachments)
                        {
                            string attachment_id = el.uri;
                            try
                            {
                                //writeln("@1 attachment_id=", attachment_ids);
                                Individual file_info = context.get_individual(attachment_id);
                                if (file_info !is Individual.init)
                                {
                                    string path     = file_info.getFirstResource("v-s:filePath").get!string;
                                    string file_uri = file_info.getFirstResource("v-s:fileUri").get!string;
                                    string fileName = file_info.getFirstResource("v-s:fileName").get!string;
                                    if (path !is null && file_uri !is null && file_uri.length > 0)
                                    {
                                        if (path.length > 0)
                                            path = path ~ "/";

                                        string full_path  = attachments_db_path ~ "/" ~ path ~ file_uri;
                                        auto   bytes      = cast(ubyte[]) read(full_path);
                                        auto   attachment = SmtpAttachment(fileName, bytes, uri_2_cid(attachment_id));

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

                        foreach (attachment_id; attachment_ids)
                        {
                            try
                            {
                                //writeln("@1 attachment_id=", attachment_ids);
                                Individual file_info = context.get_individual(attachment_id);
                                if (file_info !is Individual.init)
                                {
                                    string path     = file_info.getFirstResource("v-s:filePath").get!string;
                                    string file_uri = file_info.getFirstResource("v-s:fileUri").get!string;
                                    if (path !is null && file_uri !is null && file_uri.length > 0)
                                    {
                                        if (path.length > 0)
                                            path = path ~ "/";

                                        string full_path  = attachments_db_path ~ "/" ~ path ~ file_uri;
                                        auto   bytes      = cast(ubyte[]) read(full_path);
                                        auto   attachment = SmtpAttachment(file_uri, bytes, uri_2_cid(attachment_id));
                                        message.attach(attachment);
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                log.trace("#EX! fail prepare CID attachment for e-mail [%s], LINE:[%s], FILE:[%s], MSG:[%s]", new_indv.uri, ex.line,
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
                                rc = ResultCode.ConnectError;
                            if (res.code == 501)
                                rc = ResultCode.BadRequest;
                        }
                        else
                        {
                            rc      = ResultCode.Ok;
                            is_send = true;
                            //new_indv.addResource("v-s:isSuccess", Resource(true));
                        }

                        //new_indv.addResource("v-s:infoOfExecuting", Resource(text(res)));
                        //new_indv.addResource("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));
                        //new_indv.addResource("rdfs:label", Resource("email, from:" ~ email_from ~ ", to:" ~ email_to));
                        //context.put_individual(&sticket, new_indv.uri, new_indv, false, "fanout");
                    }

                    if (email_from.length == 0)
                    {
                        log.trace("WARN: push_to_smtp[%s]: fail extract_email from field from[%s]", new_indv.uri, from);
                    }

                    if (rr_email_to.length == 0)
                    {
                        log.trace("WARN: push_to_smtp[%s]: fail extract_email from field to[%s]", new_indv.uri, to);
                    }
                }
                else
                {
                    if (from is null || from.length < 5)
                        log.trace("WARN: push_to_smtp[%s]: empty or invalid field from[%s]", new_indv.uri, from);

                    if (to is null || to.length < 5)
                        log.trace("WARN: push_to_smtp[%s]: empty or invalid field to[%s]", new_indv.uri, to);
                }
            }

            if (is_send == false)
                log.trace("WARN: push_to_smtp[%s]: not send", new_indv.uri);

            return rc;
        }
        catch (Throwable ex)
        {
            log.trace("ERR! push_to_smtp[%s], \n%s", new_indv.uri, ex.info);
            return ResultCode.InternalServerError;
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
                Individual connection = context.get_individual(gate.uri);
                //subscribe_on_prefetch(gate.uri);
                Resource   transport = connection.getFirstResource("v-s:transport");
                if (transport != Resource.init)
                {
                    if (transport.data() == "smtp")
                    {
                        try
                        {
                            log.trace("found connect to smtp [%s]", connection);
                            host      = connection.getFirstLiteral("v-s:host");
                            port      = cast(ushort)connection.getFirstInteger("v-s:port");
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
                                    log.trace("ERR! fail connection to smtp server [%s] %s:%d", connection.uri, host, port);
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
                                    log.trace("ERR! fail authenticate to smtp [%s] %s:%d", connection.uri, host, port);
                                    smtp_conn = null;
                                    continue;
                                }

                                default_mail_sender    = connection.getFirstLiteral("v-s:mailSender");
                                always_use_mail_sender = connection.getFirstBoolean("v-s:alwaysUseMailSender");
                            }
                            else
                                log.trace("ERR! smtp server unavailable [%s] %s:%d", connection.uri, host, port);
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
            log.trace("ERR! fanout.connect_to_smtp# LINE:[%s], FILE:[%s], MSG:[%s], %s", ex.line, ex.file, ex.msg, ex.toString);
        }

        if (host is null)
            log.trace("WARN! not found configuration for connection to smtp server");
    }
}
