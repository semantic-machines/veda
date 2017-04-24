module veda.common.ticket_manager;

import std.conv, std.datetime, std.regex, std.uuid, std.stdio;
import veda.common.type, veda.util.module_info, veda.common.logger, veda.common.ticket, veda.core.common.know_predicates, veda.core.common.define;
import veda.onto.individual, veda.onto.resource, veda.core.util.utils, veda.core.az.acl, veda.core.search.vql;

version (isMStorage)
{
    alias veda.mstorage.storage_manager ticket_storage_module;
}

class TicketManager
{
    private Ticket *[ string ] user_of_ticket;
    private Storage       tickets_storage_r;
    private Logger        log;
    private long          last_ticket_manager_op_id = 0;
    private Ticket        sys_ticket;
    private string        module_name;
    private Authorization _acl_indexes;
    private VQL           vql_r;

    this(string _module_name)
    {
        module_name = _module_name;
    }

    public Authorization acl_indexes()
    {
        if (_acl_indexes is null)
            _acl_indexes = new Authorization(acl_indexes_db_path, DBMode.R, module_name ~ ":acl", this.log);

        return _acl_indexes;
    }

    public Ticket *get_ticket(string ticket_id, bool is_systicket = false)
    {
        //StopWatch sw; sw.start;

        try
        {
            Ticket *tt;
            if (ticket_id is null || ticket_id == "" || ticket_id == "systicket")
                ticket_id = "guest";

            tt = user_of_ticket.get(ticket_id, null);

            if (tt is null)
            {
                string when     = null;
                int    duration = 0;

                MInfo  mi = new ModuleInfoFile(text(P_MODULE.ticket_manager), log, OPEN_MODE.READER).get_info();

                //log.trace ("last_ticket_manager_op_id=%d, mi.op_id=%d,  mi.committed_op_id=%d", last_ticket_manager_op_id, mi.op_id, mi.committed_op_id);
                if (last_ticket_manager_op_id < mi.op_id)
                {
                    last_ticket_manager_op_id = mi.op_id;
                    tickets_storage_r.reopen_db();
                }

                string ticket_str = tickets_storage_r.find(false, null, ticket_id);
                if (ticket_str !is null && ticket_str.length > 120)
                {
                    tt = new Ticket;
                    Individual ticket;

                    if (ticket.deserialize(ticket_str) > 0)
                    {
                        subject2Ticket(ticket, tt);
                        tt.result               = ResultCode.OK;
                        user_of_ticket[ tt.id ] = tt;

                        //if (trace_msg[ T_API_80 ] == 1)
                        log.trace("тикет найден в базе, id=%s", ticket_id);
                    }
                    else
                    {
                        tt.result = ResultCode.Unprocessable_Entity;
                        log.trace("ERR! invalid individual=%s", ticket_str);
                    }
                }
                else
                {
                    tt        = new Ticket;
                    tt.result = ResultCode.Ticket_not_found;

                    //if (trace_msg[ T_API_90 ] == 1)
                    log.trace("тикет не найден в базе, id=%s", ticket_id);
                }
            }
            else
            {
                //if (trace_msg[ T_API_100 ] == 1)
                //    log.trace("тикет нашли в кеше, id=%s, end_time=%d", tt.id, tt.end_time);

                SysTime now = Clock.currTime();
                if (now.stdTime >= tt.end_time && !is_systicket)
                {
                    log.trace("ticket %s expired, user=%s, start=%s, end=%s, now=%s", tt.id, tt.user_uri, SysTime(tt.start_time,
                                                                                                                  UTC()).toISOExtString(),
                              SysTime(tt.end_time, UTC()).toISOExtString(), now.toISOExtString());

                    if (ticket_id == "guest")
                    {
                        Ticket guest_ticket = create_new_ticket("cfg:Guest", "900000000", "guest");
                        tt = &guest_ticket;
                    }
                    else
                    {
                        tt        = new Ticket;
                        tt.id     = "?";
                        tt.result = ResultCode.Ticket_expired;
                    }
                    return tt;
                }
                else
                {
                    tt.result = ResultCode.OK;
                }

                //if (trace_msg[ T_API_120 ] == 1)
                //    log.trace("ticket: %s", *tt);
            }
            return tt;
        }
        finally
        {
            //stat(CMD_GET, sw);
        }
    }

    public void get_rights_origin_from_acl(Ticket *ticket, string uri,
                                           void delegate(string resource_group, string subject_group, string right) trace_acl)
    {
        acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, true, trace_acl, null);
    }

    string allow_trusted_group = "cfg:TrustedAuthenticationUserGroup";

    Ticket get_ticket_trusted(string tr_ticket_id, string login)
    {
        Ticket ticket;

        //if (trace_msg[ T_API_60 ] == 1)
        log.trace("trusted authenticate, ticket=[%s] login=[%s]", tr_ticket_id, login);

        ticket.result = ResultCode.Authentication_Failed;

        if (login == null || login.length < 1 || tr_ticket_id.length < 6)
        {
            log.trace("WARN: trusted authenticate: invalid login [%s] or ticket [%s]", login, ticket);
            return ticket;
        }

        Ticket *tr_ticket = get_ticket(tr_ticket_id);
        if (tr_ticket.result == ResultCode.OK)
        {
            bool is_allow_trusted = false;

            void trace_acl(string resource_group, string subject_group, string right)
            {
                //log.trace ("trusted authenticate: %s %s %s", resource_group, subject_group, right);
                if (subject_group == allow_trusted_group)
                    is_allow_trusted = true;
            }

            get_rights_origin_from_acl(tr_ticket, tr_ticket.user_uri, &trace_acl);

            if (is_allow_trusted)
            {
                login = replaceAll(login, regex(r"[-]", "g"), " +");

                Ticket       sticket = sys_ticket;
                Individual[] candidate_users;
                vql_r.get(&sticket, "'" ~ veda_schema__login ~ "' == '" ~ login ~ "'", null, null, 10, 10000, candidate_users, false, false);
                foreach (user; candidate_users)
                {
                    string user_id = user.getFirstResource(veda_schema__owner).uri;
                    if (user_id is null)
                        continue;

                    ticket = create_new_ticket(user_id);

                    log.trace("trusted authenticate, result ticket=[%s]", ticket);
                    return ticket;
                }
            }
            else
            {
                log.trace("ERR: trusted authenticate: User [%s] must be a member of group [%s]", *tr_ticket, allow_trusted_group);
            }
        }
        else
            log.trace("WARN: trusted authenticate: problem ticket [%s]", ticket);

        log.trace("failed trusted authenticate, ticket=[%s] login=[%s]", tr_ticket_id, login);

        ticket.result = ResultCode.Authentication_Failed;
        return ticket;
    }

    private void subject2Ticket(ref Individual ticket, Ticket *tt)
    {
        string when;
        long   duration;

        tt.id       = ticket.uri;
        tt.user_uri = ticket.getFirstLiteral(ticket__accessor);
        when        = ticket.getFirstLiteral(ticket__when);
        string dd = ticket.getFirstLiteral(ticket__duration);

        try
        {
            duration = parse!uint (dd);
        }
        catch (Exception ex)
        {
            writeln("Ex!: ", __FUNCTION__, ":", text(__LINE__), ", ", ex.msg);
        }

        if (tt.user_uri is null)
        {
            //if (trace_msg[ T_API_10 ] == 1)
            log.trace("found a session ticket is not complete, the user can not be found.");
        }

        if (tt.user_uri !is null && (when is null || duration < 10))
        {
            //if (trace_msg[ T_API_20 ] == 1)
            log.trace("found a session ticket is not complete, we believe that the user has not been found.");
            tt.user_uri = null;
        }

        if (when !is null)
        {
            //if (trace_msg[ T_API_30 ] == 1)
            log.trace("session ticket %s Ok, user=%s, when=%s, duration=%d", tt.id, tt.user_uri, when,
                      duration);

            long start_time = stringToTime(when);

            tt.start_time = start_time;
            tt.end_time   = start_time + duration * 10_000_000;
        }
    }

    public Ticket create_new_ticket(string user_id, string duration = "40000", string ticket_id = null)
    {
        //if (trace_msg[ T_API_50 ] == 1)
        log.trace("create_new_ticket, ticket__accessor=%s", user_id);

        Ticket     ticket;
        Individual new_ticket;

        ticket.result = ResultCode.Fail_Store;

        Resources type = [ Resource(ticket__Ticket) ];

        new_ticket.resources[ rdf__type ] = type;

        if (ticket_id !is null && ticket_id.length > 0)
            new_ticket.uri = ticket_id;
        else
        {
            UUID new_id = randomUUID();
            new_ticket.uri = new_id.toString();
        }

        new_ticket.resources[ ticket__accessor ] ~= Resource(user_id);
        new_ticket.resources[ ticket__when ] ~= Resource(getNowAsString());
        new_ticket.resources[ ticket__duration ] ~= Resource(duration);

        version (isMStorage)
        {
            // store ticket
            string     ss_as_binobj = new_ticket.serialize();

            long       op_id;
            ResultCode rc =
                ticket_storage_module.update(P_MODULE.ticket_manager, false, INDV_OP.PUT, null, new_ticket.uri, null, ss_as_binobj, -1, null, -1,
                                             false,
                                             op_id);
            ticket.result = rc;

            if (rc == ResultCode.OK)
            {
                subject2Ticket(new_ticket, &ticket);
                user_of_ticket[ ticket.id ] = new Ticket(ticket);
            }

            log.trace("create new ticket %s, user=%s, start=%s, end=%s", ticket.id, ticket.user_uri, SysTime(ticket.start_time, UTC()).toISOExtString(
                                                                                                                                                      ),
                      SysTime(ticket.end_time, UTC()).toISOExtString());
        }

        version (WebServer)
        {
            subject2Ticket(new_ticket, &ticket);
            user_of_ticket[ ticket.id ] = new Ticket(ticket);
        }

        return ticket;
    }
}

