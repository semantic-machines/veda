/**
 * Внешнее API - Реализация
 */

module veda.core.thread_context;

private
{
    import core.thread, std.stdio, std.format, std.datetime, std.concurrency, std.conv, std.outbuffer, std.string, std.uuid, std.file, std.path;
    import type;
    import bind.xapian_d_header, bind.v8d_header;
    import io.mq_client;
    import util.container, util.logger, util.utils, util.cbor, veda.core.util.cbor8individual, veda.core.util.individual8json;
    import veda.core.know_predicates, veda.core.define, veda.core.context, veda.core.bus_event, veda.core.interthread_signals, veda.core.log_msg;
    import veda.onto.onto, veda.onto.individual, veda.onto.resource, storage.lmdb_storage;
    import az.acl;

    import vibe.data.json;
    import vibe.core.log;
    import vibe.http.client;
    import vibe.stream.operations;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "API");
    return _log;
}
// ////// ////// ///////////////////////////////////////////


Tid    dummy_tid;

string g_str_script_result;
string g_str_script_out;

/// реализация интерфейса Context
class PThreadContext : Context
{
    long local_count_put;
    long local_count_indexed;

    bool[ P_MODULE ] is_traced_module;

    private Ticket *[ string ] user_of_ticket;

    // // // authorization
    private Authorization acl_indexes;

    ScriptVM              script_vm;

    private Onto          onto;

    private string        name;
    private P_MODULE      id;

    private string        old_msg_key2slot;
    private int[ string ] old_key2slot;

    private                string[ string ] prefix_map;

    private LmdbStorage    inividuals_storage;
    private LmdbStorage    tickets_storage;
    private search.vql.VQL _vql;

    private                Tid[ P_MODULE ] name_2_tids;

    private long           local_last_update_time;
    private Individual     node = Individual.init;
    private string         node_id;

    this(string _node_id, string context_name, P_MODULE _id)
    {
//        if (_node_id is null)
//        {
//            writeln("---NODE_ID IS NULL---");
//            printPrettyTrace(stdout);
//            writeln("^^^NODE_ID IS NULL^^^");
//        }

        node_id = _node_id;

        inividuals_storage = new LmdbStorage(individuals_db_path, DBMode.R, context_name ~ ":inividuals");
        tickets_storage    = new LmdbStorage(tickets_db_path, DBMode.R, context_name ~ ":tickets");
        acl_indexes        = new Authorization(acl_indexes_db_path, DBMode.R, context_name ~ ":acl");

        name = context_name;

        foreach (id; P_MODULE.min .. P_MODULE.max)
        {
            name_2_tids[ id ] = locate(text(id));
        }

        id = _id;

        is_traced_module[ P_MODULE.ticket_manager ]   = true;
        is_traced_module[ P_MODULE.subject_manager ]  = true;
        is_traced_module[ P_MODULE.acl_manager ]      = true;
        is_traced_module[ P_MODULE.fulltext_indexer ] = true;
        is_traced_module[ P_MODULE.condition ]        = true;

        getConfiguration();

        _vql = new search.vql.VQL(this);

        onto = new Onto(this);
        onto.load();

        local_count_put     = get_count_put();
        local_count_indexed = get_count_indexed();

        log.trace_log_and_console("NEW CONTEXT [%s], external: write storage=[%s], js_vm=[%s]", context_name, external_write_storage_url,
                                  external_js_vm_url);
    }

    @property
    public Ticket sys_ticket(bool is_new = false)
    {
        Ticket ticket = get_global_systicket();

        if (ticket == Ticket.init || ticket.user_uri == "" || is_new)
        {
            try
            {
                ticket = create_new_ticket("v-a:VedaSystem");
            }
            catch (Exception ex)
            {
                printPrettyTrace(stderr);
                log.trace("context.sys_ticket:EX!%s", ex.msg);
            }

            if (ticket.user_uri == "")
                ticket.user_uri = "v-a:VedaSystem";

            set_global_systicket(ticket);
        }
        return ticket;
    }

    public Individual getConfiguration()
    {
        if (node == Individual.init && node_id !is null)
        {
            this.reopen_ro_subject_storage_db();
            Ticket sticket = sys_ticket();

            node = get_individual(&sticket, node_id);
            if (node.getStatus() != ResultCode.OK)
                node = Individual.init;

            set_g_external_write_storage_url(node.getFirstLiteral("vsrv:write_storage_node"));
//            external_js_vm         = node.getFirstLiteral("vsrv:jsvm_node");
        }
        return node;
    }

    public Onto get_onto()
    {
        if (onto !is null)
        {
            check_for_reload("onto", &onto.load);
        }

        return onto;
    }

    public long get_last_update_time()
    {
        long lut;

        send(getTid(P_MODULE.xapian_thread_context), CMD.GET, CNAME.LAST_UPDATE_TIME, thisTid);
        receive((long tm)
                {
                    lut = tm;
                });
        return lut;
    }

    private void reload_scripts()
    {
        Script[] scripts;
        writeln("-");

        foreach (path; [ "./public/js/server", "./public/js/common" ])
        {
            auto oFiles = dirEntries(path, SpanMode.depth);

            foreach (o; oFiles)
            {
                if (extension(o.name) == ".js")
                {
                    log.trace(" load script:%s", o);
                    auto str_js        = cast(ubyte[]) read(o.name);
                    auto str_js_script = script_vm.compile(cast(char *)(cast(char[])str_js ~ "\0"));
                    if (str_js_script !is null)
                        scripts ~= str_js_script;
                }
            }
        }

        foreach (script; scripts)
        {
            script_vm.run(script);
        }
    }

    ScriptVM get_ScriptVM()
    {
        if (script_vm is null)
        {
            try
            {
                script_vm = new_ScriptVM();
                g_context = this;

                string g_str_script_result = new char[ 1024 * 64 ];
                string g_str_script_out    = new char[ 1024 * 64 ];

                g_script_result.data           = cast(char *)g_str_script_result;
                g_script_result.allocated_size = cast(int)g_str_script_result.length;

                g_script_out.data           = cast(char *)g_str_script_out;
                g_script_out.allocated_size = cast(int)g_str_script_out.length;

                reload_scripts();
            }
            catch (Exception ex)
            {
                writeln("EX!get_ScriptVM ", ex.msg);
            }
        }

        return script_vm;
    }

    string[ 2 ] execute_script(string str_js)
    {
        string[ 2 ] res;
        get_ScriptVM();

        reload_scripts();

        try
        {
            auto str_js_script = script_vm.compile(cast(char *)(cast(char[])str_js ~ "\0"));
            if (str_js_script !is null)
                script_vm.run(str_js_script, &g_script_result);
            else
                writeln("Script is invalid");

            res[ 0 ] = cast(string)g_script_result.data[ 0..g_script_result.length ];
            res[ 1 ] = "NONE";
        }
        catch (Exception ex)
        {
            writeln("EX!executeScript ", ex.msg);
            res[ 0 ] = ex.msg;
            res[ 1 ] = "NONE";
        }

        return res;
    }
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    bool authorize(string uri, Ticket *ticket, ubyte request_acess)
    {
        if (ticket is null)
        {
            printPrettyTrace(stderr);
        }

        //writeln ("@p ### uri=", uri, " ", request_acess);
        ubyte res = acl_indexes.authorize(uri, ticket, request_acess, this);

        //writeln ("@p ### uri=", uri, " ", request_acess, " ", request_acess == res);
        return request_acess == res;
    }

    public string get_name()
    {
        return name;
    }

    public Individual[ string ] get_onto_as_map_individuals()
    {
        if (onto !is null)
        {
            check_for_reload("onto", &onto.load);
            return onto.get_individuals;
        }
        else
            return (Individual[ string ]).init;
    }


    public string get_individual_from_storage(string uri)
    {
        //writeln ("@ get_individual_as_cbor, uri=", uri);
        string res;

        if (inividuals_storage !is null)
            res = inividuals_storage.find(uri);
        else
            res = find(uri);

        if (res !is null && res.length < 10)
            log.trace_log_and_console("!ERR:get_individual_from_storage, found invalid CBOR, uri=%s", uri);

        return res;
    }
// /////////////////////////////////////////// oykumena ///////////////////////////////////////////////////

    public void push_signal(string key, long value)
    {
        try
        {
            Tid tid_interthread_signals = getTid(P_MODULE.interthread_signals);

            if (tid_interthread_signals != Tid.init)
            {
                send(tid_interthread_signals, CMD.PUT, key, value);
            }

            set_reload_signal_to_local_thread(key);
        }
        catch (Exception ex)
        {
            writeln(__FUNCTION__ ~ "", ex.msg);
        }
    }

    public void push_signal(string key, string value)
    {
        try
        {
            Tid tid_interthread_signals = getTid(P_MODULE.interthread_signals);

            if (tid_interthread_signals != Tid.init)
            {
                send(tid_interthread_signals, CMD.PUT, key, value);
            }
        }
        catch (Exception ex)
        {
            writeln(__FUNCTION__ ~ "", ex.msg);
        }
    }

// /////////////////////////////////////////////////////////////////////////////////////////////

    public Tid getTid(P_MODULE tid_id)
    {
        Tid res = name_2_tids.get(tid_id, Tid.init);

        if (res == Tid.init)
        {
            // tid not found, attempt restore
            Tid tmp_tid = locate(text(tid_id));

            if (tmp_tid == Tid.init)
            {
//                writeln("!!! NOT FOUND TID=", text(tid_id), "\n", name_2_tids, ", locate=1 ", );
                throw new Exception("!!! NOT FOUND TID=" ~ text(tid_id));
            }
            else
            {
                name_2_tids[ tid_id ] = tmp_tid;
                return tmp_tid;
            }
            //assert(false);
        }
        return res;
    }

    public int[ string ] get_key2slot()
    {
        string key2slot_str;

        if (inividuals_storage !is null)
            key2slot_str = inividuals_storage.find(xapian_metadata_doc_id);
        else
            key2slot_str = find(xapian_metadata_doc_id);

        if (key2slot_str !is null)
        {
            int[ string ] key2slot = deserialize_key2slot(key2slot_str);
            return key2slot;
        }
        return (int[ string ]).init;
    }

    ref string[ string ] get_prefix_map()
    {
        return prefix_map;
    }

    void add_prefix_map(ref string[ string ] arg)
    {
        foreach (key, value; arg)
        {
            prefix_map[ key ] = value;
        }
    }

    @property search.vql.VQL vql()
    {
        return _vql;
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

//				writeln ("tt.userId=", tt.userId);

        if (tt.user_uri is null)
        {
            if (trace_msg[ 22 ] == 1)
                log.trace("найденный сессионный билет не полон, пользователь не найден");
        }

        if (tt.user_uri !is null && (when is null || duration < 10))
        {
            if (trace_msg[ 23 ] == 1)
                log.trace(
                          "найденный сессионный билет не полон, считаем что пользователь не был найден");
            tt.user_uri = null;
        }

        if (when !is null)
        {
            if (trace_msg[ 24 ] == 1)
                log.trace("сессионный билет %s Ok, user=%s, when=%s, duration=%d", tt.id, tt.user_uri, when,
                          duration);

            // TODO stringToTime очень медленная операция ~ 100 микросекунд
            tt.end_time = stringToTime(when) + duration * 10_000_000;                     //? hnsecs?
        }
    }

    private void stat(CMD command_type, ref StopWatch sw, string func = __FUNCTION__) nothrow
    {
        try
        {
            sw.stop();
            int t = cast(int)sw.peek().usecs;

            Tid statistic_data_accumulator_tid = this.getTid(P_MODULE.statistic_data_accumulator);

            send(statistic_data_accumulator_tid, CMD.PUT, CNAME.WORKED_TIME, t);

//        send(this.getTid(P_MODULE.statistic_data_accumulator), CMD.PUT, CNAME.COUNT_COMMAND, 1);

            if (command_type == CMD.GET)
                send(statistic_data_accumulator_tid, CMD.PUT, CNAME.COUNT_GET, 1);
            else
                send(statistic_data_accumulator_tid, CMD.PUT, CNAME.COUNT_PUT, 1);

            if (trace_msg[ 555 ] == 1)
                log.trace(func[ (func.lastIndexOf(".") + 1)..$ ] ~ ": t=%d µs", t);
        }
        catch (Exception ex)
        {
        }
    }

    // ////////////////////////////////////////////////////////////////////////////////
    struct Signal
    {
        long time_update = 0;
        long time_check  = 0;
    }

    Signal *[ string ] signals;

    int timeout = 10;

    public void set_reload_signal_to_local_thread(string interthread_signal_id)
    {
        Signal *signal = signals.get(interthread_signal_id, null);

        if (signal == null)
        {
            signal                           = new Signal;
            signals[ interthread_signal_id ] = signal;
        }

        long now = Clock.currStdTime() / 10000000;
        signal.time_update = now;

        if (trace_msg[ 19 ] == 1)
            log.trace("[%s] SET RELOAD LOCAL SIGNAL [%s], signal.time_update=%d", name, interthread_signal_id,
                      signal.time_update);
    }

    long local_time_check_indexed = 0;
    public bool ft_check_for_reload(void delegate() load)
    {
        long now = Clock.currStdTime() / 10000000;
//        log.trace ("@ft_check_for_reload: #1");

        if (now - local_time_check_indexed > timeout)
        {
            long count_indexed = get_count_indexed();
            //log.trace ("@ft_check_for_reload:count_indexed=%d", count_indexed);
            //log.trace ("@ft_check_for_reload:local_count_indexed=%d", local_count_indexed);

            local_time_check_indexed = now;
            if (count_indexed > local_count_indexed)
            {
                //log.trace ("@ft_check_for_reload:execute load");
                local_count_indexed      = count_indexed;
                load();
                return true;
            }
        }
        return false;
    }

    public bool check_for_reload(string interthread_signal_id, void delegate() load)
    {
        Signal *local = signals.get(interthread_signal_id, null);

        long   now = Clock.currStdTime() / 10000000;

        if (local == null)
        {
            if (trace_msg[ 19 ] == 1)
                log.trace("[%s] NEW SIGNAL OBJ for [%s] ", name, interthread_signal_id);

            local                            = new Signal;
            local.time_update                = now;
            local.time_check                 = now - timeout - 1;
            signals[ interthread_signal_id ] = local;
        }


        if (now - local.time_check > timeout)
        {
            if (trace_msg[ 19 ] == 1)
                log.trace("[%s] CHECK FOR RELOAD #1 [%s], (now-local.time_update)=%d, (now-local.time_check)=%d", name,
                          interthread_signal_id, now - local.time_update, now - local.time_check);

            if (local.time_update > local.time_check)
            {
                if (trace_msg[ 19 ] == 1)
                    log.trace("[%s] NOW RELOAD FOR [%s], local.time_update=%d > local.time_check=%d", name, interthread_signal_id,
                              local.time_update, local.time_check);

                local.time_check  = now;
                local.time_update = now;

                load();

                return true;
            }
            else
            {
                local.time_check = now;

                long stored_time_signal = look_integer_signal(interthread_signal_id) / 1000;
                if (stored_time_signal == 0)
                    return false;

                if (trace_msg[ 19 ] == 1)
                    log.trace("[%s] CHECK FOR RELOAD #2 [%s], stored_time_signal=%d, local.time_update=%d, delta=%d",
                              name, interthread_signal_id, stored_time_signal, local.time_update, stored_time_signal - local.time_update);

                if (local.time_update < stored_time_signal)
                {
                    if (trace_msg[ 19 ] == 1)
                        log.trace("[%s] NOW RELOAD FOR [%s], local.time_update=%d < stored_time_signal=%d", name, interthread_signal_id,
                                  local.time_update, stored_time_signal);

                    local.time_update = stored_time_signal;

                    load();

                    return true;
                }
            }
        }
        return false;
    }

    public long look_integer_signal(string key)
    {
        Tid myTid                   = thisTid;
        Tid tid_interthread_signals = getTid(P_MODULE.interthread_signals);

        if (tid_interthread_signals !is Tid.init)
        {
            send(tid_interthread_signals, CMD.GET, key, DataType.Integer, myTid);

            long res;

            receive((long msg)
                    {
                        res = msg;
                    });

            return res;
        }
        return 0;
    }

    public string look_string_signal(string key)
    {
        Tid myTid                   = thisTid;
        Tid tid_interthread_signals = getTid(P_MODULE.interthread_signals);

        if (tid_interthread_signals !is Tid.init)
        {
            send(tid_interthread_signals, CMD.GET, key, DataType.String, myTid);

            string res;

            receive((string msg)
                    {
                        res = msg;
                    });

            return res;
        }
        return null;
    }


    // *************************************************** external api *********************************** //

    // /////////////////////////////////////////////////////// TICKET //////////////////////////////////////////////

    public bool is_ticket_valid(string ticket_id)
    {
        //StopWatch sw; sw.start;

        try
        {
//        writeln("@is_ticket_valid, ", ticket_id);
            Ticket *ticket = get_ticket(ticket_id);

            if (ticket is null)
            {
                return false;
            }

            SysTime now = Clock.currTime();
            if (now.stdTime < ticket.end_time)
                return true;

            return false;
        }
        finally
        {
//            stat(CMD.GET, sw);
        }
    }

    private Ticket create_new_ticket(string user_id)
    {
        Ticket ticket;

        if (external_write_storage_url !is null)
        {
            writeln("$$$ create_new_ticket EXTERNAL");
            return ticket;
        }

        Individual new_ticket;

        new_ticket.resources[ rdf__type ] ~= Resource(ticket__Ticket);

        UUID new_id = randomUUID();
        new_ticket.uri = new_id.toString();

        new_ticket.resources[ ticket__accessor ] ~= Resource(user_id);
        new_ticket.resources[ ticket__when ] ~= Resource(getNowAsString());
        new_ticket.resources[ ticket__duration ] ~= Resource("40000");

        if (trace_msg[ 18 ] == 1)
            log.trace("authenticate, ticket__accessor=%s", user_id);

        // store ticket
        string ss_as_cbor = individual2cbor(&new_ticket);

        Tid    tid_ticket_manager = getTid(P_MODULE.ticket_manager);

        if (tid_ticket_manager != Tid.init)
        {
            send(tid_ticket_manager, CMD.PUT, ss_as_cbor, thisTid);
            receive((EVENT ev, string prev_state, string msg, Tid from)
                    {
                        if (from == getTid(P_MODULE.ticket_manager))
                        {
//                            res = msg;
                            //writeln("context.store_subject:msg=", msg);
                            subject2Ticket(new_ticket, &ticket);
                            ticket.result = ResultCode.OK;
                            user_of_ticket[ ticket.id ] = &ticket;
                        }
                    });
        }
        return ticket;
    }

    public Ticket authenticate(string login, string password)
    {
        StopWatch sw; sw.start;

        Ticket    ticket;

        try
        {
            if (external_write_storage_url !is null)
            {
                string url = external_write_storage_url ~ "/authenticate";
                try
                {
                    requestHTTP(url ~ "?login=" ~ login ~ "&password=" ~ password,
                                (scope req) {
                                    req.method = HTTPMethod.GET;
                                },
                                (scope res) {
                                    auto res_json = res.readJson();
                                    if (res_json[ "result" ] == 200)
                                    {
                                        ticket.id = res_json[ "id" ].get!string;
                                        ticket.user_uri = res_json[ "user_uri" ].get!string;
                                        ticket.end_time = res_json[ "end_time" ].get!long;
                                        ticket.result = ResultCode.OK;
                                    }
                                }
                                );
                }
                catch (Exception ex)
                {
                    writeln("ERR! authenticate:", ex.msg, ", url=", url);
                }
            }
            else
            {
                if (trace_msg[ 18 ] == 1)
                    log.trace("authenticate, login=[%s] password=[%s]", login, password);

                ticket.result = ResultCode.Authentication_Failed;

                if (login == null || login.length < 1 || password == null || password.length < 6)
                    return ticket;

                if (this.getTid(P_MODULE.subject_manager) != Tid.init)
                    this.wait_thread(P_MODULE.subject_manager);
                if (this.getTid(P_MODULE.fulltext_indexer) != Tid.init)
                    this.wait_thread(P_MODULE.fulltext_indexer);

                Ticket       sticket         = sys_ticket;
                Individual[] candidate_users = get_individuals_via_query(&sticket, "'" ~ veda_schema__login ~ "' == '" ~ login ~ "'");
                foreach (user; candidate_users)
                {
                    string user_id = user.getFirstResource(veda_schema__owner).uri;
                    if (user_id is null)
                        continue;

                    Resources pass = user.resources.get(veda_schema__password, _empty_Resources);
                    if (pass.length > 0 && pass[ 0 ] == password)
                    {
                        ticket = create_new_ticket(user_id);
                        return ticket;
                    }
                }

                log.trace("fail authenticate, login=[%s] password=[%s]", login, password);

                ticket.result = ResultCode.Authentication_Failed;
            }
            return ticket;
        }
        finally
        {
            stat(CMD.PUT, sw);
        }
    }

    public Ticket *get_ticket(string ticket_id)
    {
        //StopWatch sw; sw.start;

        try
        {
            Ticket *tt = user_of_ticket.get(ticket_id, null);

            if (tt is null)
            {
                string when     = null;
                int    duration = 0;

                string ticket_str = tickets_storage.find(ticket_id);
                if (ticket_str !is null && ticket_str.length > 128)
                {
                    tt = new Ticket;
                    Individual ticket;

                    if (cbor2individual(&ticket, ticket_str) > 0)
                    {
                        subject2Ticket(ticket, tt);
                        tt.result               = ResultCode.OK;
                        user_of_ticket[ tt.id ] = tt;

                        if (trace_msg[ 17 ] == 1)
                            log.trace("тикет найден в базе, id=%s", ticket_id);
                    }
                    else
                    {
                        tt.result = ResultCode.Unprocessable_Entity;
                        log.trace("!ERR:invalid individual=%s", ticket_str);
                    }
                }
                else
                {
                    tt        = new Ticket;
                    tt.result = ResultCode.Ticket_expired;

                    if (trace_msg[ 17 ] == 1)
                        log.trace("тикет не найден в базе, id=%s", ticket_id);
                }
            }
            else
            {
                if (trace_msg[ 17 ] == 1)
                    log.trace("тикет нашли в кеше, id=%s", ticket_id);

                SysTime now = Clock.currTime();
                if (now.stdTime >= tt.end_time)
                {
                    if (trace_msg[ 17 ] == 1)
                        log.trace("тикет просрочен, id=%s", ticket_id);
                    tt        = new Ticket;
                    tt.result = ResultCode.Ticket_expired;
                    return tt;
                }
            }
            return tt;
        }
        finally
        {
            //stat(CMD.GET, sw);
        }
    }


    // //////////////////////////////////////////// INDIVIDUALS IO /////////////////////////////////////
    public Individual[] get_individuals_via_query(Ticket *ticket, string query_str)
    {
        StopWatch sw; sw.start;

        if (trace_msg[ 26 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individuals_via_query: start, query_str=%s, ticket=%s", query_str, ticket.id);
            else
                log.trace("get_individuals_via_query: start, query_str=%s, ticket=null", query_str);
        }

        try
        {
            if (query_str.indexOf("==") > 0 || query_str.indexOf("&&") > 0 || query_str.indexOf("||") > 0)
            {
            }
            else
            {
                query_str = "'*' == '" ~ query_str ~ "'";
            }

            Individual[] res;
            vql.get(ticket, query_str, null, null, 10, 10000, res);
            return res;
        }
        finally
        {
            stat(CMD.GET, sw);

            if (trace_msg[ 26 ] == 1)
                log.trace("get_individuals_via_query: end, query_str=%s", query_str);
        }
    }

    public void reopen_ro_fulltext_indexer_db()
    {
        try
        {
            if (this.getTid(P_MODULE.fulltext_indexer) != Tid.init)
                this.wait_thread(P_MODULE.fulltext_indexer);
        }
        catch (Exception ex) {}

        if (vql !is null)
            vql.reopen_db();
    }

    public void reopen_ro_subject_storage_db()
    {
        try
        {
            if (this.getTid(P_MODULE.subject_manager) != Tid.init)
                this.wait_thread(P_MODULE.subject_manager);
        }
        catch (Exception ex) {}

        if (inividuals_storage !is null)
            inividuals_storage.reopen_db();
    }

    public void reopen_ro_acl_storage_db()
    {
        try
        {
            if (this.getTid(P_MODULE.acl_manager) != Tid.init)
                this.wait_thread(P_MODULE.acl_manager);
        }
        catch (Exception ex) {}

        if (acl_indexes !is null)
            acl_indexes.reopen_db();
    }

    // ////////// external ////////////

    public ubyte get_rights(Ticket *ticket, string uri)
    {
        return acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, this);
    }

    public void get_rights_origin(Ticket *ticket, string uri,
                                  void delegate(string resource_group, string subject_group, string right) trace)
    {
        acl_indexes.authorize(uri, ticket, Access.can_create | Access.can_read | Access.can_update | Access.can_delete, this, trace);
    }

    public immutable(string)[] get_individuals_ids_via_query(Ticket * ticket, string query_str, string sort_str, string db_str = null)
    {
        StopWatch sw; sw.start;

        try
        {
            if (query_str.indexOf("==") > 0 || query_str.indexOf("&&") > 0 || query_str.indexOf("||") > 0)
            {
            }
            else
            {
                query_str = "'*' == '" ~ query_str ~ "'";
            }

            immutable(string)[] res;
            vql.get(ticket, query_str, sort_str, db_str, 100000, res);
            return res;
        }
        finally
        {
            stat(CMD.GET, sw);
        }
    }

    public Individual get_individual(Ticket *ticket, string uri)
    {
        StopWatch sw; sw.start;

        if (trace_msg[ 25 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individual, uri=%s, ticket=%s", uri, ticket.id);
            else
                log.trace("get_individual, uri=%s, ticket=null", uri);
        }

        try
        {
            Individual individual = Individual.init;

            if (acl_indexes.authorize(uri, ticket, Access.can_read, this) == Access.can_read)
            {
                string individual_as_cbor = get_individual_from_storage(uri);

                if (individual_as_cbor !is null && individual_as_cbor.length > 1)
                {
                    if (cbor2individual(&individual, individual_as_cbor) > 0)
                        individual.setStatus(ResultCode.OK);
                    else
                    {
                        individual.setStatus(ResultCode.Unprocessable_Entity);
                        writeln("ERR!: invalid cbor: [", individual_as_cbor, "] ", uri);
                    }
                }
                else
                {
                    individual.setStatus(ResultCode.Unprocessable_Entity);
                    //writeln ("ERR!: empty cbor: [", individual_as_cbor, "] ", uri);
                }
            }
            else
            {
                if (trace_msg[ 25 ] == 1)
                    log.trace("get_individual, not authorized, uri=%s", uri);
                individual.setStatus(ResultCode.Not_Authorized);
            }

            return individual;
        }
        finally
        {
            stat(CMD.GET, sw);
            if (trace_msg[ 25 ] == 1)
                log.trace("get_individual: end, uri=%s", uri);
        }
    }

    public Individual[] get_individuals(Ticket *ticket, string[] uris)
    {
        StopWatch sw; sw.start;

        try
        {
            Individual[] res = Individual[].init;

            foreach (uri; uris)
            {
                if (acl_indexes.authorize(uri, ticket, Access.can_read, this) == Access.can_read)
                {
                    Individual individual         = Individual.init;
                    string     individual_as_cbor = get_individual_from_storage(uri);

                    if (individual_as_cbor !is null && individual_as_cbor.length > 1)
                    {
                        if (cbor2individual(&individual, individual_as_cbor) > 0)
                            res ~= individual;
                        else
                        {
                            Individual indv;
                            indv.uri = uri;
                            indv.setStatus(ResultCode.Unprocessable_Entity);
                            res ~= indv;
                        }
                    }
                }
            }

            return res;
        }
        finally
        {
            stat(CMD.GET, sw);
        }
    }

    public string get_individual_as_cbor(Ticket *ticket, string uri, out ResultCode rs)
    {
        string    res;
        StopWatch sw; sw.start;

        rs = ResultCode.Unprocessable_Entity;


        if (trace_msg[ 25 ] == 1)
        {
            if (ticket !is null)
                log.trace("get_individual as cbor, uri=%s, ticket=%s", uri, ticket.id);
            else
                log.trace("get_individual as cbor, uri=%s, ticket=null", uri);
        }

        try
        {
            if (acl_indexes.authorize(uri, ticket, Access.can_read, this) == Access.can_read)
            {
                string individual_as_cbor = get_individual_from_storage(uri);

                if (individual_as_cbor !is null && individual_as_cbor.length > 1)
                {
                    res = individual_as_cbor;
                    rs  = ResultCode.OK;
                }
                else
                {
                    //writeln ("ERR!: empty cbor: ", uri);
                }
            }
            else
            {
                if (trace_msg[ 25 ] == 1)
                    log.trace("get_individual as cbor, not authorized, uri=%s", uri);
                rs = ResultCode.Not_Authorized;
            }

            return res;
        }
        finally
        {
            stat(CMD.GET, sw);
            if (trace_msg[ 25 ] == 1)
                log.trace("get_individual as cbor: end, uri=%s", uri);
        }
    }

    private OpResult store_individual(CMD cmd, Ticket *ticket, Individual *indv, bool prepare_events, string event_id)
    {
        //writeln("context:store_individual #1 ", process_name);
        StopWatch sw; sw.start;

        OpResult  res = OpResult(ResultCode.Fail_Store, -1);

        if (indv !is null && (indv.uri is null || indv.uri.length < 2))
        {
            log.trace("Ex! store_subject:%s", indv);
            res.result = ResultCode.Unprocessable_Entity;
            return res;
        }
        if (indv is null || indv.resources.length == 0)
        {
            res.result = ResultCode.No_Content;
            return res;
        }
        //writeln("context:store_individual #2 ", process_name);

        try
        {
            if (external_write_storage_url !is null)
            {
                if (trace_msg[ 27 ] == 1)
                    log.trace("[%s] store_individual use EXTERNAL", name);

                string url;
                //string _external_write_storage_url_ = "http://127.0.0.1:8111";

                if (cmd == CMD.PUT)
                    url = external_write_storage_url ~ "/put_individual";
                else if (cmd == CMD.ADD)
                    url = external_write_storage_url ~ "/add_to_individual";
                else if (cmd == CMD.SET)
                    url = external_write_storage_url ~ "/set_in_individual";
                else if (cmd == CMD.REMOVE)
                    url = external_write_storage_url ~ "/remove_from_individual";

                //writeln("context:store_individual use EXTERNAL #3, url=", url, " ", process_name);

                Json req_body = Json.emptyObject;
                try
                {
                    req_body[ "ticket" ]         = ticket.id;
                    req_body[ "individual" ]     = individual_to_json(*indv);
                    req_body[ "prepare_events" ] = prepare_events;
                    req_body[ "event_id" ]       = event_id;

                    requestHTTP(url,
                                (scope req) {
                                    req.method = HTTPMethod.PUT;
                                    req.writeJsonBody(req_body);
                                    //writeln("req:", text (req_body));
                                },
                                (scope result) {
                                    //logInfo("Response: %s", text(result.statusCode));
                                    res.result = cast(ResultCode)result.statusCode;
                                }
                                );

//                    res = ResultCode.OK;
                    //    writeln("context:store_individual #4 ", process_name);

                    return res;
                }
                catch (Exception ex)
                {
                    writeln("ERR! external put_individual:", ex.msg, ", url=", url, ", req_body=", text(req_body));
                    res.result = ResultCode.Internal_Server_Error;
                    return res;
                }
            }
            else
            {
                //  writeln("context:store_individual #5 ", process_name);

                Tid    tid_subject_manager;
                Tid    tid_acl;

                string ss_as_cbor = individual2cbor(indv);

                if (trace_msg[ 27 ] == 1)
                    log.trace("[%s] store_individual: %s", name, *indv);

                Resource[ string ] rdfType;

                setMapResources(indv.resources.get(rdf__type, Resources.init), rdfType);

                if (rdfType.anyExist(veda_schema__Membership) == true)
                {
                    // before storing the data, expected availability acl_manager.
                    wait_thread(P_MODULE.acl_manager);
                    if (this.acl_indexes.isExistMemberShip(indv) == true)
                    {
                        res.result = ResultCode.Duplicate_Key;
                        return res;
                    }
                }
                else if (rdfType.anyExist(veda_schema__PermissionStatement) == true)
                {
                    // before storing the data, expected availability acl_manager.
                    wait_thread(P_MODULE.acl_manager);
                    if (this.acl_indexes.isExistPermissionStatement(indv) == true)
                    {
                        res.result = ResultCode.Duplicate_Key;
                        return res;
                    }
                }

                EVENT ev = EVENT.NONE;

                tid_subject_manager = getTid(P_MODULE.subject_manager);
                string prev_state;
                if (tid_subject_manager != Tid.init)
                {
                    send(tid_subject_manager, cmd, ss_as_cbor, thisTid);
                    receive((EVENT _ev, string _prev_state, string _new_state, Tid from)
                            {
                                if (from == getTid(P_MODULE.subject_manager))
                                    ev = _ev;
                                prev_state = _prev_state;
                                ss_as_cbor = _new_state;
                                res.op_id = get_count_put();
                            });
                }

                if (ev == EVENT.NOT_READY)
                {
                    res.result = ResultCode.Not_Ready;
                    return res;
                }

                if (ev == EVENT.ERROR)
                {
                    res.result = ResultCode.Fail_Store;
                    return res;
                }

                if (ev == EVENT.CREATE || ev == EVENT.UPDATE)
                {
                    if (indv.isExist(veda_schema__deleted, true) == false)
                    {
                        Tid tid_search_manager = getTid(P_MODULE.fulltext_indexer);

                        if (tid_search_manager != Tid.init)
                        {
                            send(tid_search_manager, CMD.PUT, ss_as_cbor, res.op_id);
                        }
                    }
                    else
                    {
                        Tid tid_search_manager = getTid(P_MODULE.fulltext_indexer);

                        if (tid_search_manager != Tid.init)
                        {
                            send(tid_search_manager, CMD.DELETE, ss_as_cbor, res.op_id);
                        }
                    }

                    if (prepare_events == true)
                    {
                        bus_event_after(ticket, indv, rdfType, ss_as_cbor, prev_state, ev, this, event_id, res.op_id);
                    }

                    Tid tid_fanout = getTid(P_MODULE.fanout);
                    if (tid_fanout != Tid.init)
                    {
                        send(tid_fanout, CMD.PUT, prev_state, ss_as_cbor);
                    }

                    res.result = ResultCode.OK;

                    return res;
                }
                else
                {
                    log.trace("Ex! store_subject:%s", ev);

                    res.result = ResultCode.Internal_Server_Error;
                    return res;
                }
            }
        }
        finally
        {
            //writeln("context:store_individual #e ", process_name);
            if (trace_msg[ 27 ] == 1)
                log.trace("[%s] store_individual [%s] = %s", name, indv.uri, res);

            stat(CMD.PUT, sw);
        }
    }

    public OpResult put_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id)
    {
        individual.uri = uri;
        return store_individual(CMD.PUT, ticket, &individual, prepareEvents, event_id);
    }

    public OpResult add_to_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id)
    {
        individual.uri = uri;
        return store_individual(CMD.ADD, ticket, &individual, prepareEvents, event_id);
    }

    public OpResult set_in_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id)
    {
        individual.uri = uri;
        return store_individual(CMD.SET, ticket, &individual, prepareEvents, event_id);
    }

    public OpResult remove_from_individual(Ticket *ticket, string uri, Individual individual, bool prepareEvents, string event_id)
    {
        individual.uri = uri;
        return store_individual(CMD.REMOVE, ticket, &individual, prepareEvents, event_id);
    }

/////////////////////////////////////////////////////////////////////////////
    public long get_operation_state(P_MODULE module_id)
    {
        long res = -1;


        if (module_id == P_MODULE.condition)
        {
            if (external_js_vm_url !is null)
            {
                //writeln("context: get_operation_state: #1 EXTERNAL ", text(module_id), " ", process_name);
                string url = external_js_vm_url ~ "/get_operation_state?module_id=" ~ text(cast(int)module_id);
//                string url = "http://127.0.0.1:8555" ~ "/get_operation_state?module_id=" ~ text(cast(int)module_id);
                try
                {
                    requestHTTP(url,
                                (scope req) {
                                    req.method = HTTPMethod.GET;
                                },
                                (scope h_res) {
                                    auto res_as_str = h_res.bodyReader.readAllUTF8();
                                    res = to!long (res_as_str);
                                    //writeln("context: get_operation_state: #3 EXTERNAL ", text(module_id), ", url=", url, ",res=", res, " *", process_name);
                                }
                                );
                }
                catch (Exception ex)
                {
                    writeln("EX!get_operation_state:", ex.msg, ", url=", url);
                }
                //writeln("context: get_operation_state: #E EXTERNAL ", text(module_id), " ", process_name);
            }
            else
            {
                long _op_id = get_scripts_op_id;
                //writeln("context: get_operation_state: #E ", text(module_id), "op_id=", _op_id, " *", process_name);
                //core.thread.Thread.sleep(100.msecs);
                return _op_id;
            }
        }
        else if (module_id == P_MODULE.acl_manager)
        {
            return get_acl_manager_op_id;
        }
        else if (module_id == P_MODULE.fulltext_indexer)
        {
            return get_count_indexed;
        }
        else if (module_id == P_MODULE.subject_manager)
        {
            return get_count_put;
        }

        return res;
    }


    public long wait_thread(P_MODULE module_id, long op_id = 0)
    {
        if (module_id == id)
            return -1;

/*
                if (module_id == P_MODULE.fulltext_indexer)
                {
                Tid tid = this.getTid(module_id);
                if (tid != Tid.init)
                {
                        //writeln ("SEND COMMIT");
                        send(tid, CMD.COMMIT, "", thisTid);
                        core.thread.Thread.sleep(10.msecs);
                        }
        }
 */
        if (external_js_vm_url !is null && module_id == P_MODULE.condition)
        {
            for (int i = 0; i < 200; i++)
            {
                long in_module_op_id = get_operation_state(module_id);

                //if (i > 1)
                //	writeln (module_id, ", ", i, ", op_id=", op_id, ", in_module_op_id=", in_module_op_id);

                if (in_module_op_id >= op_id || in_module_op_id == -1)
                    return 0;

                core.thread.Thread.sleep(10.msecs);
            }
        }
        else
        {
            //writeln("context: wait_thread: #1 ", text(module_id), " ", process_name);
            Tid tid = this.getTid(module_id);
            if (tid != Tid.init)
            {
                //writeln("context: wait_thread: #2 send ", text(module_id), " ", process_name);
                send(tid, CMD.NOP, thisTid);
                //                receiveTimeout(1000.msecs, (bool res) {});
                //writeln("context: wait_thread: #3 recv ", text(module_id), " ", process_name);
                receive((bool res) {});
            }
        }

        return 0;
        //writeln("context: wait_thread: #e ", text(module_id), " ", process_name);
    }

    public void set_trace(int idx, bool state)
    {
        writeln("set trace idx=", idx, ":", state);
        foreach (mid; is_traced_module.keys)
        {
            Tid tid = getTid(mid);
            if (tid != Tid.init)
                send(tid, CMD.SET_TRACE, idx, state);
        }

        veda.core.log_msg.set_trace(idx, state);
    }

    public bool backup(int level = 0)
    {
        if (level == 0)
            freeze();

        try
        {
            bool result = false;

            Tid  tid_subject_manager = getTid(P_MODULE.subject_manager);

            send(tid_subject_manager, CMD.BACKUP, "", thisTid);
            string backup_id;
            receive((string res) { backup_id = res; });

            if (backup_id != "")
            {
                result = true;

                string res;
                Tid    tid_acl_manager = getTid(P_MODULE.acl_manager);
                send(tid_acl_manager, CMD.BACKUP, backup_id, thisTid);
                receive((string _res) { res = _res; });
                if (res == "")
                    result = false;
                else
                {
                    Tid tid_ticket_manager = getTid(P_MODULE.ticket_manager);
                    send(tid_ticket_manager, CMD.BACKUP, backup_id, thisTid);
                    receive((string _res) { res = _res; });
                    if (res == "")
                        result = false;
                    else
                    {
                        Tid tid_fulltext_indexer = getTid(P_MODULE.fulltext_indexer);
                        send(tid_fulltext_indexer, CMD.BACKUP, backup_id, thisTid);
                        receive((string _res) { res = _res; });
                        if (res == "")
                            result = false;
                    }
                }
            }

            if (result == false)
            {
                if (level < 10)
                {
                    log.trace_log_and_console("BACKUP FAIL, repeat(%d) %s", level, backup_id);

                    core.thread.Thread.sleep(dur!("msecs")(500));
                    return backup(level + 1);
                }
                else
                    log.trace_log_and_console("BACKUP FAIL, %s", backup_id);
            }
            else
                log.trace_log_and_console("BACKUP Ok, %s", backup_id);

            return result;
        }
        finally
        {
            if (level == 0)
                unfreeze();
        }
    }

    public long count_individuals()
    {
        long count = 0;

        if (inividuals_storage !is null)
            count = inividuals_storage.count_entries();

        return count;
    }

    public void freeze()
    {
        writeln("FREEZE");
        Tid tid_subject_manager = getTid(P_MODULE.subject_manager);

        if (tid_subject_manager != Tid.init)
        {
            send(tid_subject_manager, CMD.FREEZE, thisTid);
            receive((bool _res) {});
        }
    }

    public void unfreeze()
    {
        writeln("UNFREEZE");
        Tid tid_subject_manager = getTid(P_MODULE.subject_manager);

        if (tid_subject_manager != Tid.init)
        {
            send(tid_subject_manager, CMD.UNFREEZE);
        }
    }

    string find(string uri)
    {
        string res;
        Tid    tid_subject_manager = getTid(P_MODULE.subject_manager);

        send(tid_subject_manager, CMD.FIND, uri, thisTid);
        receive((string key, string data, Tid tid)
                {
                    res = data;
                });
        return res;
    }
}
