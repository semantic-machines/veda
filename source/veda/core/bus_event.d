/**
 * обработка событий
 */
module veda.core.bus_event;

private import std.outbuffer, std.stdio, std.concurrency, std.datetime, std.conv;
import type;
private import util.container, util.logger, util.utils, veda.core.util.cbor8individual;
private import veda.core.know_predicates, veda.core.context, veda.core.define;
private import veda.onto.individual, veda.onto.resource;

logger log;

static this()
{
    log = new logger("bus_event", "log", "bus_event");
}

int count;
import vibe.data.json;
import vibe.core.log;
import vibe.http.client;
import vibe.stream.operations;
import veda.core.util.individual8json;

import backtrace.backtrace, Backtrace = backtrace.backtrace;
void bus_event_after(Ticket *ticket, Individual *individual, Resource[ string ] rdfType, string subject_as_cbor, string prev_state, EVENT ev_type, Context context,
                     string event_id)
{
    if (ticket is null)
    {
        printPrettyTrace(stderr);
    }
/*
    try
    {
        Json indv_json = individual_to_json(*individual);

        Json req_body = Json.emptyObject;
        if (ticket is null)
            req_body[ "ticket" ] = "null";
        else
            req_body[ "ticket" ] = ticket.id;
        req_body[ "individual" ] = indv_json;
        req_body[ "event_type" ] = text(ev_type);
        req_body[ "event_id" ]   = text(event_id);



        requestHTTP("http://127.0.0.1:8081/trigger",
                    (scope req) {
                        req.method = HTTPMethod.PUT;
                        req.writeJsonBody(req_body);
                    },
                    (scope res) {
                        logInfo("Response: %s", res.bodyReader.readAllUTF8());
                    }
                    );
    }
    catch (Exception ex)
    {
        writeln("EX!bus_event:", ex.msg);
    }
*/
    //writeln ("@bus_event B subject_as_cbor=[", individual.uri, "]");
    //writeln (rdfType);

    if (ev_type == EVENT.CREATE || ev_type == EVENT.UPDATE)
    {
        if (rdfType.anyExist(owl_tags) == true)
        {
            // изменения в онтологии, послать в interthread сигнал о необходимости перезагрузки (context) онтологии
            context.push_signal("onto", Clock.currStdTime() / 10000);
        }

        if (rdfType.anyExist(veda_schema__PermissionStatement) == true || rdfType.anyExist(veda_schema__Membership) == true)
        {
            Tid tid_acl = context.getTid(P_MODULE.acl_manager);
            if (tid_acl != Tid.init)
            {
                send(tid_acl, CMD.PUT, ev_type, subject_as_cbor);
            }
        }


        Tid tid_condition = context.getTid(P_MODULE.condition);
        if (tid_condition != Tid.init)
        {
            if (rdfType.anyExist(veda_schema__Event))
            {
                // изменения в v-s:Event, послать модуль Condition сигнал о перезагузке скрипта
                send(tid_condition, CMD.RELOAD, subject_as_cbor, thisTid);
                receive((bool){});
            }


            try
            {
                immutable(string)[] types;

                foreach (key; rdfType.keys)
                    types ~= key;

                string user_uri;

                if (ticket !is null)
                    user_uri = ticket.user_uri;
                send(tid_condition, user_uri, ev_type, subject_as_cbor, prev_state, types, individual.uri, event_id);
            }
            catch (Exception ex)
            {
                writeln("EX!bus_event:", ex.msg);
            }
        }
    }
    //writeln ("#bus_event E");
}

ResultCode trigger_script(Ticket *ticket, EVENT ev_type, Individual *individual, Individual *indv_prev_state, Context context, string event_id)
{
    Tid tid_condition = context.getTid(P_MODULE.condition);

    if (tid_condition != Tid.init)
    {
        Resource[ string ] rdfType;
        setMapResources(individual.resources.get(rdf__type, Resources.init), rdfType);

        string subject_as_cbor = individual2cbor(individual);
        string prev_state;	
        
        if (indv_prev_state !is null)
			prev_state = individual2cbor(indv_prev_state);

        if (rdfType.anyExist(veda_schema__Event))
        {
            // изменения в v-s:Event, послать модуль Condition сигнал о перезагузке скрипта
            send(tid_condition, CMD.RELOAD, subject_as_cbor, thisTid);
            receive((bool){});
        }

        try
        {
            immutable(string)[] types;

            foreach (key; rdfType.keys)
                types ~= key;

            string user_uri;

            if (ticket !is null)
                user_uri = ticket.user_uri;
            send(tid_condition, user_uri, ev_type, subject_as_cbor, prev_state, types, individual.uri, event_id);

            return ResultCode.OK;
        }
        catch (Exception ex)
        {
            writeln("EX!bus_event:", ex.msg);
            return ResultCode.Internal_Server_Error;
        }
    }

    return ResultCode.Not_Ready;
}

