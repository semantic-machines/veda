/**
 * обработка событий
 */
module veda.core.bus_event;

private import std.outbuffer, std.stdio, std.concurrency, std.datetime, std.conv, std.json;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.util.container, util.logger, util.utils, veda.core.util.cbor8individual, veda.core.util.individual8json, veda.core.util.cbor8json;
private import veda.type, veda.core.know_predicates, veda.core.context, veda.core.define;
private import veda.onto.individual, veda.onto.resource;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "event");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

int count;

void bus_event_after(Ticket *ticket, Individual *individual, ref MapResource rdfType, string new_state, string prev_state, EVENT ev_type,
                     Context context, string event_id, long op_id)
{
    if (ticket is null)
    {
        writeln("---TICKET IS NULL:bus_event_after");
        printPrettyTrace(stderr);
        writeln("^^^ TICKET IS NULL:bus_event_after");
    }

    if (ev_type == EVENT.CREATE || ev_type == EVENT.UPDATE)
    {
        if (external_js_vm_url !is null)
        {
            JSONValue req_body;
            try
            {
                JSONValue indv_json = individual_to_json(*individual);

                JSONValue prev_state_json;

                if (prev_state !is null)
                    cbor2json(&prev_state_json, prev_state);

                req_body[ "prev_state" ] = prev_state_json;

                if (ticket is null)
                    req_body[ "ticket" ] = "null";
                else
                    req_body[ "ticket" ] = ticket.id;
                req_body[ "event_type" ] = text(ev_type);
                req_body[ "event_id" ]   = text(event_id);
                req_body[ "individual" ] = indv_json;
                req_body[ "op_id" ]      = op_id;

                string url = external_js_vm_url ~ "/trigger";

                // writeln ("EXECUTE SCRIPT USE EXTERNAL: #1  uri=", individual.uri, ", url=", url, " *", process_name);
/*
                requestHTTP(url,
                            (scope req) {
                                req.method = HTTPMethod.PUT;
                                req.writeJsonBody(req_body);
                            },
                            (scope res) {
                                //logInfo("Response: %s", res.bodyReader.readAllUTF8());
                            }
                            );
 */
                //  writeln ("EXECUTE SCRIPT USE EXTERNAL: #E  uri=", individual.uri, ", url=", url, " *", process_name);
            }
            catch (Exception ex)
            {
                writeln("EX!bus_event:", ex.msg, ", url=", external_js_vm_url, ", req_body=", text(req_body));
            }
        }
        else
        {
            //log.trace("EXECUTE SCRIPT: event:%s uri:[%s]", ev_type, individual.uri);
            veda.core.glue_code.scripts.send_put(context, ticket, ev_type, new_state, prev_state, rdfType, individual, event_id, op_id);
        }
    }
}

ResultCode trigger_script(Ticket *ticket, EVENT ev_type, Individual *individual, Individual *indv_prev_state, Context context, string event_id,
                          long op_id)
{
    try
    {
        Tid tid_scripts = context.getTid(P_MODULE.scripts);

        if (tid_scripts != Tid.init)
        {
            MapResource rdfType;
            Resources   _types = individual.resources.get(rdf__type, Resources.init);
            setMapResources(_types, rdfType);

            string subject_as_cbor = individual2cbor(individual);
            string prev_state;

            if (indv_prev_state !is null)
                prev_state = individual2cbor(indv_prev_state);

            if (rdfType.anyExists(veda_schema__Event))
            {
                // изменения в v-s:Event, послать модуль Condition сигнал о перезагузке скрипта
                send(tid_scripts, CMD.RELOAD, subject_as_cbor, thisTid);
                receive((bool){});
            }

            immutable(string)[] types;

            foreach (key; rdfType.keys)
                types ~= key;

            string user_uri;

            if (ticket !is null)
                user_uri = ticket.user_uri;

            send(tid_scripts, user_uri, ev_type, subject_as_cbor, prev_state, types, individual.uri, event_id, op_id);

            return ResultCode.OK;
        }
        return ResultCode.Not_Ready;
    }
    catch (Exception ex)
    {
        log.trace("EX!bus_event:trigger_script:%s", ex.msg);
        return ResultCode.Internal_Server_Error;
    }
}

