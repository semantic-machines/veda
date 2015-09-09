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

void bus_event_after(Ticket *ticket, Individual *individual, Resource[ string ] rdfType, string subject_as_cbor, EVENT ev_type, Context context,
                     string event_id)
{
    //writeln ("@bus_event B subject_as_cbor=[", individual.uri, "]");
    //writeln (rdfType);

    if (ev_type == EVENT.CREATE || ev_type == EVENT.UPDATE)
    {
    	Tid tid_fanout = context.getTid(P_MODULE.fanout);
        if (tid_fanout != Tid.init)
        {
        	send(tid_fanout, CMD.PUT, subject_as_cbor);
        }
    	
        Tid tid_condition = context.getTid(P_MODULE.condition);

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
                immutable(string)[] type;

                foreach (key; rdfType.keys)
                    type ~= key;

                string user_uri;

                if (ticket !is null)
                    user_uri = ticket.user_uri;
                send(tid_condition, user_uri, ev_type, subject_as_cbor, type, individual.uri, event_id);
            }
            catch (Exception ex)
            {
                writeln("EX!bus_event:", ex.msg);
            }
        }
    }
    //writeln ("#bus_event E");
}


