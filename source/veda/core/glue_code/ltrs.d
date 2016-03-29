/**
 * Long Time Run Scripts thread
 *
 *	START - подготавливает очередь и запускает исполнение скрипта над данными из очереди
 */

module veda.core.ltrs;

private import std.concurrency, std.stdio, std.conv, std.utf, std.string, std.file, std.datetime;
private import util.logger, veda.util.cbor, veda.core.util.cbor8individual;
private import veda.core.storage.lmdb_storage, veda.core.thread_context;
private import veda.type, veda.core.context, veda.core.define, veda.onto.resource, onto.lang, veda.onto.individual;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "LTRS");
    return _log;
}

Context    context;
string		node_id;

void ltrs_thread(string thread_name, string _node_id)
{
	    scope (exit)
    {
        log.trace("ERR! ltrs_thread dead (exit)");
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
            context = new PThreadContext(node_id, thread_name, P_MODULE.ltrs);
    }

    while (true)
    {
        try
        {
            receiveTimeout(msecs(0),
                    (CMD cmd, string new_state)
                    {
                        check_context();
                    	if (cmd == CMD.START)
                    	{
                            Individual indv;
                            if (cbor2individual(&indv, new_state) < 0)
                            	return;
                    		                    		
                    		veda.core.queue.Queue    queue = new veda.core.queue.Queue("queue-ltrs-" ~ indv.uri);
    						veda.core.queue.Consumer cs = new veda.core.queue.Consumer(queue, "consumer1");

                    		bool add_to_queue(string key, string value)
    						{    							
        							queue.push(value);
							        return true;
    						}
                    		
    						context.get_subject_storage_db.get_of_cursor(&add_to_queue);                    		
                    	}
                    },
                    (Variant v) { writeln(thread_name, "::ltrs_thread::Received some other type.", v); });
        }
        catch (Throwable ex)
        {
            log.trace("ltrs# ERR! LINE:[%s], FILE:[%s], MSG:[%s]", ex.line, ex.file, ex.msg);
        }
    }

}