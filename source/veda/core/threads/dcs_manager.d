/**
 * Data Change Signal thread
 */

module veda.core.threads.dcs_manager;

import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
import util.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
import backtrace.backtrace, Backtrace = backtrace.backtrace;
import veda.type, veda.core.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.log_msg, veda.onto.individual,
       veda.onto.resource, veda.util.tools;

string     node_id;
Context    context;
Individual node;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "DCS");
    return _log;
}

// ////// ////// ///////////////////////////////////////////
public void send_put(Context ctx, CMD cmd, string user_uri, string cur_state, string prev_state, string event_id, long op_id)
{
    Tid tid_dcs = ctx.getTid(P_MODULE.dcs);

    if (tid_dcs != Tid.init)
    {
        send(tid_dcs, cmd, user_uri, prev_state, cur_state, event_id, op_id);
    }
}

Queue queue;

shared static ~this()
{
    if (queue !is null)
    {
        queue.close();
        queue = null;
    }
}

void dcs_thread(string thread_name, string _node_id)
{
    node_id = _node_id;
    scope (exit)
    {
        log.trace("ERR! dcs_thread dead (exit)");
    }

    core.thread.Thread.getThis().name = thread_name;

    OutSignalChanel              osch;
    osch = new OutSignalChanel("tcp://localhost:8081");

    queue = new Queue("individuals-flow", Mode.RW);
    queue.remove_lock();
    queue.open();

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    while (true)
    {
        try
        {
            receive(
                    (CMD cmd, string user_uri, string prev_state, string new_state, string event_id, long op_id)
                    {
    					Individual imm;
                    	imm.resources[ "cmd" ] ~= Resource(cmd);
                    	imm.resources[ "user_uri" ] ~= Resource(user_uri);
                    	imm.resources[ "new_state" ] ~= Resource(new_state);
                    	imm.resources[ "prev_state" ] ~= Resource(prev_state);
                    	imm.resources[ "event_id" ] ~= Resource(event_id);

						string cbor = individual2cbor(&imm);
						
                        queue.push(cbor);
                        osch.send_signal(text(queue.count_pushed));
                    },
                    (Variant v) { writeln(thread_name, "::dcs_thread::Received some other type.", v); });
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }
    }
}

version (VibeZMQ)
{
    import deimos.zmq.zmq;
    import dzmq;
}

class OutSignalChanel
{
    version (VibeZMQ)
    {
        string       point;
        ZmqContext   ctx;
        ZmqSocketReq sock;

        int          timeout       = 20;
        bool         send_is_ready = true;
        long         last_fail_send;
    }

    this(string _point)
    {
        version (VibeZMQ)
        {
            ctx  = new ZmqContext();
            sock = ctx.socket_req();

            point   = _point;
            timeout = 20;
            sock.ftc.set_options(ZMQ_RCVTIMEO, timeout);
            sock.ftc.set_options(ZMQ_SNDTIMEO, timeout);
            sock.ftc.connect(point);
            send_is_ready = true;
        }
    }

    version (VibeZMQ)
    {
        private void reconnect()
        {
            writeln("reconnect");
            sock.ftc.disconnect(point);
            sock.ftc.close();

            sock = ctx.socket_req();

            int timeout = 20;
            sock.ftc.set_options(ZMQ_RCVTIMEO, timeout);
            sock.ftc.set_options(ZMQ_SNDTIMEO, timeout);

            sock.ftc.connect(point);
        }
    }

    void send_signal(string data)
    {
/*
        version (VibeZMQ)
        {
            try
            {
                if (send_is_ready)
                {
                    sock.ftc.send([ data ]);
                    auto res = sock.ftc.receive();
                }
            }
            catch (Exception ex)
            {
                writeln("ERR! send to fanout: ", ex.msg);
                send_is_ready = false;
            }

            if (send_is_ready == false)
            {
                try
                {
                    long cur_fail_send = Clock.currTime.stdTime();
//                                  writeln ("#2 cur_fail_send - last_fail_send=", cur_fail_send - last_fail_send);

                    if (cur_fail_send - last_fail_send > 30_000_000)
                    {
                        last_fail_send = SysTime().stdTime();

                        reconnect();

                        last_fail_send = cur_fail_send;
                        send_is_ready  = true;
                    }
                }
                catch (Exception ex1)
                {
                    writeln("ERR! reconnect to fanout", ex1.msg);
                }
            }
        }
*/
    }

}

