/**
 * Data Change Signal thread
 */

module veda.core.threads.dcs_manager;

import core.thread, std.stdio, std.conv, std.concurrency, std.file, std.datetime, std.outbuffer, std.string;
import util.logger, veda.core.util.utils, veda.util.cbor, veda.util.cbor8individual, veda.util.queue;
import backtrace.backtrace, Backtrace = backtrace.backtrace;
import veda.type, veda.core.bind.lmdb_header, veda.core.common.context, veda.core.common.define, veda.core.log_msg, veda.onto.individual,
       veda.onto.resource, veda.util.tools;

version (VibeDefaultMain)
{
    import vibe.core.net : TCPConnection;
    import vibe.core.net : connectTCP;
}

string     node_id;
Context    context;
Individual node;

enum CMD : byte { EXAMINE, GET, CLOSE }

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
public void send_put(byte cmd, string user_uri, string cur_state, string prev_state, string event_id, long op_id)
{
    Tid tid_dcs = getTid(P_MODULE.dcs);

    if (tid_dcs != Tid.init)
    {
        send(tid_dcs, cast(CMD)cmd, user_uri, prev_state, cur_state, event_id, op_id);
    }
}

public bool examine_modules()
{
    bool all_ready = false;
    Tid  tid_dcs   = getTid(P_MODULE.dcs);

    if (tid_dcs != Tid.init)
    {
        send(tid_dcs, CMD.EXAMINE, thisTid);
        receive((bool _all_ready)
                {
                    all_ready = _all_ready;
                });
    }
    return all_ready;
}

long timeout = 1_000;

public void wait_module(P_MODULE pm, long op_id)
{
    //writeln ("@d dsc:wait_module");
    Tid  tid_dcs = getTid(P_MODULE.dcs);

    long wait_time;

    if (tid_dcs != Tid.init)
    {
        long op_id_from_module = 0;

        while (op_id > op_id_from_module)
        {
            send(tid_dcs, CMD.GET, pm, thisTid);
            receive((long _op_id)
                    {
                        op_id_from_module = _op_id;
                    });

            if (op_id_from_module >= op_id)
                break;

            core.thread.Thread.sleep(dur!("msecs")(100));
            wait_time += 100;

            if (wait_time > timeout)
            {
                writeln("WARN! timeout (wait opid=", op_id, ", opid from module = ", op_id_from_module, ") wait_module:", pm);
                break;
            }
        }
    }
}

public long get_opid(P_MODULE pm)
{
    Tid  tid_dcs           = getTid(P_MODULE.dcs);
    long op_id_from_module = 0;

    send(tid_dcs, CMD.GET, pm, thisTid);
    receive((long _op_id)
            {
                op_id_from_module = _op_id;
            });

    return op_id_from_module;
}

public void close()
{
    Tid  tid_dcs           = getTid(P_MODULE.dcs);

    send(tid_dcs, CMD.CLOSE, thisTid);
    receive((long _op_id)
            {
            });
}


Queue queue;
OutSignalChanel[ P_MODULE ]              osch_2_name;


shared static ~this()
{
	writeln ("dsc_manager: shared static ~this");
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
        log.trace("ERR! DCS dead (exit)");
    }

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    core.thread.Thread.sleep(dur!("msecs")(3000));
    
    core.thread.Thread.getThis().name = thread_name;
    osch_2_name[ P_MODULE.fanout ]  = new OutSignalChanel("fanout", "127.0.0.1", 8081);
    osch_2_name[ P_MODULE.scripts ] = new OutSignalChanel("scripts", "127.0.0.1", 8082);

    queue = new Queue("individuals-flow", Mode.RW);
    queue.remove_lock();
    queue.open();

    while (true)
    {
        try
        {
            receive(
                    (CMD cmd, Tid tid_response_reciever)
                    {
                        if (cmd == CMD.EXAMINE)
                        {
                        	writeln("examine modules");
                        	byte count_ready_module = 0;
                            foreach (name, osch; osch_2_name)
                            {
                                string res = osch.send_signal("get_opid");

                                if (res.length > 0)
                                {
                                    count_ready_module++;
                                }
                            }
                        	writeln("examine modules, count_ready_module=", count_ready_module, ", all=", osch_2_name.keys.length);
                        	send(tid_response_reciever, osch_2_name.keys.length == count_ready_module);
                        }else if (cmd == CMD.CLOSE)
                        {

                        	foreach (name, osch; osch_2_name)
                        	{
                            	osch.disconnect();
                        	}
                        	send(tid_response_reciever, 0);                        	
                        }
                    },
                    (CMD cmd, P_MODULE pm, Tid tid_response_reciever)
                    {
                        long op_id_from_module;
                        if (cmd == CMD.GET)
                        {
                            OutSignalChanel osch = osch_2_name.get(pm, null);
                            if (osch !is null)
                            {
                                osch.send_is_ready = true;
                                string res = osch.send_signal("get_opid");

                                if (res.length > 0)
                                {
                                    try
                                    {
                                        op_id_from_module = to!long (res[ 0 ]);
                                    }
                                    catch (Exception ex)
                                    {
                                        op_id_from_module = -1;
                                    }
                                }
                            }
                        }
                        send(tid_response_reciever, op_id_from_module);
                    },
                    (CMD cmd, string user_uri, string prev_state, string new_state, string event_id, long op_id)
                    {
                        Individual imm;
                        imm.uri = text(op_id);
                        imm.addResource("cmd", Resource(cmd));

                        if (user_uri !is null && user_uri.length > 0)
                            imm.addResource("user_uri", Resource(DataType.String, user_uri));

                        imm.addResource("new_state", Resource(DataType.String, new_state));

                        if (prev_state !is null && prev_state.length > 0)
                            imm.addResource("prev_state", Resource(DataType.String, prev_state));

                        if (event_id !is null && event_id.length > 0)
                            imm.addResource("event_id", Resource(DataType.String, event_id));

                        imm.addResource("op_id", Resource(op_id));

//writeln ("*imm=[", imm, "]");

                        string cbor = individual2cbor(&imm);
                        //writeln("*cbor.length=", cbor.length);

                        queue.push(cbor);

                        foreach (name, osch; osch_2_name)
                        {
                            osch.send_signal(text(queue.count_pushed));
                        }
                    },
                    (Variant v) { writeln(thread_name, "::dcs_thread::Received some other type.", v); });
        }
        catch (Throwable ex)
        {
            log.trace("ERR! LINE:[%s], FILE:[%s], MSG:[%s]", __LINE__, __FILE__, ex.msg);
        }
    }
}


class OutSignalChanel
{
    string name;
    string host;
    ushort port;
    bool   send_is_ready = true;
    long   last_fail_send;

    version (VibeDefaultMain)
    {
        import vibe.d;

        TCPConnection con = null;
    }

    this(string _name, string _host, ushort _port)
    {
        name = _name;
        host = _host;
        port = _port;
    }

	~this ()
	{
    	disconnect();
	}

    public void disconnect()
    {
    version (VibeDefaultMain)
    {
        con.flush();
        con.finalize();
        con.close();    	
            writeln("@ disconnect ", name, ", ", host, ":", port);
    }		
    }

    private void reconnect()
    {
        try
        {
            writeln("@ reconnect");

            if (con !is null && con.connected() == true)
            {
                writeln("@ reconnect: close socket #1");
                con.close();
                writeln("@ reconnect: close socket #2");
            }

            con = connectTCP(host, port);
            con.tcpNoDelay(true);
            con.keepAlive(true);

            send_is_ready = true;
        }
        catch (Exception ex)
        {
            writeln("ERR! DCS: reconnect [", name, "]: ", ex.msg);
            send_is_ready = false;
        }
    }

    ubyte[] buf = new ubyte[ 1024 ];


    bool is_send_recv_complete = true;

    string send_signal(string data)
    {
//        writeln("@ c10");
        string res;

        version (VibeDefaultMain)
        {
            if (con is null)
            {
                reconnect();
            }

            try
            {
                if (is_send_recv_complete == false)
                    reconnect();

                if (send_is_ready)
                {
                    is_send_recv_complete = false;
                    con.write(data);
                    con.flush();
                    con.read(buf[ 0..2 ]);
//                    writeln("b0=", buf[ 0 ], ", b1=", buf[ 1 ]);
                    int len = buf[ 0 ] + (buf[ 1 ] << 8);
//                    writeln("len = ", len);
                    con.read(buf[ 0..len ]);
                    res = (cast(string)buf[ 0..len ]);

                    //writeln("@recv=", res);

                    is_send_recv_complete = true;
                }
            }
            catch (Exception ex)
            {
                writeln("ERR! send to [", name, "]: ", ex.msg);
                send_is_ready = false;
            }
        }
        return res;
    }
}

