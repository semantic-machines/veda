module veda.process.child_process;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime;
import std.socket    : InternetAddress, Socket, SocketException, SocketSet, TcpSocket;
import std.algorithm : remove;

private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools, veda.onto.onto;
import util.logger;
import core.stdc.stdlib, core.sys.posix.signal, core.sys.posix.unistd;

logger _log;

extern (C) void handleTermination(int _signal)
{
    f_listen_exit = true;

    _log.trace("!SYS: %s: caught signal: %s", process_name, text(_signal));
    writefln("!SYS: %s: caught signal: %s", process_name, text(_signal));
    _log.close();

    foreach (soc; reads)
    {
        writeln(process_name, ": close socket=", soc.localAddress, "- ", soc.remoteAddress);
        if (soc.isAlive())
        {
            soc.close();
        }
    }

    writeln("!SYS: ", process_name, ": exit");
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}

Socket[] reads;
bool     f_listen_exit = false;

class ChildProcess
{
    Context    context;
    Onto       onto;

    Individual node;

    ushort     port;
    string     host;
    string     queue_name = "individuals-flow";

    this(P_MODULE _module_name, string _host, ushort _port)
    {
        process_name = text(_module_name);
        port         = _port;
        host         = _host;
        _log         = new logger("veda-core-" ~ process_name, "log", "PROCESS");
        context      = new PThreadContext("cfg:standart_node", process_name, _module_name, "http://localhost:8080");
        Ticket sticket;
        sticket = *context.get_systicket_from_storage();
        set_global_systicket(sticket);

        if (node == Individual.init)
        {
            node = context.getConfiguration();
            configure();
        }
    }

    logger log()
    {
        if (_log is null)
            _log = new logger("veda-core-" ~ process_name, "log", process_name);
        return _log;
    }

    long     count_signal           = 0;
    long     count_readed           = 0;
    long     count_success_prepared = 0;
    long     op_id;
    Queue    queue;
    Consumer cs;

    void run()
    {
        ubyte[] buffer = new ubyte[ 1024 ];

        queue = new Queue(queue_name, Mode.R);
        queue.open();

        while (!queue.isReady)
        {
            log.trace("queue [%s] not ready, sleep and repeate...", queue_name);
            core.thread.Thread.sleep(dur!("seconds")(10));
            queue.open();
        }


        cs = new Consumer(queue, process_name);
        cs.open();

        char[ 1024 ] buf;

        if (count_signal == 0)
            prepare_queue();

        auto listener = new TcpSocket();
        listener.blocking = false;

        bool is_connected = false;

        int  count_attempt = 0;
        while (is_connected == false)
        {
            count_attempt++;
            try
            {
                listener.bind(new InternetAddress(host, port));
                listener.listen(10);
                log.trace("Listening on port %d.", port);
                is_connected = true;
            }
            catch (Throwable tr)
            {
                log.trace("fail bind ex=%s, sleep, and repeate (%d)", tr.msg, count_attempt);
                core.thread.Thread.sleep(dur!("seconds")(10));
                try
                {
                    listener.close();
                    listener = new TcpSocket();
                    listener.connect(new InternetAddress(host, port));
                    listener.close();
                }
                catch (Throwable tr1)
                {
                    log.trace("fail undind, ex=%s", tr1.msg);
                }
            }
        }

        enum MAX_CONNECTIONS = 60;
        // Room for listener.
        SocketSet socketSet = new SocketSet(MAX_CONNECTIONS + 1);
        ubyte[]   buff      = new ubyte[ 1024 ];

        while (true)
        {
            socketSet.add(listener);

            foreach (sock; reads)
                socketSet.add(sock);

            Socket.select(socketSet, null, null);

            if (f_listen_exit == true)
            {
                log.trace("stop listen port");
                return;
            }
            for (size_t i = 0; i < reads.length; i++)
            {
                if (reads[ i ] !is null && socketSet.isSet(reads[ i ]))
                {
                    {
                        auto datLength = reads[ i ].receive(buf[]);

                        if (datLength == Socket.ERROR)
                            log.trace("Connection error.");
                        else if (datLength != 0)
                        {
                            char[] msg = buf[ 0..datLength ];

                            string res;

                            if (msg == "get_opid")
                                res = text(op_id);
                            else
                                res = "Ok";

                            //writefln("Received %d bytes from %s: \"%s\", send %s", datLength, reads[ i ].remoteAddress().toString(), msg, res);

                            long len = (cast(ubyte[])res).length;

                            buff[ 0 ]          = cast(ubyte)(len & 0x00FF);
                            buff[ 1 ]          = cast(ubyte)((len & 0xFF00) >> 8);
                            buff[ 2..len + 2 ] = (cast(ubyte[])res);

                            reads[ i ].send(buff[ 0..len + 2 ]);

                            if (msg != "get_opid")
                                prepare_queue();

                            continue;
                        }
                        else
                        {
                            try
                            {
                                // if the connection closed due to an error, remoteAddress() could fail
                                log.trace("Connection from %s closed.", reads[ i ].remoteAddress().toString());
                            }
                            catch (SocketException)
                            {
                                log.trace("Connection closed.");
                            }
                        }
                    }

                    // release socket resources now
                    reads[ i ].close();

                    reads = reads.remove(i);
                    // i will be incremented by the for, we don't want it to be.
                    i--;

                    log.trace("Total connections: %d", reads.length);
                }
            }

            if (socketSet.isSet(listener))    // connection request
            {
                Socket sn = null;
                scope (failure)
                {
                    log.trace("Error accepting");

                    if (sn)
                        sn.close();
                }

                try
                {
                    sn = listener.accept();
                }
                catch (Exception ex)
                {
                    sn = null;
                    log.trace("ERR! ex=%s", ex.msg);
                }

//                assert(sn.isAlive);
//                assert(listener.isAlive);

                if (sn is null)
                    return;

                if (reads.length < MAX_CONNECTIONS)
                {
                    sn.setKeepAlive(1, 1);
                    log.trace("Connection from %s established.", sn.remoteAddress().toString());
                    reads ~= sn;
                    log.trace("Total connections: %d", reads.length);
                }
                else
                {
                    log.trace("Rejected connection from %s; too many connections.", sn.remoteAddress().toString());
                    sn.close();
//                    assert(!sn.isAlive);
//                    assert(listener.isAlive);
                }
            }

            socketSet.reset();
        }
    }

    private void prepare_queue()
    {
        queue.close();
        queue.open();
        while (true)
        {
            string data = cs.pop();

            if (data !is null)
            {
                count_readed++;

                Individual imm;

                if (data !is null && cbor2individual(&imm, data) < 0)
                {
                    log.trace("ERR! invalid individual:[%s]", data);
                    continue;
                }

                string  new_bin  = imm.getFirstLiteral("new_state");
                string  prev_bin = imm.getFirstLiteral("prev_state");
                string  user_uri = imm.getFirstLiteral("user_uri");
                string  event_id = imm.getFirstLiteral("event_id");
                INDV_OP cmd      = cast(INDV_OP)imm.getFirstInteger("cmd");
                op_id = imm.getFirstInteger("op_id");

                Individual prev_indv, new_indv;
                if (new_bin !is null && cbor2individual(&new_indv, new_bin) < 0)
                {
                    log.trace("ERR! invalid individual:[%s]", new_bin);
                }
                else
                {
                    //writeln ("@read from queue new_indv.uri=", new_indv.uri, ", op_id=", op_id);

                    if (prev_bin !is null && cbor2individual(&prev_indv, prev_bin) < 0)
                    {
                        log.trace("ERR! invalid individual:[%s]", prev_bin);
                    }
                }

                count_success_prepared++;

                //writeln ("%1 prev_bin=[", prev_bin, "], \nnew_bin=[", new_bin, "]");
                if (onto is null)
                    onto = context.get_onto();

                onto.update_class_in_hierarchy(new_indv, true);

                try
                {
                    bool res = prepare(cmd, user_uri, prev_bin, prev_indv, new_bin, new_indv, event_id, op_id);

                    if (res == true)
                        cs.commit();
                    else
                    {
                        writeln(process_name, ": message fail prepared, sleep and repeate...");
                        core.thread.Thread.sleep(dur!("seconds")(10));
                    }
                }
                catch (Throwable ex)
                {
                    log.trace("EX! ex=%s", ex.msg);
                }
            }
            else
                break;
        }
        if (count_readed != count_success_prepared)
            log.trace("WARN! : readed=%d, success_prepared=%d", count_readed, count_success_prepared);
    }





    abstract bool prepare(INDV_OP cmd, string user_uri, string prev_bin, ref Individual prev_indv, string new_bin, ref Individual new_indv,
                          string event_id,
                          long op_id);

    abstract void configure();
}