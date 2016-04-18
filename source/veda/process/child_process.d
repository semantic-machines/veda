module veda.process.child_process;

private import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime;
private import backtrace.backtrace, Backtrace = backtrace.backtrace;
private import veda.type, veda.core.common.define, veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.util.queue;
private import util.logger, veda.util.cbor, veda.util.cbor8individual, veda.core.storage.lmdb_storage, veda.core.impl.thread_context;
private import veda.core.common.context, veda.util.tools;
import util.logger;

class ChildProcess
{
    Context    context;
    Individual node;

    string     process_name;
    string     tcp_point;
    string     queue_name = "individuals-flow";
    logger     _log;

    this(P_MODULE _module_name, string _tcp_point)
    {
        process_name = text(_module_name);
        tcp_point    = _tcp_point;
        _log         = new logger("veda-core-" ~ process_name, "log", "FANOUT");
        context      = new PThreadContext("cfg:standart_node", process_name, _module_name);
    }

    logger log()
    {
        if (_log is null)
            _log = new logger("veda-core-" ~ process_name, "log", process_name);
        return _log;
    }

    void run()
    {
        {
            import zmqd;

            // Socket to talk to clients
            auto responder = Socket(SocketType.rep);
            responder.bind(tcp_point);

            ubyte[] buffer = new ubyte[ 1024 ];

            Queue   queue = new Queue(queue_name, Mode.R);
            queue.open();

            while (!queue.isReady)
            {
                writeln(process_name, ": queue [", queue_name, "] not ready, sleep and repeate...");
                core.thread.Thread.sleep(dur!("seconds")(10));
                queue.open();
            }

            Consumer consumer;
            Consumer cs = new Consumer(queue, process_name);
            cs.open();
            long     count_readed           = 0;
            long     count_success_prepared = 0;

            while (true)
            {
                ulong count = responder.receive(buffer);
                responder.send("Ok");

                //writeln("Received signal ", count, cast(string)buffer[ 0..count ], ", readed=", count_readed, ", success_prepared=", count_success_prepared);

                queue.close();
                queue.open();
                while (true)
                {
                    string data = cs.pop();

                    if (data !is null)
                    {
                        count_readed++;
                        IndividualsModifyMessage imm = IndividualsModifyMessage(data);
                        cs.commit();

                        if (imm.is_ok)
                        {
                            Individual prev_indv, new_indv;
                            if (imm.new_state !is null && cbor2individual(&new_indv, imm.new_state) < 0)
                            {
                                log.trace("ERR! invalid individual:[%s]", imm.new_state);
                            }
                            else
                            {
                                if (imm.prev_state !is null && cbor2individual(&prev_indv, imm.prev_state) < 0)
                                {
                                    log.trace("ERR! invalid individual:[%s]", imm.prev_state);
                                }
                            }

                            count_success_prepared++;

                            if (node == Individual.init)
                            {
                                node = context.getConfiguration();
                                configure();
                            }

                            prepare(prev_indv, new_indv);
                        }
                    }
                    else
                        break;
                }

                writeln(process_name, ": readed=", count_readed, ", success_prepared=", count_success_prepared);
            }
        }
    }

    abstract void prepare(ref Individual prev_indv, ref Individual new_indv);

    abstract void configure();
}