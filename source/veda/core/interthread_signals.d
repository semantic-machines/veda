/**
 * обмен сообщениями между нитями
 */
module veda.core.interthread_signals;

import core.thread, std.conv, std.concurrency, std.stdio, std.datetime;
version (linux) import core.stdc.stdlib;
private import type;
import veda.core.context;
import veda.core.define;

public void interthread_signals_thread(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;

    long[ string ] signals;
    string[ string ] str_signals;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });
    while (true)
    {
        receive(
                (CMD cmd, string key, DataType type, Tid tid_sender)
                {
                    if (cmd == CMD.GET && type == DataType.Integer)
                    {
                        long res;
                        res = signals.get(key, 0);
                        //writeln ("@get signal ", key);
                        send(tid_sender, res);
                    }
                    else if (cmd == CMD.GET && type == DataType.String)
                    {
                        string res;
                        res = str_signals.get(key, "");
                        //writeln ("@get signal ", key);
                        send(tid_sender, res);
                    }
                    else
                        send(tid_sender, "unknown command");
                },
                (CMD cmd, string key, long value)
                {
                    if (cmd == CMD.PUT)
                    {
                        signals[ key ] = value;
                        //writeln ("@set signal ", key, "=", value);
                    }
                },
                (CMD cmd, string key, string value)
                {
                    if (cmd == CMD.PUT)
                    {
                        str_signals[ key ] = value;
                        //writeln("@set signal ", key, "=", value);
                    }
                },
                (type.std.concurrency.OwnerTerminated ot)
                {
                    writeln(thread_name, ": OWNER THREAD TERMINATED, APOPTOSIS !");
                    system(cast(char *)("kill -kill " ~ text(getpid()) ~ "\0"));
                },
                (Variant v) { writeln(thread_name, "::interthread_signals_thread::Received some other type.", v); }
                );
    }
}
