/**
 * отображение в консоли количества выполненных операций
 */
module veda.mstorage.load_info;

private
{
    import core.thread, std.format, std.stdio, std.datetime, std.concurrency, std.datetime, std.conv, std.array : appender;
    import veda.core.util.utils;
    import veda.common.logger;
    import veda.common.type, veda.core.common.context, veda.core.common.define;
}

private string set_bar_color_1 = "\x1B[41m";
private string set_bar_color_2 = "\x1B[43m";
private string set_bar_color_3 = "\x1B[45m";
private string set_bar_color_4 = "\x1B[46m";
private string set_bar_color_5 = "\x1B[40m";

private string set_text_color_green       = "\x1B[32m";
private string set_text_color_blue        = "\x1B[34m";
private string set_all_attribute_off      = "\x1B[0m";
private string set_cursor_in_begin_string = "\x1B[0E";

Logger         log;

static this()
{
    log = new Logger("server-statistics", "log", "");
}

enum CMD : byte
{
    /// Сохранить
    PUT  = 1,

    /// Получить
    GET  = 2,

    EXIT = 49
}


public void stat(byte command_type, ref StopWatch sw) nothrow
{
    try
    {
        sw.stop();
        int t = cast(int)sw.peek().usecs;

        Tid statistic_data_accumulator_tid = getTid(P_MODULE.statistic_data_accumulator);

        if (statistic_data_accumulator_tid !is Tid.init)
        {
            send(statistic_data_accumulator_tid, CMD_PUT, CNAME.WORKED_TIME, t);

            if (command_type == CMD_GET)
                send(statistic_data_accumulator_tid, CMD_PUT, CNAME.COUNT_GET, 1);
            else
                send(statistic_data_accumulator_tid, CMD_PUT, CNAME.COUNT_PUT, 1);

            //if (trace_msg[ T_API_40 ] == 1)
            //   log.trace(func[ (func.lastIndexOf(".") + 1)..$ ] ~ ": t=%d µs", t);
        }
    }
    catch (Exception ex)
    {
    }
}

void statistic_data_accumulator(string thread_name)
{
    core.thread.Thread.getThis().name = thread_name;

    long[]                       stat = new long[ 3 ];
//    writeln("SPAWN: statistic_data_accumulator");

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    bool is_exit1 = false;
    bool is_exit;

    while (is_exit == false)
    {
        try
        {
            receive(
                    (byte cmd, CNAME idx, int delta)
                    {
                        if (cmd == CMD_PUT)
                        {
                            stat[ idx ] += delta;
                        }
                    },
                    (byte cmd, Tid tid_sender)
                    {
                        if (cmd == CMD_GET)
                        {
                            if (is_exit1 == true)
                            {
                                stat[ CNAME.COUNT_PUT ] = -1;
                                is_exit = true;
                            }

                            send(tid_sender, cast(immutable)stat.dup);
                        }
                        else if (cmd == CMD_EXIT)
                        {
                            is_exit1 = true;
                            writefln("[%s] recieve signal EXIT", "statistic_data_accumulator");
                            send(tid_sender, true);
                            thread_term();
                        }
                    },
                    (OwnerTerminated ot)
                    {
                        return;
                    },
                    (Variant v) { writeln(thread_name, "::Received some other type.", v); });
            //               (Variant v) { writeln(thread_name, "::Received some other type.", v); });
        }
        catch (Throwable tr)
        {
            writefln("ERR! msg=%s tr=%s", tr.msg, tr.info);
        }
    }
}

void print_statistic(string thread_name, Tid _statistic_data_accumulator)
{
    core.thread.Thread.getThis().name = thread_name;

    long                         sleep_time = 1;
//    Thread.sleep(dur!("seconds")(sleep_time));

    long prev_read_count  = 0;
    long prev_write_count = 0;
    long prev_worked_time = 0;

    // SEND ready
    receive((Tid tid_response_reciever)
            {
                send(tid_response_reciever, true);
            });

    bool is_exit = false;

    while (is_exit == false)
    {
        sleep_time = 1;

        send(_statistic_data_accumulator, CMD_GET, thisTid);
        const_long_array stat = receiveOnly!(const_long_array);

        if (stat[ CNAME.COUNT_PUT ] == -1)
        {
            is_exit = true;
            break;
        }

        long read_count  = stat[ CNAME.COUNT_GET ];
        long write_count = stat[ CNAME.COUNT_PUT ];
        long worked_time = stat[ CNAME.WORKED_TIME ];

        long delta_count_read  = read_count - prev_read_count;
        long delta_count_write = write_count - prev_write_count;

        prev_read_count  = read_count;
        prev_write_count = write_count;

        float p100 = 3000;

        if (delta_count_read > 0 || delta_count_write)
        {
            long delta_worked_time = worked_time - prev_worked_time;
            prev_worked_time = worked_time;

            char[] now = cast(char[]) getNowAsString();
            now[ 10 ]  = ' ';
            now.length = 19;

            float cps   = 0.1f;
            float cps_w = 0.1f;
            float cps_r = 0.1f;
            float wt    = cast(float)delta_worked_time;
            if (wt > 0)
            {
                cps   = ((cast(float)delta_count_write + cast(float)delta_count_read) / wt) * 1000 * 1000;
                cps_w = (cast(float)delta_count_write / wt) * 1000 * 1000;
                cps_r = (cast(float)delta_count_read / wt) * 1000 * 1000;
            }

            long ft_count_prep_put; // = veda.core.threads.xapian_indexer.get_count_prep_put();
            long ft_count_recv_put; // = veda.core.threads.xapian_indexer.get_count_recv_put();

            long sc_count_prep_put; // = veda.core.glue_code.scripts.get_count_prep_put();
            long sc_count_recv_put; // = veda.core.glue_code.scripts.get_count_recv_put();

            auto writer = appender!string();
            formattedWrite(writer, "%s|r/w :%7d/%5d|cps/thr:%9.1f|wt:%7d µs|tp r/w: %7d/%5d|t.w.t. : %7d ms|FTS:%7d/%7d/%7d|SCR:%7d/%7d/%7d",
                           now, read_count, write_count, cps, delta_worked_time, delta_count_read, delta_count_write, worked_time / 1000,
                           ft_count_recv_put, ft_count_prep_put, ft_count_recv_put - ft_count_prep_put, sc_count_recv_put, sc_count_prep_put,
                           sc_count_recv_put - sc_count_prep_put);

            log.trace("cps(r/w):%8.1f/%8.1f", cps_r, cps_w);

            string set_bar_color;

            if (cps < 3000)
            {
                p100          = 3000;
                set_bar_color = set_bar_color_1;
            }
            else if (cps >= 3000 && cps < 6000)
            {
                p100          = 6000;
                set_bar_color = set_bar_color_2;
            }
            else if (cps >= 6000 && cps < 10000)
            {
                p100          = 10000;
                set_bar_color = set_bar_color_3;
            }
            else if (cps >= 10000 && cps < 20000)
            {
                p100          = 20000;
                set_bar_color = set_bar_color_4;
            }
            else if (cps >= 20000)
            {
                p100          = 30000;
                set_bar_color = set_bar_color_5;
            }

            int d_cps_count = cast(int)((cast(float)writer.data.length / cast(float)p100) * cps + 1);

            if (d_cps_count > 0)
            {
                if (d_cps_count >= writer.data.length)
                    d_cps_count = cast(int)(writer.data.length - 1);

                writeln(set_bar_color, writer.data[ 0..d_cps_count ], set_all_attribute_off, writer.data[ d_cps_count..$ ]);
            }
        }

        Thread.sleep(dur!("seconds")(sleep_time));
    }

    writeln("exit form thread ", thread_name);
}
