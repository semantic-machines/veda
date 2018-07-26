/**
 * Logger
 */
module veda.common.logger;

// TODO: ++ module core.sys.posix.syslog;

private
{
    import core.stdc.time, core.stdc.stdio, core.stdc.string, std.outbuffer;
    import std.format, std.datetime, std.array : appender;
    import std.stdio, std.datetime, std.concurrency, std_file = std.file;
}

/// Процесс отвечающий за логгирование
private void logger_process()
{
    //writeln("SPAWN: Logger");
    LoggerQueue[ string ] llq_2_filename;

    while (true)
    {
        try
        {
            // Receive a message from the owner thread.
            auto        msg = receiveOnly!(char, string, string, string, string)();

            char        cmd       = msg[ 0 ];
            string      file_name = msg[ 1 ];

            LoggerQueue llq = llq_2_filename.get(file_name, null);
            if (llq is null)
            {
                llq                         = new LoggerQueue(file_name, msg[ 2 ]);
                llq_2_filename[ file_name ] = llq;
            }

            if (cmd == 'T')
                llq.trace(msg[ 4 ], msg[ 3 ]);
            else if (cmd == 'C')
                llq.trace_log_and_console(msg[ 4 ], msg[ 3 ]);
            else if (cmd == 'I')
                llq.trace_io(true, msg[ 4 ]);
            else if (cmd == 'O')
                llq.trace_io(false, msg[ 4 ]);
            else if (cmd == 'X')
                return;
        }
        catch (OwnerTerminated ot)
        {
            break;
        }
        catch (Throwable tr)
        {
            writeln("ERR! logging, ex=", tr.msg);
            break;
        }
    }
}

public class ArrayLogger
{
    OutBuffer obuff;

    this()
    {
        obuff = new OutBuffer();
    }

    public void trace(Char, A ...) (in Char[] fmt, A args)
    {
        auto writer = appender!string();

        formattedWrite(writer, fmt, args);

        string     src = writer.data;

        SysTime    time = Clock.currTime();
        const auto dt   = cast(DateTime)time;
        const auto fsec = time.fracSecs.total !"usecs";

        if (src.length > 0)
            obuff.writefln("%04d-%02d-%02d %02d:%02d:%02d.%06d %s", dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
                           fsec, src);
        else
            obuff.writefln("%04d-%02d-%02d %02d:%02d:%02d.%06d", dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
                           fsec);
    }

    string raw()
    {
        return obuff.toString();
    }
}

/** класс Logger

   Sample:

   ...

   Logger log;

   static this()
   {
    log = new Logger("pacahon", "log", "API");
   }

   ...
 */
public class Logger
{
    private string log_name = "app";
    private string ext      = "log";
    private string src      = "";
    Tid            tid_logger;

    private void init_tid_logger()
    {
        if (tid_logger == Tid.init)
        {
            tid_logger = locate("Logger");

            if (tid_logger == Tid.init)
            {
                tid_logger = spawn(&logger_process);
                register("Logger", tid_logger);
            }
        }
    }

    /// Конструктор
    this(string _log_name, string _ext, string _src)
    {
        log_name = _log_name;
        src      = _src;
        ext      = _ext;
    }

    public void close()
    {
        init_tid_logger();
        send(tid_logger, 'X', log_name, ext, src, "");
    }

    /**
        Записать информацию в лог файл.
       Params:
                fmt = разметка сообщения
                args = выводимые переменные
     */
    public void trace(Char, A ...) (in Char[] fmt, A args)
    {
        init_tid_logger();
        auto writer = appender!string();
        formattedWrite(writer, fmt, args);
        send(tid_logger, 'T', log_name, ext, src, writer.data);
    }

    /**
        Записать информацию в лог файл и на консоль.
       Params:
                fmt = разметка сообщения
                args = выводимые переменные
     */
    public void trace_log_and_console(Char, A ...) (in Char[] fmt, A args)
    {
        init_tid_logger();
        auto writer = appender!string();
        formattedWrite(writer, fmt, args);
        send(tid_logger, 'C', log_name, ext, src, writer.data);
    }

    /**
        Записать информацию в лог файл и на консоль.
       Params:
                fmt = разметка сообщения
                args = выводимые переменные
     */
    public void tracec(Char, A ...) (in Char[] fmt, A args)
    {
        init_tid_logger();
        auto writer = appender!string();
        formattedWrite(writer, fmt, args);
        send(tid_logger, 'C', log_name, ext, src, writer.data);
    }

    /**
        Записать информацию в лог файл и на консоль.
       Params:
                fmt = разметка сообщения
                args = выводимые переменные
     */
    public void trace_console(Char, A ...) (in Char[] fmt, A args)
    {
        init_tid_logger();
        auto writer = appender!string();
        formattedWrite(writer, fmt, args);
        send(tid_logger, 'C', log_name, ext, src, writer.data);
    }

    /**
        Записать информацию о вводе/выводе в лог файл.
       Params:
                        io = указатель направления информации (I/O)
                data = указатель на данные
                length = длинна блока данных
     */
    public void trace_io(bool io, byte *data, ulong length)
    {
        init_tid_logger();
        if (io == true)
            send(tid_logger, 'I', log_name, ext, src, cast(immutable)(cast(char *)data)[ 0..length ]);
        else
            send(tid_logger, 'O', log_name, ext, src, cast(immutable)(cast(char *)data)[ 0..length ]);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////

alias long _time;


private class LoggerQueue
{
    private int    count     = 0;
    private int    prev_time = 0;

    private string trace_logfilename = "app";
    private string ext               = "log";

    private FILE   *ff = null;

    this(string log_name, string _ext)
    {
        trace_logfilename = log_name;
        ext               = _ext;

        try
        {
        	auto path = "logs";
            std_file.mkdir(path);
            writeln("create folder: ", path);
        }
        catch (Exception ex)
        {
        }

    }

    ~this()
    {
        fclose(ff);
    }

    private void open_new_file()
    {
        count = 0;

        SysTime    time = Clock.currTime();
        const auto dt   = cast(DateTime)time;
        const auto fsec = time.fracSecs.total !"usecs";

        auto       writer = appender!string();

        formattedWrite(writer, "logs/%s_%04d-%02d-%02d_%02d:%02d:%02d-%06d.%s", trace_logfilename, dt.year, dt.month, dt.day, dt.hour, dt.minute,
                       dt.second,
                       fsec,
                       ext);

        writer.put(cast(char)0);

        if (ff !is null)
        {
            fflush(ff);
            fclose(ff);
        }

        ff = fopen(writer.data.ptr, "aw");
    }

    void trace_io(bool io, string data)
    {
        if (data.length <= 0)
            return;

        string str_io;

        if (io == true)
            str_io = "INPUT";
        else
            str_io = "OUTPUT";

        SysTime    time = Clock.currTime();
        const auto dt   = cast(DateTime)time;
        const auto fsec = time.fracSecs.total !"usecs";

        count++;

        if (ff is null || prev_time > 0 && dt.day != prev_time || count > 1_000_000)
            open_new_file();

        auto writer = appender!string();

        formattedWrite(writer, "[%04d-%02d-%02d %02d:%02d:%02d.%06d]\n%s\n", dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
                       fsec, str_io);

        fwrite(cast(char *)writer.data, 1, writer.data.length, ff);
        fwrite(cast(char *)data, 1, data.length, ff);
        fputc('\r', ff);

        fflush(ff);

        prev_time = dt.day;
    }

    string trace(string arg, string src)
    {
        SysTime    time = Clock.currTime();
        const auto dt   = cast(DateTime)time;
        const auto fsec = time.fracSecs.total !"usecs";

        count++;
        if (ff is null || prev_time > 0 && dt.day != prev_time || count > 1_000_000)
            open_new_file();

        //	       StopWatch sw1; sw1.start();
        auto writer = appender!string();

        if (src.length > 0)
            formattedWrite(writer, "[%04d-%02d-%02d %02d:%02d:%02d.%06d] [%s] ", dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
                           fsec, src);
        else
            formattedWrite(writer, "[%04d-%02d-%02d %02d:%02d:%02d.%06d] ", dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
                           fsec);

        writer.put(arg);
        writer.put(cast(char)0);

        fputs(cast(char *)writer.data, ff);
        fputc('\n', ff);

        //          sw1.stop();
        //               writeln (cast(long) sw1.peek().microseconds);

        fflush(ff);

        prev_time = dt.day;

        return writer.data;
    }

    void trace_log_and_console(string arg, string src)
    {
        write(trace(arg, src), "\n");
    }
}
