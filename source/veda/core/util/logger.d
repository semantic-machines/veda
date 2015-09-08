/**
 * logger
 */
module util.logger;

// TODO: ++ module core.sys.posix.syslog;

private
{
    import std.format;
    import std.c.stdio;
    import std.datetime;

    import std.array : appender;

    import std.stdio;
    import std.datetime;
    import std.c.linux.linux;
    import std.concurrency;
}

/// Процесс отвечающий за логгирование
private void logger_process()
{
    //writeln("SPAWN: Logger");
    LoggerQueue llq = null;

    while (true)
    {
        // Receive a message from the owner thread.
        auto msg = receiveOnly!(char, string, string, string, string)();

        char cmd = msg[ 0 ];

        if (llq is null)
            llq = new LoggerQueue(msg[ 1 ], msg[ 2 ]);

        if (cmd == 'T')
            llq.trace(msg[ 4 ], msg[ 3 ]);
        else if (cmd == 'C')
            llq.trace_log_and_console(msg[ 4 ], msg[ 3 ]);
        else if (cmd == 'I')
            llq.trace_io(true, msg[ 4 ]);
        else if (cmd == 'O')
            llq.trace_io(false, msg[ 4 ]);
    }
}

/** класс logger

   Sample:

   ...

   logger log;

   static this()
   {
    log = new logger("pacahon", "log", "API");
   }

   ...
 */
public class logger
{
    private string log_name = "app";
    private string ext      = "log";
    private string src      = "";
    Tid            tid_logger;

    private void init_tid_logger()
    {
        if (tid_logger == Tid.init)
        {
            tid_logger = locate("logger");

            if (tid_logger == Tid.init)
            {
                tid_logger = spawn(&logger_process);
                register("logger", tid_logger);
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


    private FILE *ff = null;

    this(string log_name, string _ext)
    {
        trace_logfilename = log_name;
        ext               = _ext;
    }

    ~this()
    {
        fclose(ff);
    }

    private void open_new_file()
    {
        count = 0;
        _time tt     = time(null);
        tm    *ptm   = localtime(&tt);
        int   year   = ptm.tm_year + 1900;
        int   month  = ptm.tm_mon + 1;
        int   day    = ptm.tm_mday;
        int   hour   = ptm.tm_hour;
        int   minute = ptm.tm_min;
        int   second = ptm.tm_sec;

        auto  writer = appender!string();

        formattedWrite(writer, "%s_%04d-%02d-%02d_%02d:%02d:%02d.%s", trace_logfilename, year, month, day, hour, minute, second, ext);

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

        _time tt     = time(null);
        tm    *ptm   = localtime(&tt);
        int   year   = ptm.tm_year + 1900;
        int   month  = ptm.tm_mon + 1;
        int   day    = ptm.tm_mday;
        int   hour   = ptm.tm_hour;
        int   minute = ptm.tm_min;
        int   second = ptm.tm_sec;
        auto  now    = Clock.currTime();
        int   usecs  = now.fracSec.usecs;

        count++;

        if (ff is null || prev_time > 0 && day != prev_time || count > 1_000_000)
        {
            open_new_file();
        }

        auto writer = appender!string();

        formattedWrite(writer, "[%04d-%02d-%02d %02d:%02d:%02d.%06d]\n%s\n", year, month, day, hour, minute, second, usecs, str_io);

        fwrite(cast(char *)writer.data, 1, writer.data.length, ff);

        fwrite(cast(char *)data, 1, data.length, ff);

        fputc('\r', ff);

        fflush(ff);

        prev_time = day;
    }

    string trace(string arg, string src)
    {
        _time tt     = time(null);
        tm    *ptm   = localtime(&tt);
        int   year   = ptm.tm_year + 1900;
        int   month  = ptm.tm_mon + 1;
        int   day    = ptm.tm_mday;
        int   hour   = ptm.tm_hour;
        int   minute = ptm.tm_min;
        int   second = ptm.tm_sec;
        auto  now    = Clock.currTime();
        int   usecs  = now.fracSec.usecs;

        count++;
        if (ff is null || prev_time > 0 && day != prev_time || count > 1_000_000)
        {
            open_new_file();
        }

        //	       StopWatch sw1; sw1.start();
        auto writer = appender!string();

        if (src.length > 0)
            formattedWrite(writer, "[%04d-%02d-%02d %02d:%02d:%02d.%06d] [%s] ", year, month, day, hour, minute, second, usecs, src);
        else
            formattedWrite(writer, "[%04d-%02d-%02d %02d:%02d:%02d.%06d] ", year, month, day, hour, minute, second, usecs);

        writer.put(arg);
        writer.put(cast(char)0);

        fputs(cast(char *)writer.data, ff);
        fputc('\n', ff);

        //          sw1.stop();
        //               writeln (cast(long) sw1.peek().microseconds);

        fflush(ff);

        prev_time = day;

        return writer.data;
    }

    void trace_log_and_console(string arg, string src)
    {
        write(trace(arg, src), "\n");
    }
}
