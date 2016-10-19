module veda.util.module_info;

import std.stdio, std.conv, std.utf, std.string, std.file, std.datetime, std.json, std.algorithm : remove;
import veda.core.common.define, util.logger;

public struct MInfo
{
    string name;
    long   op_id;
    long   committed_op_id;
    bool   is_Ok = false;
}

public enum OPEN_MODE : byte
{
    /// Выдача и проверка тикетов
    READER        = 1,
    WRITER        = 2,
    READER_WRITER = 3
}

class ModuleInfoFile
{
    private string    fn_module_info    = null;
    private File      *ff_module_info_w = null;
    private File      *ff_module_info_r = null;
    private string    module_name;
    private logger    log;
    private bool      is_writer_open = false;
    private bool      is_reader_open = false;
    private ubyte[]   buff;
    private OPEN_MODE mode;

    this(string _module_name, logger _log, OPEN_MODE _mode)
    {
        module_name    = _module_name;
        fn_module_info = module_info_path ~ "/" ~ module_name ~ "_info";
        log            = _log;
        mode           = _mode;
    }

    ~this()
    {
        ff_module_info_w.flush();
        ff_module_info_w.close();
    }

    private void open_writer()
    {
        if (mode != OPEN_MODE.WRITER && mode != OPEN_MODE.READER_WRITER)
            return;

        try
        {
            if (exists(fn_module_info) == false)
                ff_module_info_w = new File(fn_module_info, "w");
            else
                ff_module_info_w = new File(fn_module_info, "r+");
            is_writer_open = true;
        }
        catch (Throwable tr)
        {
            log.trace("ERR! ModuleInfoFile:open_reader, %s", tr.msg);
        }
    }

    private void open_reader()
    {
        if (mode != OPEN_MODE.READER && mode != OPEN_MODE.READER_WRITER)
            return;

        try
        {
            ff_module_info_r = new File(fn_module_info, "r");
            is_reader_open   = true;
        }
        catch (Throwable tr)
        {
            log.trace("ERR! ModuleInfoFile:open_reader, %s", tr.msg);
        }
    }

    bool put_info(long op_id, long committed_op_id)
    {
        if (is_writer_open == false)
        {
            open_writer();
            if (is_writer_open == false)
                return false;
        }

        try
        {
            ff_module_info_w.seek(0);
            ff_module_info_w.writefln("%s;%d;%d", module_name, op_id, committed_op_id);
            ff_module_info_w.flush();
            return true;
        }
        catch (Throwable tr)
        {
            log.trace("module:put_info [%s;%d;%d] %s", module_name, op_id, committed_op_id, tr.msg);
        }
        return false;
    }

    public MInfo get_info()
    {
        MInfo res;

        if (is_reader_open == false)
        {
            open_reader();
            if (is_reader_open == false)
                return res;
        }

//        log.trace("get_info #1 [%s]", module_name);

        res.is_Ok = false;

        try
        {
            ff_module_info_r.seek(0);

            if (buff is null)
                buff = new ubyte[ 4096 ];

            ubyte[] newbuff = ff_module_info_r.rawRead(buff);
            string  str     = cast(string)newbuff[ 0..$ ];
            if (str !is null)
            {
                if (str.length > 2)
                {
                    string[] ch = str[ 0..$ - 1 ].split(';');
                    //writeln("@ queue.get_info ch=", ch);
                    if (ch.length != 3)
                    {
                        return res;
                    }
                    res.name            = ch[ 0 ];
                    res.op_id           = to!long (ch[ 1 ]);
                    res.committed_op_id = to!long (ch[ 2 ]);
                    res.is_Ok           = true;
                }
                else
                    res.is_Ok = false;
            }
        }
        catch (Throwable tr)
        {
            log.trace("ERR! get_info[%s] fail, msg=%s", module_name, tr.msg);
        }

//        log.trace("get_info #e [%s], res(%s): name=%s, op_id=%d, committed_op_id=%d",
//          module_name, text(res.is_Ok), res.name, res.op_id, res.committed_op_id);

        return res;
    }
}