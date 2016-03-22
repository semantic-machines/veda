module veda.core.queue;

import std.conv, std.stdio, std.file, std.array, std.digest.crc;
import veda.type, veda.core.know_predicates, veda.core.define, veda.core.context, veda.core.storage.lmdb_storage, veda.onto.onto;
import veda.onto.individual, veda.onto.resource, veda.util.tools;
import veda.core.util.cbor8individual;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "queue");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

enum QMessageType
{
    STRING = 'S',
    OBJECT = 'O'
}

struct Header
{
    ulong        start_pos;
    ulong        msg_length;
    uint         count_pushed;
    ubyte[ 4 ] crc;
    QMessageType type;

//

    void to_buff(ubyte[] buff)
    {
        int pos = 0;

        ulong_to_buff(buff, pos, start_pos);
        pos += ulong.sizeof;
        ulong_to_buff(buff, pos, msg_length);
        pos += ulong.sizeof;
        uint_to_buff(buff, pos, count_pushed);
        pos            += uint.sizeof;
        buff[ pos ]     = type;
        pos            += QMessageType.sizeof;
        buff[ pos + 0 ] = 0;
        buff[ pos + 1 ] = 0;
        buff[ pos + 2 ] = 0;
        buff[ pos + 3 ] = 0;

        //writeln ("write header:", this);
    }

    void from_buff(ubyte[] buff)
    {
        int pos = 0;

        start_pos    = ulong_from_buff(buff, pos);
        pos         += ulong.sizeof;
        msg_length   = ulong_from_buff(buff, pos);
        pos         += ulong.sizeof;
        count_pushed = uint_from_buff(buff, pos);
        pos         += uint.sizeof;
        type         = cast(QMessageType)buff[ pos ];
        pos         += QMessageType.sizeof;

        crc[ 0 ] = buff[ pos + 0 ];
        crc[ 1 ] = buff[ pos + 1 ];
        crc[ 2 ] = buff[ pos + 2 ];
        crc[ 3 ] = buff[ pos + 3 ];

        //writeln ("read header:", this);
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("header:");
        sink("  start_pos=" ~ text(start_pos));
        sink(", count_pushed=" ~ text(count_pushed));
        sink(", msg_length=" ~ text(msg_length));
        sink(", crc=" ~ text(crc[ 0 ]) ~ ", " ~  text(crc[ 1 ]) ~ ", " ~ text(crc[ 2 ]) ~ ", " ~ text(crc[ 3 ]));
    }

    int length()
    {
        return ulong.sizeof + ulong.sizeof + uint.sizeof + QMessageType.sizeof + crc.length;
    }
}

ubyte[] buff;
ubyte[] header_buff;
ubyte[ 1 ] buff1;
ubyte[ 4 ] buff4;
ubyte[ 8 ] buff8;
ubyte[ 4 ] crc;


class Consumer
{
    Queue  queue;
    string name;
    ulong  first_element;
    uint   count_popped;

    File   *ff_info_pop_w = null;
    File   *ff_info_pop_r = null;

    // tmp
    Header header;
    CRC32  hash;

    this(Queue _queue, string _name)
    {
        queue = _queue;
        name  = _name;

        string file_name_info_pop = queue_db_path ~ "/" ~ queue.name ~ "_info_pop_" ~ name;

        if (exists(file_name_info_pop) == false)
            ff_info_pop_w = new File(file_name_info_pop, "w");
        else
            ff_info_pop_w = new File(file_name_info_pop, "r+");

        ff_info_pop_r = new File(file_name_info_pop, "r");
    }

    private void put_info()
    {
        if (!queue.isReady)
            return;

        ff_info_pop_w.seek(0);
        ff_info_pop_w.writefln("%s;%d;%s;%d;%d", queue.name, queue.chunk, name, first_element, count_popped);
        ff_info_pop_w.flush();
    }

    private bool get_info()
    {
        if (!queue.isReady)
            return false;

        ff_info_pop_r.seek(0);
//        writeln("@2 ff_info_push_r.size=", ff_info_push_r.size);

        string str = ff_info_pop_r.readln();
        //writeln("@3 str=[", str, "]");
        if (str !is null)
        {
            string[] ch = str[ 0..$ - 1 ].split(';');
            //writeln("@ queue.get_info ch=", ch);
            if (ch.length != 6)
            {
                queue.isReady = false;
                return false;
            }

            name = ch[ 0 ];

            if (ch[ 0 ] != name)
            {
                queue.isReady = false;
                return false;
            }
            string _name = ch[ 0 ];
            if (name != queue.name)
            {
                writeln("consumer:get_info:queue name from info [", _name, "] != consumer.queue.name[", queue.name, "]");
                return false;
            }

            int _chunk = to!int (ch[ 1 ]);
            if (_chunk != queue.chunk)
            {
                writeln("consumer:get_info:queue chunk from info [", _chunk, "] != consumer.queue.chunk[", queue.chunk, "]");
                return false;
            }

            _name = ch[ 2 ];
            if (_name != name)
            {
                writeln("consumer:get_info:consumer name from info[", _name, "] != consumer.name[", name, "]");
                return false;
            }

            first_element = to!ulong (ch[ 3 ]);
            count_popped  = to!uint (ch[ 4 ]);
        }

        writeln(this);

        return true;
    }

    public string pop()
    {
        if (!queue.isReady)
            return null;

        if (count_popped >= queue.count_pushed)
            return null;

        queue.ff_queue_r.seek(first_element);

        queue.ff_queue_r.rawRead(header_buff);
        header.from_buff(header_buff);

        if (header.start_pos != first_element)
        {
            writeln("queue pop:invalid msg: header.start_pos[", header.start_pos, "] != first_element[", first_element, "] :", header);
            return null;
        }
//        writeln("@queue=", this);
//        writeln("@header=", header);

        ubyte[] _buff;

        if (header.msg_length < buff.length)
        {
            _buff = buff[ 0..header.msg_length ];
            _buff = queue.ff_queue_r.rawRead(_buff);
            if (_buff.length < header.msg_length)
            {
                writeln("queue:pop:invalid msg: msg.length < header.msg_length :", header);
                return null;
            }
        }
        else
        {
                writeln("queue:pop:invalid msg: header.msg_length[",header.msg_length,"] < buff.length[",buff.length,"] :", header);
                return null;        	
        }

        header_buff[ header_buff.length - 4 ] = 0;
        header_buff[ header_buff.length - 3 ] = 0;
        header_buff[ header_buff.length - 2 ] = 0;
        header_buff[ header_buff.length - 1 ] = 0;

        hash.start();
        hash.put(header_buff);
        hash.put(_buff);
        crc = hash.finish();

        if (header.crc[ 0 ] != crc[ 0 ] || header.crc[ 1 ] != crc[ 1 ] || header.crc[ 2 ] != crc[ 2 ] || header.crc[ 3 ] != crc[ 3 ])
        {
            writeln("queue:pop:invalid msg: fail crc[", crc, "] :", header);
            writeln (_buff.length);
            writeln (cast(string)_buff);
            return null;
        }

        count_popped++;
        first_element += header.length + header.msg_length;

        put_info();

        return cast(string)_buff;
//return null;
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("consumer:" ~ name);
        sink(", queue:" ~ queue.name);
        sink(", first_element=" ~ text(first_element));
        sink(", count_popped=" ~ text(count_popped));
    }
}

class Queue
{
    bool   isReady;
    string name;
    int    chunk;
//    ulong   first_element;
    ulong  right_edge;
    uint   count_pushed;
//    uint    count_popped;

    File *ff_info_push_w = null;
    File *ff_info_push_r = null;

    File *ff_queue_w = null;
    File *ff_queue_r = null;

    // tmp
    Header header;
    CRC32  hash;

    this(string _name)
    {
        name    = _name;
        isReady = false;
        open();
        get_info();
        put_info();

        buff        = new ubyte[ 4096*100 ];
        header_buff = new ubyte[ header.length() ];
    }

    ~this()
    {
        close();
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("queue:" ~ name);
//      sink (", first_element=" ~ text(first_element));
        sink(", right_edge=" ~ text(right_edge));
        sink(", count_pushed=" ~ text(count_pushed));
//      sink (", count_popped=" ~ text(count_popped));
    }

    public void open()
    {
        if (isReady == false)
        {
            string file_name_info_push = queue_db_path ~ "/" ~ name ~ "_info_push";
            string file_name_queue     = queue_db_path ~ "/" ~ name ~ "_queue_" ~ text(chunk);

            if (exists(file_name_info_push) == false)
                ff_info_push_w = new File(file_name_info_push, "w");
            else
                ff_info_push_w = new File(file_name_info_push, "r+");

            ff_info_push_r = new File(file_name_info_push, "r");

            if (exists(file_name_queue) == false)
                ff_queue_w = new File(file_name_queue, "wb");
            else
                ff_queue_w = new File(file_name_queue, "ab+");

            ff_queue_r = new File(file_name_queue, "r");

            if (ff_info_push_w !is null && ff_info_push_r !is null && ff_queue_w !is null && ff_queue_r !is null)
            {
                isReady = true;
                get_info();
                if (ff_queue_w.size() != right_edge)
                {
                    writeln("queue:open: [", file_name_queue, "].size (", ff_queue_w.size(), ") != right_edge=", right_edge);
                }
                else
                {
                    isReady = true;
                }
            }
        }
    }

    public void close()
    {
        if (isReady == true)
        {
            isReady = false;
            ff_info_push_w.close();
            ff_queue_w.close();
            ff_info_push_r.close();
            ff_queue_r.close();
        }
    }

    private void put_info()
    {
        if (!isReady)
            return;

        ff_info_push_w.seek(0);
        ff_info_push_w.writefln("%s;%d;%d;%d", name, chunk, right_edge, count_pushed);
    }

    private bool get_info()
    {
        if (!isReady)
            return false;

        ff_info_push_r.seek(0);
//        writeln("@2 ff_info_push_r.size=", ff_info_push_r.size);

        string str = ff_info_push_r.readln();
        //writeln("@3 str=[", str, "]");
        if (str !is null)
        {
            string[] ch = str[ 0..$ - 1 ].split(';');
            //writeln("@ queue.get_info ch=", ch);
            if (ch.length != 4)
            {
                isReady = false;
                return false;
            }

            name = ch[ 0 ];

            if (ch[ 0 ] != name)
            {
                isReady = false;
                return false;
            }
            name         = ch[ 0 ];
            chunk        = to!int (ch[ 1 ]);
            right_edge   = to!ulong (ch[ 2 ]);
            count_pushed = to!uint (ch[ 3 ]);
        }

        writeln(this);

        return true;
    }

    private void flush()
    {
        ff_queue_w.flush();
        ff_info_push_w.flush();
    }

    private void put_msg(string msg, QMessageType type = QMessageType.STRING)
    {
        ubyte[] _buff2 = cast(ubyte[])msg;

        header.start_pos    = right_edge;
        header.msg_length   = _buff2.length;
        header.count_pushed = count_pushed;
        header.type         = type;

        header.to_buff(header_buff);

        hash.start();
        hash.put(header_buff);
        hash.put(_buff2);
        crc = hash.finish();

        header_buff[ header_buff.length - 4 ] = crc[ 0 ];
        header_buff[ header_buff.length - 3 ] = crc[ 1 ];
        header_buff[ header_buff.length - 2 ] = crc[ 2 ];
        header_buff[ header_buff.length - 1 ] = crc[ 3 ];

        ff_queue_w.rawWrite(header_buff);
        ff_queue_w.rawWrite(_buff2);

        right_edge += header_buff.length + _buff2.length;
    }

///////////////////////////////////////////////////////////////////////////

    public void push(string msg, QMessageType type = QMessageType.STRING)
    {
        if (!isReady)
            return;

        count_pushed++;
        put_msg(msg, type);
        put_info();

        flush();
    }
}
