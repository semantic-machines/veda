module veda.util.queue;

import std.conv, std.stdio, std.file, std.array, std.digest.crc, std.format, std.ascii, std.uuid, std.outbuffer;
import veda.common.logger;

enum QMessageType
{
    STRING = 'S',
    OBJECT = 'O'
}

enum Mode
{
    R       = 0,
    RW      = 1,
    DEFAULT = 2
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

enum QueueCode : int
{
    OK                 = 200,
    ConsumerIdNotEqual = 701
}

class Consumer
{
    public bool      isReady;
    public QueueCode status;
    public string    name;
    public uint      count_popped;

    private ubyte[]  buff;
    private ubyte[]  header_buff;
    private ubyte[ 1 ] buff1;
    private ubyte[ 4 ] buff4;
    private ubyte[ 8 ] buff8;
    private ubyte[ 4 ] crc;

    private Logger  log;

    private Queue   queue;
    private uint    id;
    private string  path;
    private ulong   start_pos_record;
    private ubyte[] last_read_msg;
    private Mode    mode;

    private File    *ff_info_pop_w = null;
    private File    *ff_info_pop_r = null;

    private string  file_name_info_pop;

    // tmp
    private Header header;
    private CRC32  hash;

    this(Queue _queue, string _path, string _name, Mode _mode, Logger _log)
    {
        queue = _queue;
        path  = _path;
        name  = _name;
        mode  = _mode;

        log         = _log;
        buff        = new ubyte[ 4096 * 100 ];
        header_buff = new ubyte[ header.length() ];
    }

    public uint get_id()
    {
        return id;
    }

    public string get_name()
    {
        return name;
    }

    public bool open(bool open_only_if_exists = false, Mode _mode = Mode.DEFAULT)
    {
        if (_mode != Mode.DEFAULT)
            mode = _mode;

        if (!queue.isReady)
        {
            isReady = false;
            return false;
        }

        file_name_info_pop = path ~ "/" ~ queue.name ~ "_info_pop_" ~ name;

        if (open_only_if_exists && !exists(file_name_info_pop))
        {
            return false;
        }

        if (mode == Mode.RW)
        {
            if (exists(file_name_info_pop) == false)
                ff_info_pop_w = new File(file_name_info_pop, "w");
            else
                ff_info_pop_w = new File(file_name_info_pop, "r+");
        }

        ff_info_pop_r = new File(file_name_info_pop, "r");

        isReady = get_info();

        return isReady;
    }

    public void close()
    {
        if (ff_info_pop_w !is null)
        {
            ff_info_pop_w.flush();
            ff_info_pop_w.close();
            ff_info_pop_w = null;
        }

        if (ff_info_pop_r !is null)
        {
            ff_info_pop_r.close();
            ff_info_pop_r = null;
        }
        isReady = false;
    }

    public void reopen()
    {
        close();
        open();
    }


    public void remove()
    {
        close();
        std.file.remove(file_name_info_pop);
    }

    private bool put_info(bool is_sync_data)
    {
        if (!queue.isReady || !isReady || mode == Mode.R)
            return false;

        id = queue.get_id();

        try
        {
            ff_info_pop_w.seek(0);
            ff_info_pop_w.writefln("%s;%s;%d;%d;%d", queue.name, name, start_pos_record, count_popped, id);

            if (is_sync_data)
                ff_info_pop_w.flush();
        }
        catch (Throwable tr)
        {
            log.trace("consumer:put_info [%s;%s;%d;%d;%d] %s", queue.name, name, start_pos_record, count_popped, id, tr.msg);
            return false;
        }
        return true;
    }

    public bool get_info()
    {
        if (!queue.isReady)
            return false;

        if (ff_info_pop_r is null)
            return false;

        ff_info_pop_r.seek(0);

        string str = ff_info_pop_r.readln();
        if (str !is null && str.length > 1)
        {
            if (isDigit(str[ $ - 1 ]) == false)
                str = str[ 0..$ - 1 ];

            string[] ch = str.split(';');
            if (ch.length != 5 && ch.length != 6)
            {
                isReady = false;
                return false;
            }

            string _name = ch[ 0 ];
            if (_name != queue.name)
            {
                log.trace("consumer:get_info:queue name from info [%s] != consumer.queue.name[%s]", _name, queue.name);
                isReady = false;
                return false;
            }

            _name = ch[ 1 ];
            if (_name != name)
            {
                log.trace("consumer:get_info:consumer name from info[%s] != consumer.name[%s]", _name, name);
                isReady = false;
                return false;
            }

            start_pos_record = to!ulong (ch[ 2 ]);
            count_popped     = to!uint (ch[ 3 ]);

            if (ch.length == 5 && ch[ 4 ].length > 0)
            {
                id = to!uint (ch[ 4 ]);

                //if (id != queue.id)
                //{
                //    log.trace("consumer:get_info:consumer.id [%d] != queue.id [%d]", id, queue.id);
                //    isReady = false;
                //    status  = QueueCode.ConsumerIdNotEqual;
                //    return false;
                //}
            }
        }

        //log.trace("get_info:%s", text(this));

        return true;
    }

    public string pop()
    {
        if (!queue.isReady)
        {
            log.trace("ERR! queue:pop: queue %s not ready", queue.name);
            return null;
        }

        if (!isReady)
        {
            log.trace("ERR! queue:pop: consumer %s not ready", name);
            return null;
        }

        if (mode == Mode.R)
        {
            log.trace("ERR! queue:pop: consumer %s reads only", name);
            return null;
        }

        if (queue.get_info_push(id) == false)
        {
            log.trace("ERR! queue:pop: queue %s not ready", queue.name);
            return null;
        }

        if (count_popped >= queue.count_pushed)
        {
            if (queue.id == id)
                queue.get_info_queue(true);

            if (queue.id > id)
            {
                log.trace("INFO: queue.id=%d, consumer.id=%d, set reader on next part %d", queue.id, id, id + 1);
                id       = id + 1;
                queue.id = id;
                if (queue.get_info_push(id) == false)
                {
                    log.trace("ERR! queue:pop: queue %s not ready", queue.name);
                    return null;
                }
                remove();

                count_popped     = 0;
                start_pos_record = 0;

                open();
                put_info(true);
            }

            return null;
        }

        File *ff_queue_r = queue.get_r_queue_file(id);
        ff_queue_r.seek(start_pos_record);

        ff_queue_r.rawRead(header_buff);
        header.from_buff(header_buff);

        if (header.start_pos != start_pos_record)
        {
            log.trace(
                      "ERR! queue:pop: queue.id: %d, invalid msg: header.start_pos[%d] != start_pos_record[%d] : %s, count_popped : %d, queue.count_pushed : %d",
                      queue.id, header.start_pos, start_pos_record, text(header), count_popped, queue.count_pushed);
            return null;
        }

        if (header.msg_length >= buff.length)
        {
            log.trace("INFO: queue:pop: inc buff size %d -> %d", buff.length, header.msg_length);
            buff = new ubyte[ header.msg_length + 1 ];
        }

        if (header.msg_length < buff.length)
        {
            last_read_msg = ff_queue_r.rawRead(buff[ 0..header.msg_length ]).dup;
            if (last_read_msg.length < header.msg_length)
            {
                log.trace("ERR! queue:pop:invalid msg: msg.length < header.msg_length : %s", text(header));
                return null;
            }
        }
        else
        {
            log.trace("ERR! queue:pop: invalid msg: header.msg_length[%d] < buff.length[%d] : %s", header.msg_length, buff.length, text(header));
            return null;
        }

        return cast(string)last_read_msg;
    }

    public void sync()
    {
        ff_info_pop_w.flush();
    }

    public bool commit_and_next(bool is_sync_data)
    {
        if (!queue.isReady || !isReady || mode == Mode.R)
        {
            log.trace("ERR! queue:commit_and_next:!queue.isReady || !isReady");
            return false;
        }

        queue.get_info_push(id);

        if (count_popped >= queue.count_pushed)
        {
            log.trace("ERR! queue[%s][%s]:commit_and_next:count_popped(%d) >= queue.count_pushed(%d)", queue.name, name, count_popped,
                      queue.count_pushed);
            return false;
        }

        header_buff[ header_buff.length - 4 ] = 0;
        header_buff[ header_buff.length - 3 ] = 0;
        header_buff[ header_buff.length - 2 ] = 0;
        header_buff[ header_buff.length - 1 ] = 0;

        hash.start();
        hash.put(header_buff);
        hash.put(last_read_msg);
        crc = hash.finish();

        if (header.crc[ 0 ] != crc[ 0 ] || header.crc[ 1 ] != crc[ 1 ] || header.crc[ 2 ] != crc[ 2 ] || header.crc[ 3 ] != crc[ 3 ])
        {
            log.trace("ERR! queue[%s][%s]:commit_and_next:invalid msg, fail crc[%s] : %s", queue.name, name, text(crc),
                      text(header));
            OutBuffer ob = new OutBuffer();
            hexdump(ob, last_read_msg);
            log.trace("%d\n%s", last_read_msg.length, ob);
            return false;
        }

        count_popped++;
        start_pos_record += header.length + header.msg_length;

        return put_info(is_sync_data);
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("consumer:" ~ name);
        sink(", queue:" ~ queue.name);
        sink(", start_pos_record=" ~ text(start_pos_record));
        sink(", count_popped=" ~ text(count_popped));
    }
}

class Queue
{
    bool            isReady;
    uint            count_pushed;

    private ubyte[] buff;
    private ubyte[] header_buff;
    private ubyte[ 1 ] buff1;
    private ubyte[ 4 ] buff4;
    private ubyte[ 8 ] buff8;
    private ubyte[ 4 ] crc;

    private Logger log;
    private string name;

    private uint   id;

    private string path;
    private ulong  right_edge;
    private Mode   mode;

    private File   *ff_queue_w      = null;
    private File   *ff_info_push_w  = null;
    private File   *ff_info_queue_w = null;

    // Read (request)
    private File   *ff_queue_r      = null;
    private File   *ff_info_push_r  = null;
    private File   *ff_info_queue_r = null;

    private File   *ff_lock_w = null;

    private string file_name_queue;
    private string file_name_info_push;
    private string file_name_info_queue;

    // tmp
    private Header header;
    private CRC32  hash;

    this(string _path, string _name, Mode _mode, Logger _log)
    {
        log                  = _log;
        mode                 = _mode;
        path                 = _path;
        name                 = _name;
        isReady              = false;
        buff                 = new ubyte[ 4096 * 100 ];
        header_buff          = new ubyte[ header.length() ];
        file_name_info_queue = path ~ "/" ~ name ~ "_info_queue";
    }

    ~this()
    {
        close();
    }

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("queue:" ~ name);
//      sink (", start_pos_record=" ~ text(start_pos_record));
        sink(", right_edge=" ~ text(right_edge));
        sink(", count_pushed=" ~ text(count_pushed));
//      sink (", count_popped=" ~ text(count_popped));
    }

    public string get_name()
    {
        return name;
    }

    public uint get_id()
    {
        return id;
    }

    public static bool is_lock(string path, string _queue_name)
    {
        return(exists(path ~ "/" ~ _queue_name ~ "_queue.lock"));
    }

    public void remove()
    {
        close();
        std.file.remove(file_name_info_push);
        std.file.remove(file_name_queue);
    }

    public bool open(Mode _mode = Mode.DEFAULT)
    {
        try
        {
            if (isReady == false)
            {
                if (_mode != Mode.DEFAULT)
                    mode = _mode;

                //writeln("open ", text (mode));
                string part_name;

                if (mode == Mode.RW)
                {
                    string file_name_lock = path ~ "/" ~ name ~ "_queue.lock";
                    if (exists(file_name_lock) == false)
                    {
                        ff_lock_w = new File(file_name_lock, "w");
                        ff_lock_w.write(text(id));
                    }
                    else
                    {
                        ff_lock_w = new File(file_name_lock, "r+");
                    }

                    if (ff_lock_w.tryLock(LockType.readWrite) == false)
                    {
                        isReady = false;
                        log.trace("ERR! queue %s already open", name);
                        return isReady;
                    }
                    ff_lock_w.flush();

                    if (exists(file_name_info_queue) == false)
                    {
                        ff_info_queue_w = new File(file_name_info_queue, "w");
                        put_info_queue(false);
                        ff_info_queue_w.flush();
                    }
                    else
                    {
                        ff_info_queue_w = new File(file_name_info_queue, "r+");
                        if (get_info_queue(false) == false)
                        {
                            log.trace("Queue [%s] fail read info queue", name);
                            return false;
                        }
                        log.trace("Queue [%s] current id = %d", name, id);

                        id           = id + 1;
                        count_pushed = 0;
                        right_edge   = 0;

                        put_info_queue(false);
                    }

                    part_name = name ~ "-" ~ text(id);
                    try
                    {
                        mkdir(path ~ "/" ~ part_name);
                    }
                    catch (Exception ex)
                    {
                    }

                    if (file_name_queue is null)
                    {
                        file_name_queue     = path ~ "/" ~ part_name ~ "/" ~ name ~ "_queue";
                        file_name_info_push = path ~ "/" ~ part_name ~ "/" ~ name ~ "_info_push";
                    }

                    if (exists(file_name_info_push) == false)
                        ff_info_push_w = new File(file_name_info_push, "w");
                    else
                        ff_info_push_w = new File(file_name_info_push, "r+");

                    if (exists(file_name_queue) == false)
                        ff_queue_w = new File(file_name_queue, "wb");
                    else
                        ff_queue_w = new File(file_name_queue, "ab+");

                    get_info_push(false);
                }

                if (mode == Mode.RW && ff_info_push_w !is null && ff_info_queue_w !is null && ff_queue_w !is null)
                    isReady = true;

                if (mode == Mode.R)
                {
//                        get_info_queue();

                    isReady = true;
                }
            }
        }
        catch (Throwable ex)
        {
            log.trace("ERR! queue, not open: ex: %s", ex.msg);
        }

        if (isReady == false)
            log.trace("ERR! queue %s, not open", name);

        return isReady;
    }

    private File *get_r_queue_file(int part_id)
    {
        if (id != part_id || ff_queue_r is null)
        {
            id = part_id;
            string part_name = name ~ "-" ~ text(id);
            file_name_queue = path ~ "/" ~ part_name ~ "/" ~ name ~ "_queue";

            ff_queue_r = new File(file_name_queue, "r");
        }

        return ff_queue_r;
    }

    public void close()
    {
        if (isReady == true)
        {
            //writeln("queue_close:", file_name_queue);

            if (ff_info_push_r !is null)
            {
                ff_info_push_r.close();
                ff_info_push_r = null;
            }

            if (ff_info_queue_r !is null)
            {
                ff_info_queue_r.close();
                ff_info_queue_r = null;
            }

            if (ff_queue_r !is null)
            {
                ff_queue_r.close();
                ff_queue_r = null;
            }

            if (mode == Mode.RW)
            {
                flush();

                ff_info_push_w.close();
                ff_info_push_w = null;

                ff_info_queue_w.close();
                ff_info_queue_w = null;

                ff_queue_w.close();
                ff_queue_w = null;
            }
            isReady = false;
        }
    }

    private void put_info_queue(bool is_check_ready = true)
    {
        if ((is_check_ready && !isReady) || mode == Mode.R)
            return;

        ff_info_queue_w.seek(0);

        auto writer = appender!string();
        formattedWrite(writer, "%s;%d;", name, id);

        hash.start();
        hash.put(cast(ubyte[])writer.data);
        string hash_hex = crcHexString(hash.finish());

        ff_info_queue_w.write(writer.data);
        ff_info_queue_w.writeln(hash_hex);
        ff_info_queue_w.flush();
    }

    public bool get_info_queue(bool is_check_ready = true)
    {
        if (is_check_ready && !isReady)
            return false;

        if (ff_info_queue_r is null)
        {
            try
            {
                ff_info_queue_r = new File(file_name_info_queue, "r");
            }
            catch (Throwable tr)
            {
                isReady = false;
                log.trace("ERR! queue:get_info: fail open file %s", path ~ "/" ~ name ~ "_info_queue");
                return false;
            }
        }

        ff_info_queue_r.seek(0);
        string str = ff_info_queue_r.readln();
        string hash_hex;

        if (str !is null)
        {
            string[] ch = str[ 0..$ - 1 ].split(';');
            if (ch.length != 3)
            {
                isReady = false;
                log.trace("ERR! queue:get_info_queue: invalid info record %s", str);
                return false;
            }

            if (ch[ 0 ] != name)
            {
                isReady = false;
                log.trace("ERR! queue:get_info_queue: request queue name %s not equal exist name %s", name, ch[ 0 ]);
                return false;
            }

            id = to!uint (ch[ 1 ]);
            string hash = ch[ 2 ];
        }

        return true;
    }

    private void put_info_push(bool is_check_ready = true)
    {
        if ((is_check_ready && !isReady) || mode == Mode.R)
            return;

        ff_info_push_w.seek(0);

        auto writer = appender!string();
        formattedWrite(writer, "%s;%d;%d;", name, right_edge, count_pushed);

        hash.start();
        hash.put(cast(ubyte[])writer.data);
        string hash_hex = crcHexString(hash.finish());

        ff_info_push_w.write(writer.data);
        ff_info_push_w.writeln(hash_hex);
    }

    public bool get_info_push(int part_id, bool is_check_ready = true)
    {
        if (is_check_ready && !isReady)
            return false;

        if (id != part_id || ff_info_push_r is null)
        {
            try
            {
                string part_name = name ~ "-" ~ text(part_id);
                file_name_info_push = path ~ "/" ~ part_name ~ "/" ~ name ~ "_info_push";
                ff_info_push_r      = new File(file_name_info_push, "r");
            }
            catch (Throwable tr)
            {
                isReady = false;
                log.trace("ERR! queue:get_info: fail open file %s", file_name_info_push);
                return false;
            }
        }

        ff_info_push_r.seek(0);
        string str = ff_info_push_r.readln();
        string hash_hex;

        if (str !is null)
        {
            string[] ch = str[ 0..$ - 1 ].split(';');
            if (ch.length != 4)
            {
                isReady = false;
                log.trace("ERR! queue:get_info: invalid info record %s", str);
                return false;
            }

            name = ch[ 0 ];

            if (ch[ 0 ] != name)
            {
                isReady = false;
                log.trace("ERR! queue:get_info: %s not equal %s", ch[ 0 ], name);
                return false;
            }
            name         = ch[ 0 ];
            right_edge   = to!ulong (ch[ 1 ]);
            count_pushed = to!uint (ch[ 2 ]);
            hash_hex     = ch[ 3 ];
        }

        return true;
    }

    private void flush()
    {
        if (mode == Mode.R)
            return;

        if (ff_queue_w !is null)
            ff_queue_w.flush();

        if (ff_info_push_w !is null)
            ff_info_push_w.flush();
    }

    private void put_msg(string msg, QMessageType type = QMessageType.STRING)
    {
        if (mode == Mode.R)
            return;

        if (msg.length == 0)
            return;

        ubyte[] _buff2 = cast(ubyte[])msg.dup;

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

        log.trace("@ %s %s, crc=%s", name, header, crc);

        right_edge += header_buff.length + _buff2.length;
    }

///////////////////////////////////////////////////////////////////////////

    public void push(string msg, bool is_flush = true, QMessageType type = QMessageType.STRING)
    {
        if (!isReady || mode == Mode.R)
        {
            log.trace("ERR! queue, no push into [%s], ready=%s, mode=%s", name, text(isReady), text(mode));
            return;
        }

        count_pushed++;
        put_msg(msg, type);
        put_info_push();

        if (is_flush)
            flush();
    }
}

unittest
{
    import std.datetime, std.uuid;
    import veda.util.tests_tools;
    import veda.onto.individual, veda.onto.resource;

    Logger log = new Logger("test", "log", "QUEUE");

    Queue  queue = new Queue("queue1" ~ randomUUID().toString(), Mode.RW, log);
    queue.open(Mode.RW);
    assert(queue.isReady);

    Consumer cs = new Consumer(queue, "consumer1", log);
    cs.open();

    assert(cs.isReady);

    Individual new_indv_A1 = generate_new_test_individual();
    string     binobj      = new_indv_A1.serialize();
    queue.push(binobj);

    Individual new_indv_A = generate_new_test_individual();
    binobj = new_indv_A.serialize();

    queue.push(binobj);
    queue.push(binobj);
    queue.push(binobj);

    string val = cs.pop();
    val = cs.pop();
    val = cs.pop();

    Individual indv_B;
    indv_B.deserialize(val);

    bool compare_res = new_indv_A.compare(indv_B);
    if (compare_res == false)
        writefln("new_indv_A [%s] != indv_B [%s]", new_indv_A, indv_B);

    assert(compare_res);

    val = cs.pop();

    Individual indv_B1;
    indv_B1.deserialize(val);

    compare_res = new_indv_A1.compare(indv_B1);
    if (compare_res == false)
        writefln("new_indv_A1 [%s] != indv_B [%s]", new_indv_A1, indv_B1);

    assert(compare_res);

    //queue.close();
    //cs.close();

    queue.remove();
    cs.remove();

    writeln("unittest [Queue] Ok");
}

private ushort ushort_from_buff(ubyte[] buff, int pos)
{
    ushort res = *((cast(ushort *)(buff.ptr + pos)));

    return res;
}

private uint uint_from_buff(ubyte[] buff, int pos)
{
    uint res = *((cast(uint *)(buff.ptr + pos)));

    return res;
}

private ulong ulong_from_buff(ubyte[] buff, int pos)
{
    ulong res = *((cast(ulong *)(buff.ptr + pos)));

    return res;
}

private void uint_to_buff(ubyte[] _buff, int pos, ulong data)
{
    _buff[ pos + 0 ] = (data & 0x000000FF);
    _buff[ pos + 1 ] = (data & 0x0000FF00) >> 8;
    _buff[ pos + 2 ] = (data & 0x00FF0000) >> 16;
    _buff[ pos + 3 ] = (data & 0xFF000000) >> 24;
}

private void ulong_to_buff(ubyte[] _buff, int pos, ulong data)
{
    _buff[ pos + 0 ] = (data & 0x00000000000000FF);
    _buff[ pos + 1 ] = (data & 0x000000000000FF00) >> 8;
    _buff[ pos + 2 ] = (data & 0x0000000000FF0000) >> 16;
    _buff[ pos + 3 ] = (data & 0x00000000FF000000) >> 24;
    _buff[ pos + 4 ] = (data & 0x000000FF00000000) >> 32;
    _buff[ pos + 5 ] = (data & 0x0000FF0000000000) >> 40;
    _buff[ pos + 6 ] = (data & 0x00FF000000000000) >> 48;
    _buff[ pos + 7 ] = (data & 0xFF00000000000000) >> 56;
}

void hexdump(T) (OutBuffer ob, T[] s)
{
    byte *b = cast(byte *)s.ptr;
    int  N  = cast(int)(s.length * T.sizeof);

    for (int i = 0; i < N; i++)
    {
        ob.writef("%2.2x ", b[ i ]);

        if ((i & 7) == 7)
            ob.writef(" ");

        if ((i & 31) == 31)
        {
            ob.writef("    ");
            for (int j = i - 31; j <= i; j++)
            {
                if (b[ j ] != 0)
                    ob.writef("%s", cast(char)b[ j ]);
                else
                    ob.write(".");
            }

            ob.writef("\n");
        }
    }
    ob.writefln("\n");
}
