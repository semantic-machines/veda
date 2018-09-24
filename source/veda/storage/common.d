module veda.storage.common;

import std.conv, std.datetime, std.uuid;
import veda.common.logger, veda.common.type;
import veda.onto.individual;

/// Режим работы хранилища
enum DBMode
{
    /// чтение
    R  = true,

    /// чтение/запись
    RW = false
}

public interface KeyValueDB
{
    public string get_binobj(string uri);
    public void get_individual(string uri, ref Individual indv);

    public long get_last_op_id();
    public void open();
    public void reopen();
    public void close();
    public void flush(int force);

    public long count_entries();

    public ResultCode store(string in_key, string in_value, long op_id);
    public ResultCode remove(string in_key);
}
