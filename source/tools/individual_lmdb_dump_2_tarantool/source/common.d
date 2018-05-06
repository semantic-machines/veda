module veda.storage.common;

import std.conv, std.datetime, std.uuid;
import veda.common.logger, veda.common.type;

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
    public string find(OptAuthorize op_auth, string user_uri, string uri);

    public long get_last_op_id();
    public void open();
    public void reopen();
    public void close();
    public void flush(int force);

    public long count_entries();

    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id);
    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key);
}
