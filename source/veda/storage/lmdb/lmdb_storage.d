/**
 * lmdb реализация хранилища
 */
module veda.storage.lmdb.lmdb_storage;

import veda.common.type, veda.storage.common, veda.core.common.transaction;

public class LmdbStorage : Storage
{
    public long last_op_id()
    {
        return -1;
    }

    public OpResult put(OptAuthorize op_auth, immutable TransactionItem ti)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }


    public OpResult[] put(OptAuthorize op_auth, immutable(TransactionItem)[] items)
    {
        return [ OpResult(ResultCode.Not_Implemented, -1) ];
    }

    public OpResult remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }

    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        return null;
    }

    public string find_ticket(string ticket_id)
    {
        return null;
    }

    public ubyte authorize(string user_uri, string uri, bool trace)
    {
        return 0;
    }

    public void flush(int force)
    {
    }

    public void reopen()
    {
    }

    public void open()
    {
    }

    public void close()
    {
    }

    long count_entries()
    {
        return -1;
    }
}



