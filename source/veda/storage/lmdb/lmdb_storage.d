/**
 * LMDB реализация хранилища
 */
module veda.storage.lmdb.lmdb_storage;

import veda.core.common.define, veda.common.logger;
import veda.common.type, veda.storage.common, veda.core.common.transaction, veda.storage.storage;
import veda.storage.lmdb.lmdb_driver;
import veda.storage.lmdb.lmdb_acl;

const string   individuals_db_path   = "./data/lmdb-individuals";
const string   tickets_db_path       = "./data/lmdb-tickets";

static this ()
{
	paths_list ~= individuals_db_path;
	paths_list ~= tickets_db_path;
}


public class LmdbStorage : Storage
{
    private Authorization acl_indexes;
    private KeyValueDB    tickets_storage_r;
    private KeyValueDB    inividuals_storage_r;

    this(string _name, Logger _log)
    {
        log  = _log;
        name = _name;
    }

    ~this()
    {
        log.trace_log_and_console("DESTROY OBJECT LmdbStorage:[%s]", name);
        tickets_storage_r.close();
        inividuals_storage_r.close();
        acl_indexes.close();
    }

    override Authorization get_acl_indexes()
    {
        if (acl_indexes is null)
            acl_indexes = new LmdbAuthorization(DBMode.R, name ~ ":acl", this.log);

        return acl_indexes;
    }

    override KeyValueDB get_tickets_storage_r()
    {
        if (tickets_storage_r is null)
            tickets_storage_r = new LmdbDriver(tickets_db_path, DBMode.R, name ~ ":tickets", log);

        return tickets_storage_r;
    }

    override KeyValueDB get_inividuals_storage_r()
    {
        if (inividuals_storage_r is null)
            inividuals_storage_r = new LmdbDriver(individuals_db_path, DBMode.R, name ~ ":inividuals", log);

        return inividuals_storage_r;
    }

    override public long last_op_id()
    {
        return -1;
    }

    override public OpResult put(OptAuthorize op_auth, immutable TransactionItem ti)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }

    override public OpResult[] put(OptAuthorize op_auth, immutable(TransactionItem)[] items)
    {
        return [ OpResult(ResultCode.Not_Implemented, -1) ];
    }

    override public OpResult remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        return OpResult(ResultCode.Not_Implemented, -1);
    }

    override public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        return null;
    }

    override public void flush(int force)
    {
    }

    override public void reopen()
    {
    }

    override public void open()
    {
    }

    override public void close()
    {
    }

    override long count_individuals()
    {
        return get_inividuals_storage_r().count_entries();
    }
}



