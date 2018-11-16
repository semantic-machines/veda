/**
 * LMDBSRV client storage
 */
module veda.storage.lmdbsrv.lmdbsrv_r_storage;

import veda.core.common.define, veda.common.logger, veda.util.properd;
import veda.common.type, veda.storage.common, veda.storage.storage;

static this()
{
}

public class LmdbSrvRStorage : Storage
{

    this(string _name, Logger _log)
    {
        log  = _log;
        name = _name;
    }

    ~this()
    {
        log.trace_log_and_console("DESTROY OBJECT LmdbClientStorage:[%s]", name);
    }

    override KeyValueDB get_tickets_storage_r()
    {
        if (tickets_storage_r is null)
            tickets_storage_r = new LmdbClient(name ~ ":tickets", log);

        return tickets_storage_r;
    }

    override KeyValueDB get_inividuals_storage_r()
    {
        if (inividuals_storage_r is null)
            inividuals_storage_r = new LmdbClient(name ~ ":inividuals", log);

        return inividuals_storage_r;
    }

    override long count_individuals()
    {
        return get_inividuals_storage_r().count_entries();
    }
}

public class LmdbClient : KeyValueDB
{

    public string get_binobj(string uri)
    {
	return null;
    }

    public void get_individual(string uri, ref Individual indv)
    {
    }

    public void open()
    {
    }

    public void reopen()
    {
    }

    public void close()
    {
    }

    public void flush(int force)
    {
    }

    public long count_entries()
    {
	return -1;
    }

    public ResultCode store(string in_key, string in_value, long op_id)
    {
	return ResultCode.NotImplemented;
    }

    public ResultCode remove(string in_key)
    {
	return ResultCode.NotImplemented;
    }
}

