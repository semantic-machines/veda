/**
 * tarantool + az LMDB реализация хранилища
 */
module veda.storage.tarantool.tarantool_storage;

import veda.core.common.define, veda.common.logger, veda.util.properd;
import veda.common.type, veda.storage.common, veda.storage.storage;
import veda.storage.tarantool.tarantool_driver;

public class TarantoolStorage : Storage
{
    private KeyValueDB    tickets_storage_r;
    private KeyValueDB    inividuals_storage_r;

    this(string _name, Logger _log)
    {
        log  = _log;
        name = _name;
    }

    ~this()
    {
        log.trace_log_and_console("DESTROY OBJECT TarantoolStorage:[%s]", name);
    }

    override KeyValueDB get_tickets_storage_r()
    {
        if (tickets_storage_r is null)
            tickets_storage_r = new TarantoolDriver(log, "TICKETS", 513);

        return tickets_storage_r;
    }

    override KeyValueDB get_inividuals_storage_r()
    {
        if (inividuals_storage_r is null)
            inividuals_storage_r = new TarantoolDriver(log, "INDIVIDUALS", 512);

        return inividuals_storage_r;
    }

    override long count_individuals()
    {
        return get_inividuals_storage_r().count_entries();
    }
}



