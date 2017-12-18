module veda.storage.tarantool.tarantool_acl;

import veda.common.type, veda.core.common.define, veda.common.logger;
import veda.storage.right_set, veda.storage.common, veda.storage.authorization;
import veda.storage.tarantool.tarantool_driver;

class TarantoolAuthorization : ImplAuthorization
{
    TarantoolDriver driver;

    this(Logger _log)
    {
        log    = _log;
        driver = new TarantoolDriver(log);
    }


    bool open()
    {
        if (driver.db_is_opened == false)
            driver.open();

        return driver.db_is_opened;
    }

    void reopen()
    {
        driver.reopen();
    }

    void close()
    {
        driver.close();
    }

    override string get_in_current_transaction(string in_key)
    {
        return null;
    }

    override void abort_transaction()
    {
    }

    override bool begin_transaction(bool is_check_for_reload)
    {
        return true;
    }
}