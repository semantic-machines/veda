module veda.storage.tarantool.tarantool_acl;

import veda.common.type, veda.core.common.define, veda.common.logger;
import veda.authorization.right_set, veda.storage.common, veda.authorization.authorization;
import veda.storage.tarantool.tarantool_driver;

class TarantoolAuthorization : ImplAuthorization
{
    TarantoolDriver driver;

    this(Logger _log)
    {
        log    = _log;
        driver = new TarantoolDriver(log, "acl-indexes", 513);
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

    override string get_in_current_transaction(string in_key, int level = 0)
    {
        return driver.find(OptAuthorize.NO, null, in_key);
    }

    override void abort_transaction()
    {
    }

    override bool begin_transaction(bool is_check_for_reload)
    {
        return true;
    }
}