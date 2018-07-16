/**
 * tarantool + az LMDB реализация хранилища
 */
module veda.storage.tarantool.tarantool_storage;

import veda.core.common.define, veda.common.logger, veda.util.properd, veda.authorization.az_client;
import veda.common.type, veda.storage.common, veda.storage.storage;
import veda.storage.tarantool.tarantool_driver, veda.authorization.authorization;


public class TarantoolStorage : Storage
{
    private Authorization acl_client;
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
        acl_client.close();
    }

    override Authorization get_acl_client()
    {
        if (acl_client is null)
        {
            try
            {
                string[ string ] properties;
                properties = readProperties("./veda.properties");
                string acl_service = properties.as!(string)("acl_service_url");
                if (acl_service !is null)
                    acl_client = new ClientAuthorization(acl_service, this.log);
                else
                {
                    acl_client = new AuthorizationUseLib(this.log);
                }
            }
            catch (Throwable ex)
            {
                log.trace("ERR! unable read ./veda.properties");
            }
        }

        return acl_client;
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

    override public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        return null;
    }

    override long count_individuals()
    {
        return get_inividuals_storage_r().count_entries();
    }
}



