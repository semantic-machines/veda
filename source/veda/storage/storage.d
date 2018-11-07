module veda.storage.storage;

import std.conv, std.datetime, std.stdio;
import veda.common.logger, veda.common.type, veda.storage.common;
import veda.onto.individual, veda.onto.resource;

public abstract class Storage
{
    string name;
    Logger log;

    /**
       Количество индивидуалов в базе данных
     */
    abstract long count_individuals();

    abstract KeyValueDB get_tickets_storage_r();
    abstract KeyValueDB get_inividuals_storage_r();

    public string get_binobj_from_individual_storage(string uri)
    {
        string res = get_inividuals_storage_r.get_binobj(uri);

        if (res !is null && res.length < 10)
            log.trace_log_and_console("ERR! get_individual_from_storage, found invalid BINOBJ, uri=%s", uri);

        return res;
    }

    public void get_obj_from_individual_storage(string uri, ref Individual indv)
    {
        get_inividuals_storage_r.get_individual(uri, indv);
    }

    public void reopen_ro_ticket_manager_db()
    {
        get_tickets_storage_r().reopen();
    }
}

string getNowAsString()
{
    SysTime sysTime = Clock.currTime();

    return sysTime.toISOExtString();
}
