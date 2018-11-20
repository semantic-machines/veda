/**
 * LMDBSRV client storage
 */
module veda.storage.lmdbsrv.lmdbsrv_r_storage;

import std.stdio, std.string, std.conv;
import kaleidic.nanomsg.nano;
import veda.core.common.define, veda.common.logger, veda.util.properd, veda.common.type, veda.storage.common, veda.storage.storage;
import veda.onto.individual;

static this()
{
}

public class LmdbSrvRStorage : Storage
{
    private string     srv_url;
    private KeyValueDB tickets_storage_r;
    private KeyValueDB inividuals_storage_r;

    this(string _name, string _srv_url, Logger _log)
    {
        log     = _log;
        name    = _name;
        srv_url = _srv_url;
    }

    ~this()
    {
        log.trace_log_and_console("DESTROY OBJECT LmdbClientStorage:[%s]", name);
    }

    override KeyValueDB get_tickets_storage_r()
    {
        if (tickets_storage_r is null)
            tickets_storage_r = new LmdbClient("tickets", srv_url, log);

        return tickets_storage_r;
    }

    override KeyValueDB get_inividuals_storage_r()
    {
        if (inividuals_storage_r is null)
            inividuals_storage_r = new LmdbClient("individuals", srv_url, log);

        return inividuals_storage_r;
    }

    override long count_individuals()
    {
        return get_inividuals_storage_r().count_entries();
    }
}

public class LmdbClient : KeyValueDB
{
    private Logger log;
    private string space_name;
    private string srv_url;
    private int    sock_srv = -1;

    this(string _space_name, string _srv_url, Logger _log)
    {
        if (_space_name == "tickets")
            space_name = "t";
        else if (_space_name == "individuals")
            space_name = "i";
        else
            space_name = "?";

        srv_url = _srv_url;
        log     = _log;
    }

    public string get_binobj(string uri)
    {
        return reqrep_binobj_2_srv(space_name ~ "," ~ uri);
    }

    public void get_individual(string uri, ref Individual individual)
    {
        string individual_as_binobj = reqrep_binobj_2_srv(space_name ~ "," ~ uri);

        if (individual_as_binobj is null || (individual_as_binobj.length == 1 && individual_as_binobj[ 0 ] == 0))
        {
            individual.setStatus(ResultCode.NotFound);
            return;
        }

        if (individual_as_binobj !is null && individual_as_binobj.length > 1)
        {
            if (individual.deserialize(individual_as_binobj) > 0)
                individual.setStatus(ResultCode.Ok);
            else
            {
                individual.setStatus(ResultCode.UnprocessableEntity);
                writeln("ERR! LmdbSrvRStorage: invalid binobj: [", individual_as_binobj, "] ", uri);
            }
        }
        else
        {
            individual.setStatus(ResultCode.UnprocessableEntity);
            //writeln ("ERR!: empty binobj: [", individual_as_binobj, "] ", uri);
        }
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

    private int get_sock_2_srv()
    {
        if (sock_srv >= 0)
            return sock_srv;

        sock_srv = nn_socket(AF_SP, NN_REQ);
        if (sock_srv < 0)
        {
            log.trace("ERR! LmdbSrvRStorage: cannot create socket");
            return -1;
        }
        else if (nn_connect(sock_srv, cast(char *)srv_url) < 0)
        {
            log.trace("ERR! LmdbSrvRStorage: cannot connect socket to %s", srv_url);
            return -1;
        }
        else
        {
            log.trace("LmdbSrvRStorage: success connect %s", srv_url);
            return sock_srv;
        }
    }

    private string reqrep_binobj_2_srv(string req)
    {
        string rep;
        int    res;

        try
        {
            int sock = get_sock_2_srv();

            if (sock >= 0)
            {
                char *buf = cast(char *)0;

                res = nn_send(sock, cast(char *)req, req.length, 0);
                //log.trace("TRACE! LmdbSrvRStorage: req (%s)", req);

                if (res < 0)
                {
                    log.trace("ERR! LmdbSrvRStorage: send: err=%s", fromStringz(nn_strerror(nn_errno())));
                    log.trace("ERR! LmdbSrvRStorage: send (%s)", req);
                }

                for (int attempt = 0; attempt < 10; attempt++)
                {
                    res = nn_recv(sock, &buf, NN_MSG, 0);

                    if (res < 0)
                    {
                        log.trace("ERR! LmdbSrvRStorage: recv: err=%s", fromStringz(nn_strerror(nn_errno())));
                    }

                    if (res > 0 || res == -1 && nn_errno() != 4)
                        break;

                    log.trace("ERR! LmdbSrvRStorage: repeat recv, attempt=%d", attempt + 1);
                }


                if (res >= 0)
                {
                    int bytes = res;

                    rep = to!string(buf);
                    //log.trace("TRACE! LmdbSrvRStorage: recv (%s)", rep);

                    nn_freemsg(buf);
                }
            }
            else
            {
                log.trace("ERR! LmdbSrvRStorage: invalid socket");
            }

            if (rep.length == 0)
                rep = null;

            return rep;
        }
        catch (Throwable tr)
        {
            log.trace("ERR! LmdbSrvRStorage: %s", tr.info);
            log.trace("req: %s", req);
            log.trace("rep: %s", rep);

            return null;
        }
    }
}

