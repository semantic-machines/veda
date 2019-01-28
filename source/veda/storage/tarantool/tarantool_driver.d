/**
 * реализация хранилища, используя немодифицированный tarantool
 */
module veda.storage.tarantool.tarantool_driver;

import core.thread, std.conv, std.stdio, std.string, std.conv, std.datetime;
import veda.bind.tarantool.tnt_stream, veda.bind.tarantool.tnt_net, veda.bind.tarantool.tnt_opt, veda.bind.tarantool.tnt_ping;
import veda.bind.tarantool.tnt_reply, veda.bind.tarantool.tnt_insert, veda.bind.tarantool.tnt_delete, veda.bind.tarantool.tnt_object,
       veda.bind.tarantool.tnt_select;
import veda.util.properd, veda.bind.msgpuck;
import veda.common.logger, veda.common.type;
import veda.onto.individual;
import veda.storage.common;

tnt_stream *tnt         = null;
bool       db_is_opened = false;

public class TarantoolDriver : KeyValueDB
{
    Logger log;
    string uri;
    string space_name;
    int    space_id;

    this(Logger _log, string _space_name, int _space_id)
    {
        log = _log;
        string[ string ] properties;
        properties = readProperties("./veda.properties");
        uri        = properties.as!(string)("tarantool_url") ~ "\0";

        space_name = _space_name;
        space_id   = _space_id;
    }

	public DBType get_type ()
	{
		return DBType.TARANTOOL;
	}

    public void get_individual(string uri, ref Individual individual)
    {
        string individual_as_binobj = get_binobj(uri);

        if (individual_as_binobj is null)
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
                writeln("ERR!: invalid binobj: [", individual_as_binobj, "] ", uri);
            }
        }
        else
        {
            individual.setStatus(ResultCode.UnprocessableEntity);
            //writeln ("ERR!: empty binobj: [", individual_as_binobj, "] ", uri);
        }
    }

    public string get_binobj(string uri)
    {
        if (uri is null || uri.length < 2)
            return null;

        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
                return null;
        }

        tnt_stream *tuple;
        tnt_reply_ *reply;

        try
        {
            reply = tnt_reply_init(null);
            tuple = tnt_object(null);

            tnt_object_add_array(tuple, 1);
            tnt_object_add_str(tuple, cast(const(char)*)(uri ~ "\0"), cast(uint)uri.length);

            tnt_select(tnt, space_id, 0, (2 ^ 32) - 1, 0, 0, tuple);
            tnt_flush(tnt);

            tnt.read_reply(tnt, reply);
            if (reply.code != 0)
            {
                log.trace("Select [%s] failed, errcode=%s msg=%s", uri, reply.code, to!string(reply.error));
                return null;
            }

            mp_type field_type = mp_typeof(*reply.data);
            if (field_type != mp_type.MP_ARRAY)
            {
                log.trace("VALUE CONTENT INVALID FORMAT [], KEY=%s, field_type=%s", uri, field_type);
                return null;
            }

            uint tuple_count = mp_decode_array(&reply.data);
            if (tuple_count == 0)
                return null;

            if (tuple_count != 1)
            {
                log.trace("VALUE CONTENT INVALID FORMAT, KEY=%s, tuple_count=%d", uri, tuple_count);
                return null;
            }

            field_type = mp_typeof(*reply.data);
            if (field_type != mp_type.MP_ARRAY)
            {
                log.trace("VALUE CONTENT INVALID FORMAT [[]], KEY=%s, field_type=%s", uri, field_type);
                return null;
            }

            auto   data_size = reply.data_end - reply.data;
            string res       = cast(string)reply.data[ 0..data_size ].dup;

            //stderr.writefln("@ TarantoolDriver.find: FOUND %s->[%s]", uri, cast(string)str_value[ 0..str_value_length ]);

            return res;
        }
        finally
        {
            if (reply !is null)
                tnt_reply_free(reply);

            if (tuple !is null)
                tnt_stream_free(tuple);
        }
    }

    public ResultCode store(string in_key, string in_value, long op_id)
    {
        tnt_reply_ *reply = null;
        tnt_stream *tuple = null;

        try
        {
            tuple = tnt_object(null);

            if (db_is_opened != true)
            {
                open();
                if (db_is_opened != true)
                    return ResultCode.ConnectError;
            }

            if (in_value.length < 3)
                return ResultCode.InternalServerError;

            tuple = tnt_object_as(tuple, cast(char *)in_value, in_value.length);

            tnt_replace(tnt, space_id, tuple);
            tnt_flush(tnt);

            reply = tnt_reply_init(null);
            tnt.read_reply(tnt, reply);
            if (reply.code != 0)
            {
                log.trace("Insert failed [%s][%s] errcode=%s msg=%s", in_key, in_value, reply.code, to!string(reply.error));
                return ResultCode.InternalServerError;
            }

            return ResultCode.Ok;
        }
        finally
        {
            if (reply !is null)
                tnt_reply_free(reply);

            if (tuple !is null)
                tnt_stream_free(tuple);
        }

    }

    public ResultCode remove(string in_key)
    {
        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
                return ResultCode.ConnectError;
        }

        tnt_reply_ *reply = null;
        tnt_stream *tuple = null;
        
        try
        {
            tuple = tnt_object(null);
            tnt_object_add_array(tuple, 1);

            tnt_object_add_str(tuple, cast(const(char)*)in_key, cast(uint)in_key.length);

            tnt_delete(tnt, space_id, 0, tuple);
            tnt_flush(tnt);

            reply = tnt_reply_init(null);
            tnt.read_reply(tnt, reply);
            if (reply.code != 0)
            {
                log.trace("Remove failed [%s] errcode=%s msg=%s", in_key, reply.code, to!string(reply.error));
                return ResultCode.InternalServerError;
            }

            return ResultCode.Ok;
        }
        finally
        {
            if (reply !is null)
                tnt_reply_free(reply);

            if (tuple !is null)
                tnt_stream_free(tuple);
        }
    }

    public void open()
    {
        tnt = tnt_net(null);

        tnt_set(tnt, tnt_opt_type.TNT_OPT_URI, uri.ptr);
        tnt_set(tnt, tnt_opt_type.TNT_OPT_SEND_BUF, 0);
        tnt_set(tnt, tnt_opt_type.TNT_OPT_RECV_BUF, 0);
        int res = tnt_connect(tnt);
        if (res == 0)
        {
            tnt_ping(tnt);
            tnt_reply_ *reply = tnt_reply_init(null);
            tnt.read_reply(tnt, reply);
            if (reply.code == 0)
            {
                tnt_stream *tuple = tnt_object(null);

                tnt_object_add_array(tuple, 1);
                tnt_object_add_str(tuple, "?", 1);

                tnt_select(tnt, space_id, 0, (2 ^ 32) - 1, 0, 0, tuple);
                tnt_flush(tnt);
                tnt_stream_free(tuple);

                tnt.read_reply(tnt, reply);
                if (reply.code == 36)
                {
                    tnt_reply_free(reply);
                    log.trace("ERR! SPACE %s NOT FOUND", space_name);
                    log.trace("SLEEP AND REPEAT");
                    core.thread.Thread.sleep(dur!("seconds")(1));
                    return open();
                }
                else
                    tnt_reply_free(reply);

                log.trace("SUCCESS CONNECT TO TARANTOOL %s", uri);
                db_is_opened = true;
            }
        }
        else
        {
            log.trace("FAIL CONNECT TO TARANTOOL %s err=%s", uri, to!string(tnt_strerror(tnt)));
            log.trace("SLEEP AND REPEAT");
            core.thread.Thread.sleep(dur!("seconds")(1));
            return open();
        }
    }

    public void reopen()
    {
    }

    public void close()
    {
    }

    public long count_entries()
    {
        return -1;
    }

    public void flush(int force)
    {
    }
}
