/**
 * реализация хранилища, используя немодифицированный tarantool
 */
module veda.storage.tarantool.tarantool_driver;

import std.conv, std.stdio, std.string, std.conv, std.datetime;
import veda.util.properd, veda.bind.msgpuck;
import veda.common.logger, veda.common.type;
import veda.storage.common;
import veda.bind.tarantool.tnt_stream, veda.bind.tarantool.tnt_net, veda.bind.tarantool.tnt_opt, veda.bind.tarantool.tnt_ping,
       veda.bind.tarantool.tnt_reply;
import veda.bind.tarantool.tnt_insert, veda.bind.tarantool.tnt_object, veda.bind.tarantool.tnt_select;

public class TarantoolDriver : KeyValueDB
{
    Logger     log;
    string     uri;
    tnt_stream *tnt;
    bool       db_is_opened = false;
    string     space_name;
    int        space_id;

    this(Logger _log, string _space_name, int _space_id)
    {
        log = _log;
        string[ string ] properties;
        properties = readProperties("./veda.properties");
        uri        = properties.as!(string)("tarantool_url") ~ "\0";

        space_name = _space_name;
        space_id   = _space_id;
    }

    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true)
    {
        if (uri is null || uri.length < 2)
            return null;

        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
                return null;
        }

        tnt_reply_ reply;
        tnt_reply_init(&reply);

        try
        {
            tnt_stream *tuple = tnt_object(null);

            tnt_object_format(tuple, "[%s]", (uri ~ "\0").ptr);
            tnt_select(tnt, space_id, 0, (2 ^ 32) - 1, 0, 0, tuple);
            tnt_flush(tnt);
            tnt_stream_free(tuple);

            tnt.read_reply(tnt, &reply);
            if (reply.code != 0)
            {
                log.trace("Select [%s] failed", uri);
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
            {
                return null;
            }

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

            uint field_count = mp_decode_array(&reply.data);

            char *str_value;
            uint str_value_length;
            str_value = mp_decode_str(&reply.data, &str_value_length);
            str_value = mp_decode_str(&reply.data, &str_value_length);

            //stderr.writefln("@ TarantoolDriver.find: FOUND %s->[%s]", uri, cast(string)str_value[ 0..str_value_length ]);

            string res = cast(string)str_value[ 0..str_value_length ].dup;
            return res;
        }
        finally
        {
            tnt_reply_free(&reply);
        }
    }

    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id)
    {
        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
                return ResultCode.Connect_Error;
        }

        tnt_stream *tuple = tnt_object(null);
        tnt_object_add_array(tuple, 2);
        tnt_object_add_str(tuple, cast(const(char)*)in_key, cast(uint)in_key.length);
        tnt_object_add_str(tuple, cast(const(char)*)in_value, cast(uint)in_value.length);
        tnt_replace(tnt, space_id, tuple);
        tnt_flush(tnt);
        tnt_stream_free(tuple);

        tnt_reply_ reply;
        tnt_reply_init(&reply);
        tnt.read_reply(tnt, &reply);
        if (reply.code != 0)
        {
            log.trace("Insert failed [%s][%s] errcode=%s", in_key, in_value, reply.code);
            tnt_reply_free(&reply);
            return ResultCode.Internal_Server_Error;
        }

        tnt_reply_free(&reply);
        return ResultCode.OK;
    }

    public ResultCode remove(OptAuthorize op_auth, string user_uri, string in_key)
    {
        return ResultCode.Not_Implemented;
    }

    public long get_last_op_id()
    {
        return -1;
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
            tnt_reply_free(reply);
            if (reply.code == 0)
            {
                log.trace("SUCCESS CONNECT TO TARANTOOL %s", uri);
                db_is_opened = true;
            }
        }
        else
        {
            log.trace("FAIL CONNECT TO TARANTOOL %s", uri);
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
