/**
 * реализация хранилища, используя немодифицированный tarantool
 */
module veda.storage.tarantool.tarantool_driver;

import core.thread, std.conv, std.stdio, std.string, std.conv, std.datetime.stopwatch, std.uuid, std.variant;
import veda.bind.tarantool.tnt_stream, veda.bind.tarantool.tnt_net, veda.bind.tarantool.tnt_opt, veda.bind.tarantool.tnt_ping;
import veda.bind.tarantool.tnt_reply, veda.bind.tarantool.tnt_insert, veda.bind.tarantool.tnt_delete, veda.bind.tarantool.tnt_object,
       veda.bind.tarantool.tnt_select;
import veda.util.properd, veda.bind.msgpuck;
import veda.common.logger, veda.common.type, veda.onto.lang;
import veda.onto.individual, veda.onto.resource;
import msgpack;
import veda.storage.common;
import std.digest.ripemd, std.digest.md;

public enum TTFIELD : ubyte
{
    ID        = 0,
    SUBJECT   = 1,
    PREDICATE = 2,
    OBJECT    = 3,
    TYPE      = 4,
    LANG      = 5,
    ORDER     = 6
}

struct TripleRow
{
    long     id;
    string   subject;
    string   predicate;

    string   str_obj;
    bool     bool_obj;
    long     num_obj;

    DataType type;
    LANG     lang;
    int      order;
    bool     is_deleted;
    bool     is_tested;
}

tnt_stream *tnt         = null;
bool       db_is_opened = false;

public class TarantoolDriver : KeyValueDB
{
    Logger log;
    string db_uri;
    string space_name;
    int    space_id;

    this(Logger _log, string _space_name, int _space_id)
    {
        log = _log;
        string[ string ] properties;
        properties = readProperties("./veda.properties");
        db_uri     = properties.as!(string)("tarantool_url") ~ "\0";

        space_name = _space_name;
        space_id   = _space_id;
    }

    public static int INDEX_S = 1;

    public string get_binobj(string uri)
    {
        Individual indv;
        string     res = null;

        get_individual(uri, indv);

        if (indv.getStatus() == ResultCode.Ok)
        {
            res = indv.serialize();
            //log.trace("@ get_binobj, uri=%s, indv=%s", uri, indv);
        }
        return res;
    }

    private ResultCode reply_to_triple_row(tnt_reply_ *reply, ref TripleRow row, string uri)
    {
        mp_type field_type = mp_typeof(*reply.data);

        if (field_type != mp_type.MP_ARRAY)
        {
            log.trace("VALUE CONTENT INVALID FORMAT [[]], KEY=%s, field_type=%s", uri, field_type);
            return ResultCode.UnprocessableEntity;
        }

        int field_count = mp_decode_array(&reply.data);

        for (int fidx = 0; fidx < field_count; ++fidx)
        {
            long   num_value;
            string value;

            field_type = mp_typeof(*reply.data);
            if (field_type == mp_type.MP_BOOL)
            {
                bool obj = mp_decode_bool(&reply.data);
                if (fidx == TTFIELD.OBJECT)
                    row.bool_obj = obj;
            }
            else if (field_type == mp_type.MP_INT)
            {
                num_value = mp_decode_int(&reply.data);
                if (fidx == TTFIELD.OBJECT)
                    row.num_obj = num_value;
                else if (fidx == TTFIELD.ID)
                    row.id = num_value;
            }
            else if (field_type == mp_type.MP_UINT)
            {
                num_value = mp_decode_uint(&reply.data);
                //log.trace("fidx=%d,    num value=%d\n", fidx, num_value);
                if (fidx == TTFIELD.OBJECT)
                    row.num_obj = num_value;
                else if (fidx == TTFIELD.ORDER)
                    row.order = cast(int)num_value;
                else if (fidx == TTFIELD.ID)
                    row.id = num_value;
            }
            else if (field_type == mp_type.MP_STR)
            {
                char *str_value;
                uint str_value_length;
                str_value = mp_decode_str(&reply.data, &str_value_length);
                value     = cast(string)str_value[ 0..str_value_length ].dup;
                //log.trace("fidx=%d,    str value=%s\n", fidx, cast(string)str_value[ 0..str_value_length ]);

                if (fidx == cast(int)TTFIELD.OBJECT)
                    row.str_obj = value;
            }
            else
            {
                log.trace("wrong field type\n");
                //exit(1);
            }

            if (fidx == TTFIELD.SUBJECT && row.subject is null)
                row.subject = value;

            if (fidx == TTFIELD.PREDICATE && row.predicate is null)
                row.predicate = value;

            if (fidx == TTFIELD.TYPE)
                row.type = cast(DataType)num_value;

            if (fidx == TTFIELD.LANG)
                row.lang = cast(LANG)num_value;
        }

        return ResultCode.Ok;
    }

    public void get_individual(string uri, ref Individual indv)
    {
        indv.setStatus(ResultCode.UnprocessableEntity);

        if (uri is null || uri.length < 2)
        {
            indv.setStatus(ResultCode.BadRequest);
            return;
        }

        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
            {
                indv.setStatus(ResultCode.NotReady);
                return;
            }
        }

        //log.trace("@%X %s get individual uri=[%s], space_id=%s", tnt, core.thread.Thread.getThis().name(), uri, text(space_id));

        tnt_stream *tuple;
        tnt_reply_ *reply;

        try
        {
            reply = tnt_reply_init(null);
            tuple = tnt_object(null);

            tnt_object_add_array(tuple, 1);
            tnt_object_add_str(tuple, cast(const(char)*)(uri ~ "\0"), cast(uint)uri.length);

            tnt_select(tnt, space_id, INDEX_S, 1024, 0, 0, tuple);
            tnt_flush(tnt);

            tnt.read_reply(tnt, reply);

            //log.trace("@get individual @5 reply.code=[%d] uri=%s", reply.code, uri);
            if (reply.code != 0)
            {
                log.trace("Select [%s] failed, errcode=%s msg=%s", uri, reply.code, to!string(reply.error));
                if (reply.code == 36)
                    indv.setStatus(ResultCode.NotReady);
                else
                    indv.setStatus(ResultCode.UnprocessableEntity);
                return;
            }

            mp_type field_type = mp_typeof(*reply.data);
            if (field_type != mp_type.MP_ARRAY)
            {
                log.trace("VALUE CONTENT INVALID FORMAT [], KEY=%s, field_type=%s", uri, field_type);
                indv.setStatus(ResultCode.UnprocessableEntity);
                return;
            }

            uint tuple_count = mp_decode_array(&reply.data);
            if (tuple_count == 0)
            {
                //log.trace("ERR! not found ! request uri=[%s]", uri);
                indv.setStatus(ResultCode.NotFound);
                return;
            }
            //log.trace ("@get individual @8 tuple_count=%d", tuple_count);

            for (int irow = 0; irow < tuple_count; ++irow)
            {
                TripleRow  row;
                ResultCode rc = reply_to_triple_row(reply, row, uri);
                if (rc != ResultCode.Ok)
                {
                    indv.setStatus(rc);
                    return;
                }

                //log.trace("  field count=%d\n", field_count);

                if (uri != row.subject)
                {
                    log.trace("ERR! not found ?, request uri=%s, get uri=%s", uri, row.subject);
                    indv.setStatus(ResultCode.NotFound);
                    return;
                }

                if (indv.uri is null)
                    indv.uri = row.subject;

                if (row.type == DataType.Uri || row.type == DataType.String || row.type == DataType.Decimal)
                {
                    Resource rr = Resource(row.type, row.str_obj, row.lang);
                    rr.order = row.order;
                    indv.addResource(row.predicate, rr);
                }
                else if (row.type == DataType.Boolean)
                {
                    Resource rr = Resource(row.bool_obj);
                    rr.order = row.order;
                    indv.addResource(row.predicate, rr);
                }
                else
                {
                    Resource rr = Resource(row.type, row.num_obj);
                    rr.order = row.order;
                    indv.addResource(row.predicate, rr);
                }
            }

            foreach (predicate; indv.resources.keys)
            {
                indv.reorder(predicate);
            }

            indv.setStatus(ResultCode.Ok);
            //log.trace("driver:get:indv=%s", indv);

            //tnt_reply_free(&reply);
            //log.trace("@ TarantoolDriver.find: FOUND %s->[%s]", uri, cast(string)str_value[ 0..str_value_length ]);
        }
        finally
        {
            tnt_reply_free(reply);

            if (tuple !is null)
                tnt_stream_free(tuple);
        }
    }

    private void update_row(string subject, string predicate, Value object, DataType type, LANG lang, int order, ref tnt_stream *[] tuples,
                            decimal num, ref TripleRow[] prev_rows)
    {
        tnt_stream *tuple = tnt_object(null);

        tnt_object_add_array(tuple, 7);

        tnt_object_add_nil(tuple);

        tnt_object_add_str(tuple, cast(const(char)*)subject, cast(uint)subject.length);
        tnt_object_add_str(tuple, cast(const(char)*)predicate, cast(uint)predicate.length);

        string str_obj;
        long   num_obj;
        bool   bool_obj;

        if (type == DataType.Datetime || type == DataType.Integer)
        {
            if (object.type == Value.Type.unsigned)
                num_obj = object.via.uinteger;
            else
                num_obj = object.via.integer;

            tnt_object_add_int(tuple, num_obj);
        }
        else if (type == DataType.Uri || type == DataType.String)
        {
            if (object.type == Value.type.nil)
                str_obj = "";
            else
                str_obj = (cast(string)object.via.raw).dup;

            tnt_object_add_str(tuple, cast(const(char)*)str_obj, cast(uint)str_obj.length);
        }
        else if (type == DataType.Boolean)
        {
            bool_obj = object.via.boolean;
            tnt_object_add_bool(tuple, bool_obj);
        }
        else if (type == DataType.Decimal)
        {
            str_obj = num.asString();
            tnt_object_add_str(tuple, cast(const(char)*)str_obj, cast(uint)str_obj.length);
        }
        else
        {
            str_obj = "";
            tnt_object_add_str(tuple, cast(const(char)*)str_obj, cast(uint)str_obj.length);
            log.trace("ERR! update triple, unknown type %s", type);
        }

        tnt_object_add_int(tuple, type);
        tnt_object_add_int(tuple, lang);
        tnt_object_add_int(tuple, order);

        tnt_replace(tnt, space_id, tuple);

        tnt_flush(tnt);

        tnt_reply_ *reply = tnt_reply_init(null);
        tnt.read_reply(tnt, reply);
        if (reply.code != 0)
        {
            auto row = format("%s;%s;%s;%d;%d", subject, predicate, object, type, lang);
            log.trace("Insert failed errcode=%s msg=%s [%s]", reply.code, to!string(reply.error), row);
            tnt_reply_free(reply);
            tnt_stream_free(tuple);
            return;
        }

        tnt_reply_free(reply);
        tnt_stream_free(tuple);

        foreach (row; prev_rows)
        {
            if (row.is_deleted == false && row.is_tested == false)
            {
                //log.trace("#2 [subject=%s, row.subject=%s], [predicate=%s, row.predicate=%s], [type=%d, row.type=%d], [lang=%d, row.lang=%d]", subject, row.subject,
                //          predicate, row.predicate, type, row.type, lang,
                //          row.lang);
                if (subject == row.subject && predicate == row.predicate)
                {
                    if (type == row.type && lang == row.lang)
                    {
                        row.is_deleted = true;
                        if (type == DataType.Datetime || type == DataType.Integer)
                        {
                            if (num_obj == row.num_obj)
                                row.is_deleted = false;
                        }
                        else if (type == DataType.Uri || type == DataType.String || type == DataType.Decimal)
                        {
                            if (str_obj == row.str_obj)
                                row.is_deleted = false;
                        }
                        else if (type == DataType.Boolean)
                        {
                            //log.trace("#3 bool_obj=%s", text (bool_obj));
                            if (bool_obj == row.bool_obj)
                                row.is_deleted = false;
                        }
                    }
                    row.is_tested = true;
                }
            }

            //if (row.is_deleted == true)
            //    log.trace("row candidate for delete: %s", row);
        }
    }

    ubyte magic_header = 146;

    public ResultCode store(string in_key, string in_str, long op_id)
    {
        string subject;

        //log.trace("@%X %s store uri=%s", tnt, core.thread.Thread.getThis().name(), in_key);

        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
                return ResultCode.ConnectError;
        }

        if (in_str.length < 3)
            return ResultCode.InternalServerError;

        tnt_stream *[] tuples;
        ubyte[]        src = cast(ubyte[])in_str;

        if (src[ 0 ] != magic_header)
        {
            log.trace("ERR! msgpack2individual: invalid format");
            return ResultCode.InternalServerError;
        }

        if (src.length < 5)
        {
            log.trace("ERR! msgpack2individual: binobj is empty [%s]", src);
            return ResultCode.InternalServerError;
        }

        TripleRow[] prev_rows;
        get_individual_as_triple(in_key, prev_rows);
        if (prev_rows.length > 0)
            remove_triple_rows(prev_rows, in_key);

        try
        {
            try
            {
                StreamingUnpacker unpacker = StreamingUnpacker(src[ 0..$ ]);

                if (unpacker.execute())
                {
                    size_t root_el_size = unpacker.unpacked.length;
                    // writefln("TRY TO UNPACK root_el_size=%d", root_el_size);
                    if (root_el_size != 2)
                    {
                        log.trace("ERR! msgpack2individual: root_el_size != 2");
                        return ResultCode.InternalServerError;
                    }

                    foreach (obj; unpacker.purge())
                    {
                        switch (obj.type)
                        {
                        case Value.Type.raw:
                            subject = (cast(string)obj.via.raw).dup;

                            break;

                        case Value.Type.map:

                            Value[ Value ] map = obj.via.map;
                            foreach (key; map.byKey)
                            {
                                string predicate = (cast(string)key.via.raw).dup;

//                            Resources resources      = Resources.init;
                                Value[] resources_vals = map[ key ].via.array;
                                // writeln("\t\tTRY UNPACK RESOURCES len ", resources_vals.length);
                                for (int i = 0; i < resources_vals.length; i++)
                                {
                                    // writeln("\t\t\tTRY UNPACK RESOURCES type ", resources_vals[i].type);
                                    switch (resources_vals[ i ].type)
                                    {
                                    case Value.Type.array:
                                        Value[] arr = resources_vals[ i ].via.array;
                                        if (arr.length == 2)
                                        {
                                            long type = arr[ 0 ].via.uinteger;

                                            update_row(subject, predicate, arr[ 1 ], cast(DataType)type, LANG.NONE, i, tuples, decimal.init,
                                                       prev_rows);
                                        }
                                        else if (arr.length == 3)
                                        {
                                            long type = arr[ 0 ].via.uinteger;

                                            if (type == DataType.Decimal)
                                            {
                                                long mantissa, exponent;

                                                if (arr[ 1 ].type == Value.Type.unsigned)
                                                    mantissa = arr[ 1 ].via.uinteger;
                                                else
                                                    mantissa = arr[ 1 ].via.integer;

                                                if (arr[ 2 ].type == Value.Type.unsigned)
                                                    exponent = arr[ 2 ].via.uinteger;
                                                else
                                                    exponent = arr[ 2 ].via.integer;

                                                update_row(subject, predicate, Value.init, cast(DataType)type, LANG.NONE,
                                                           i, tuples, decimal(mantissa, cast(byte)exponent), prev_rows);
                                            }
                                            else if (type == DataType.String)
                                            {
                                                long lang = arr[ 2 ].via.uinteger;
                                                update_row(subject, predicate, arr[ 1 ], cast(DataType)type, cast(LANG)lang,
                                                           i, tuples, decimal.init, prev_rows);
                                            }
                                            else
                                            {
                                                log.trace("ERR! msgpack2individual: [0][1][3] unknown type [%d]", type);
                                                return ResultCode.InternalServerError;
                                            }
                                        }
                                        break;

                                    default:
                                        log.trace("ERR! msgpack2individual: unknown type [%d]", resources_vals[ i ].type);
                                        break;
                                    }
                                }
                                //individual.resources[ predicate ] = resources;
                            }
                            break;

                        default:
                            break;
                        }
                    }
                }
                else
                {
                    log.trace("ERR! msgpack2individual: binobj is invalid! src=[%s]", in_str);
                    return ResultCode.InternalServerError;
                }

                if (tnt_flush(tnt) < 0)
                {
                    log.trace("Insert failed network error [%s][%s]", in_key, in_str);
                    return ResultCode.InternalServerError;
                }
/*
                                tnt_reply_ *reply = tnt_reply_init(null);
                tnt.read_reply(tnt, &reply);
                if (reply.code != 0)
                {
                    log.trace("Insert failed errcode=%s msg=%s [%s][%s]", reply.code, to!string(reply.error), in_key, in_str);
                    tnt_reply_free(&reply);
                    return ResultCode.Internal_Server_Error;
                }

                tnt_reply_free(&reply);
 */
                //log.trace ("@%X END, store uri=%s", tnt, in_key);

                //tnt_flush (tnt);
                //reopen ();

                return ResultCode.Ok;
                // return cast(int)(ptr - cast(char *)in_str.ptr); //read_element(individual, cast(ubyte[])in_str, dummy);
            }
            catch (Throwable ex)
            {
                log.trace("ERR! msgpack2individual ex=", ex.msg, ", in_str=", in_str);
                //throw new Exception("invalid binobj");
                return ResultCode.InternalServerError;
            }
        } finally
        {
//                foreach (tuple; tuples)
//                {
//					log.trace ("@ free tuple");
//
//                    tnt_stream_free(tuple);
//				}
//            log.trace ("@d msgpack2individual @E");
        }
    }

    private void get_individual_as_triple(string in_key, ref TripleRow[] res)
    {
        tnt_stream *tuple;
        tnt_reply_ *reply;

        try
        {
            reply = tnt_reply_init(null);

            tuple = tnt_object(null);

            tnt_object_add_array(tuple, 1);
            tnt_object_add_str(tuple, cast(const(char)*)in_key, cast(uint)in_key.length);

            tnt_select(tnt, space_id, INDEX_S, 1024, 0, 0, tuple);
            tnt_flush(tnt);

            tnt.read_reply(tnt, reply);

            //log.trace("@remove individual @5 reply.code=[%d]", reply.code);
            if (reply.code != 0)
            {
                log.trace("Select [%s] failed, errcode=%s msg=%s", in_key, reply.code, to!string(reply.error));
                return;
            }

            mp_type field_type = mp_typeof(*reply.data);
            if (field_type != mp_type.MP_ARRAY)
            {
                log.trace("VALUE CONTENT INVALID FORMAT [], KEY=%s, field_type=%s", in_key, field_type);

                return;
            }

            uint tuple_count = mp_decode_array(&reply.data);
            if (tuple_count == 0)
            {
                //log.trace("ERR! remove individual, not found ! request uri=%s", in_key);
                return;
            }
            //log.trace("@remove individual, @8 tuple_count=%d", tuple_count);

            string subject;

            for (int irow = 0; irow < tuple_count; ++irow)
            {
                TripleRow  row;
                ResultCode rc = reply_to_triple_row(reply, row, in_key);
                if (rc == ResultCode.Ok)
                    res ~= row;
            }
            return;
        }
        finally
        {
            tnt_reply_free(reply);

            if (tuple !is null)
                tnt_stream_free(tuple);
        }
    }

    private long remove_triple_rows(TripleRow[] drows, string in_key)
    {
        //log.trace("deleted_rows=%s, length=%d", drows, drows.length);

        long       count_deleted = 0;

        tnt_stream *tuple;

        foreach (row; drows)
        {
            //log.trace("row=%s", row);

            tuple = tnt_object(null);
            tnt_object_add_array(tuple, 1);

            tnt_object_add_int(tuple, row.id);

            tnt_delete(tnt, space_id, 0, tuple);
            tnt_flush(tnt);
            tnt_stream_free(tuple);

            tnt_reply_ *reply = tnt_reply_init(null);
            tnt.read_reply(tnt, reply);
            if (reply.code != 0)
            {
                log.trace("Remove failed [%s] id=[%s], errcode=%s msg=%s", in_key, row.id, reply.code, to!string(reply.error));
            }
            else
            {
                count_deleted++;
                //log.trace("Remove Ok, key=[%s] id=[%s]", in_key, row.id);
            }

            tnt_reply_free(reply);
        }

        return count_deleted;
    }

    public ResultCode remove(string in_key)
    {
        if (db_is_opened != true)
        {
            open();
            if (db_is_opened != true)
                return ResultCode.ConnectError;
        }

        //log.trace("@%X %s remove individual uri=%s", tnt, core.thread.Thread.getThis().name(), in_key);

        TripleRow[] deleted_rows;
        get_individual_as_triple(in_key, deleted_rows);
        long        need_count_deleted = deleted_rows.length;
        if (deleted_rows.length > 0)
        {
            long count_deleted = remove_triple_rows(deleted_rows, in_key);
            if (count_deleted != need_count_deleted)
            {
                log.trace("ERR! fail delete rows from [%s], count_deleted(%d) < %d", in_key, count_deleted, need_count_deleted);
            }
        }

        return ResultCode.Ok;
    }

    public long get_last_op_id()
    {
        return -1;
    }

    public void open()
    {
        if (db_is_opened == false)
        {
            tnt = tnt_net(null);

            tnt_set(tnt, tnt_opt_type.TNT_OPT_URI, db_uri.ptr);
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
                    reply = tnt_reply_init(null);
                    // CHECK EXISTS SPACE

                    tnt_stream *tuple = tnt_object(null);

                    tnt_object_add_array(tuple, 1);
                    tnt_object_add_int(tuple, 1);

                    tnt_select(tnt, space_id, 0, 1, 0, 0, tuple);
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


                    log.trace("SUCCESS CONNECT TO TARANTOOL %s", db_uri);
                    db_is_opened = true;
                }
            }
            else
            {
                log.trace("FAIL CONNECT TO TARANTOOL %s err=%s", db_uri, to!string(tnt_strerror(tnt)));
                log.trace("SLEEP AND REPEAT");
                core.thread.Thread.sleep(dur!("seconds")(1));
                return open();
            }
        }
    }

    public void reopen()
    {
//close();
//open();
    }

    public void close()
    {
//if (db_is_opened == true) {
//		tnt_close(tnt);
//      tnt_stream_free(tnt);
//      db_is_opened = false;
//	}
    }

    public long count_entries()
    {
        return -1;
    }

    public void flush(int force)
    {
    }
}
