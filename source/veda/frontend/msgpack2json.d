/**
 * MSGPACK <-> vibe Json
 */
module veda.frontend.msgpack2vjson;

private import std.outbuffer, std.stdio, std.string, std.conv, std.datetime;
private import vibe.data.json, msgpack;
private import veda.common.type, veda.onto.resource, veda.onto.individual, veda.onto.lang;

ubyte magic_header = 146;

// ///////////////////////////////////////////////////////////////////////////////////
public int msgpack2json(Json *individual, string in_str)
{
    try
    {
        ubyte[] src = cast(ubyte[])in_str;

        if (src[ 0 ] != magic_header)
        {
            stderr.writeln("ERR! msgpack2vjson: invalid format");
            return -1;
        }

        if (src.length < 5)
        {
            stderr.writefln("ERR! msgpack2vjson: binobj is empty [%s]", src);
            return -1;
        }

        StreamingUnpacker unpacker = StreamingUnpacker(src[ 0..$ ]);

        if (unpacker.execute())
        {
            size_t root_el_size = unpacker.unpacked.length;
            // writefln("TRY TO UNPACK root_el_size=%d", root_el_size);
            if (root_el_size != 2)
            {
                stderr.writeln("ERR! msgpack2individual: root_el_size != 2");
                return -1;
            }

            foreach (obj; unpacker.purge())
            {
                switch (obj.type)
                {
                case Value.Type.raw:

                    //stderr.writefln("URI = %s", (cast(string)obj.via.raw));
                    (*individual)[ "@" ] = (cast(string)obj.via.raw).dup;

                    break;

                case Value.Type.map:

                    Value[ Value ] map = obj.via.map;
                    foreach (key; map.byKey)
                    {
                        string  predicate = (cast(string)key.via.raw).dup;

                        Json    resources      = Json.emptyArray;
                        Value[] resources_vals = map[ key ].via.array;
                        // writeln("\t\tTRY UNPACK RESOURCES len ", resources_vals.length);
                        for (int i = 0; i < resources_vals.length; i++)
                        {
                            Json resource_json = Json.emptyObject;

                            // writeln("\t\t\tTRY UNPACK RESOURCES type ", resources_vals[i].type);
                            switch (resources_vals[ i ].type)
                            {
                            case Value.Type.array:
                                Value[] arr = resources_vals[ i ].via.array;
                                if (arr.length == 2)
                                {
                                    long type = arr[ 0 ].via.uinteger;

                                    if (type == DataType.Datetime)
                                    {
                                        long value;

                                        if (arr[ 1 ].type == Value.Type.unsigned)
                                            value = arr[ 1 ].via.uinteger;
                                        else
                                            value = arr[ 1 ].via.integer;


                                        resource_json[ "type" ] = text(DataType.Datetime);
                                        SysTime st = SysTime(unixTimeToStdTime(value), UTC());
                                        resource_json[ "data" ] = st.toISOExtString();
                                        resources ~= resource_json;
                                    }
                                    else if (type == DataType.String)
                                    {
                                        string str;

                                        if (arr[ 1 ].type == Value.type.raw)
                                            str = (cast(string)arr[ 1 ].via.raw).dup;
                                        else if (arr[ 1 ].type == Value.type.nil)
                                            str = "";


                                        resource_json[ "type" ] = text(DataType.String);

                                        resource_json[ "data" ] = str;
                                        resource_json[ "lang" ] = text(LANG.NONE);
                                        resources ~= resource_json;
                                    }
                                    else
                                    {
                                        stderr.writeln("ERR! msgpack2individual: [0][1] unknown type [%d]", type);
                                        return -1;
                                    }
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

                                        resource_json[ "type" ] = text(DataType.Decimal);

                                        auto dres = decimal(mantissa, cast(byte)exponent);
                                        resource_json[ "data" ] = dres.asString();
                                        resources ~= resource_json;
                                    }
                                    else if (type == DataType.String)
                                    {
                                        string str  = (cast(string)arr[ 1 ].via.raw).dup;
                                        long   lang = arr[ 2 ].via.uinteger;

                                        resource_json[ "type" ] = text(DataType.String);

                                        resource_json[ "data" ] = str;
                                        resource_json[ "lang" ] = text(cast(LANG)lang);
                                        resources ~= resource_json;
                                    }
                                    else
                                    {
                                        stderr.writeln("ERR! msgpack2individual: [0][1][3] unknown type [%d]", type);
                                        return -1;
                                    }
                                }
                                break;

                            case Value.Type.raw:
                                string str              = (cast(string)resources_vals[ i ].via.raw).dup;
                                resource_json[ "type" ] = text(DataType.Uri);
                                resource_json[ "data" ] = str;
                                resources ~= resource_json;
                                break;

                            case Value.Type.unsigned:

                                long value              = resources_vals[ i ].via.uinteger;
                                resource_json[ "type" ] = text(DataType.Integer);
                                resource_json[ "data" ] = value;
                                resources ~= resource_json;

                                break;

                            case Value.Type.signed:

                                long value              = resources_vals[ i ].via.integer;
                                resource_json[ "type" ] = text(DataType.Integer);
                                resource_json[ "data" ] = value;
                                resources ~= resource_json;

                                break;


                            case Value.Type.boolean:

                                resource_json[ "type" ] = text(DataType.Boolean);
                                resource_json[ "data" ] = resources_vals[ i ].via.boolean;
                                resources ~= resource_json;

                                break;

                            default:
                                stderr.writeln("ERR! msgpack2individual: unknown type [%d]", resources_vals[ i ].type);
                                break;
                            }
                        }
                        (*individual)[ predicate ] = resources;
                    }
                    break;

                default:
                    break;
                }
            }
        }
        else
        {
            stderr.writefln("ERR! msgpack2individual: binobj is invalid! src=[%s]", in_str);
            return -1;
        }

        //stderr.writefln("JSON OUT: %s", individual.serializeToPrettyJson ());

        return 1;
    }
    catch (Exception ex)
    {
        writeln("@@@ ex=", ex.msg);
        return -1;
    }
}

