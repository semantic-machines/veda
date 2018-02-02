/**
 * msgpack <-> individual
   Copyright: © 2014-2017 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev
 */

module veda.onto.bj8individual.msgpack8individual;

private import msgpack;
private import std.outbuffer, std.stdio, std.string, std.conv;
private import veda.common.type, veda.onto.resource, veda.onto.individual, veda.onto.lang, veda.bind.msgpuck;
import veda.util.tests_tools;
import backtrace.backtrace;
import Backtrace = backtrace.backtrace;

string          dummy;
ubyte[]         buff;

private ubyte[] write_individual(ref Individual ii)
{
    // writeln("PACK START");
    Packer packer = Packer(false);

    packer.beginArray(2).pack(ii.uri.dup);


    // stderr.writef("D WRITE ARRAY of 2\n");
    packer.beginMap(ii.resources.length);
    // stderr.writef("D WRITE RESOURCES\n");
    foreach (key, resources; ii.resources)
        write_resources(key, resources, packer);

    // writefln("PACKED %s", cast(string)packer.stream.data);
    // writeln("PACK END");
    return packer.stream.data;
}

private void write_resources(string uri, ref Resources vv, ref Packer packer)
{
    packer.pack(uri.dup);
    // stderr.writef("\tRESOURCE URI=%s\n", uri);
    packer.beginArray(vv.length);
    foreach (value; vv)
    {
        if (value.type == DataType.Uri)
        {
            string svalue = value.get!string;

            if (svalue == "")
                packer.pack(null);
            else
                packer.pack(svalue.dup);
            // stderr.writef("\tDATATYPE URI %s\n", value.get!string);
        }
        else if (value.type == DataType.Integer)
        {
            packer.pack(value.get!long);
            // stderr.writef("\tDATATYPE INTEGER %d\n", value.get!long);
        }
        else if (value.type == DataType.Datetime)
        {
            packer.beginArray(2).pack(DataType.Datetime, value.get!long);
            // stderr.writef("\tDATATYPE DATETIME %d\n", value.get!long);
        }
        else if (value.type == DataType.Decimal)
        {
            decimal x = value.get!decimal;
            packer.beginArray(3).pack(DataType.Decimal, x.mantissa, x.exponent);
            // stderr.writef("\tDATATYPE DECIMAL %d %d\n", x.mantissa, x.exponent);
        }
        else if (value.type == DataType.Boolean)
        {
            packer.pack(value.get!bool);
            // stderr.writef("\tDATATYPE BOOLEAN %s\n", value.get!long);
        }
        else
        {
            string svalue = value.get!string;

            if (value.lang != LANG.NONE)
            {
                if (svalue == "")
                    packer.beginArray(3).pack(DataType.String, null, value.lang);
                else
                    packer.beginArray(3).pack(DataType.String, svalue.dup, value.lang);
                // stderr.writef("\tSOME LANG %s %d\n", svalue , value.lang);
            }
            else
            {
                if (svalue == "")
                    packer.beginArray(2).pack(DataType.String, null);
                else
                    packer.beginArray(2).pack(DataType.String, svalue.dup);
                // stderr.writef("\tLANG NONE %s\n", svalue);
            }
        }
    }
}

ubyte[] magic_header = [0xFF]; 

public string individual2msgpack(ref Individual in_obj)
{
	// this concatinate created copy ?
	return cast(string) (magic_header ~ write_individual(in_obj));
	
    //ubyte[] buff = write_individual(in_obj);
    //return cast(string)buff[ 0..buff.length ].dup;
}

/////////////////////////////////////////////////////////////////////

public int msgpack2individual(ref Individual individual, string in_str)
{
    try
    {
        try
        {
            StreamingUnpacker unpacker = StreamingUnpacker(cast(ubyte[])in_str);

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
                        individual.uri = (cast(string)obj.via.raw).dup;

                        break;

                    case Value.Type.map:

                        Value[ Value ] map = obj.via.map;
                        foreach (key; map.byKey)
                        {
                            string    predicate = (cast(string)key.via.raw).dup;

                            Resources resources      = Resources.init;
                            Value[]   resources_vals = map[ key ].via.array;
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

                                        if (type == DataType.Datetime)
                                        {
                                            if (arr[ 1 ].type == Value.Type.unsigned)
                                                resources ~= Resource(DataType.Datetime,
                                                                      arr[ 1 ].via.uinteger);
                                            else
                                                resources ~= Resource(DataType.Datetime,
                                                                      arr[ 1 ].via.integer);
                                        }
                                        else if (type == DataType.String)
                                        {
                                            if (arr[ 1 ].type == Value.type.raw)
                                                resources ~= Resource(DataType.String,
                                                                      (cast(string)arr[ 1 ].via.raw).dup, LANG.NONE);
                                            else if (arr[ 1 ].type == Value.type.nil)
                                                resources ~= Resource(DataType.String, "",
                                                                      LANG.NONE);
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

                                            resources ~= Resource(decimal(mantissa,
                                                                          cast(byte)exponent));
                                        }
                                        else if (type == DataType.String)
                                        {
                                            long lang = arr[ 2 ].via.uinteger;
                                            resources ~= Resource(DataType.String,
                                                                  (cast(string)arr[ 1 ].via.raw).dup, cast(LANG)lang);
                                        }
                                        else
                                        {
                                            stderr.writeln("ERR! msgpack2individual: [0][1][3] unknown type [%d]", type);
                                            return -1;
                                        }
                                    }
                                    break;

                                case Value.Type.raw:
                                    // writeln("\t\t\t\t", cast(string)resources_vals[i].via.raw);
                                    resources ~= Resource(DataType.Uri,
                                                          (cast(string)resources_vals[ i ].via.raw).dup);
                                    break;

                                case Value.Type.unsigned:
                                    resources ~= Resource(DataType.Integer,
                                                          resources_vals[ i ].via.uinteger);
                                    break;

                                case Value.Type.signed:
                                    resources ~= Resource(DataType.Integer,
                                                          resources_vals[ i ].via.integer);
                                    break;


                                case Value.Type.boolean:
                                    resources ~= Resource(DataType.Boolean,
                                                          resources_vals[ i ].via.boolean);
                                    break;

                                default:
                                    stderr.writeln("ERR! msgpack2individual: unknown type [%d]", resources_vals[ i ].type);
                                    break;
                                }
                            }
                            individual.resources[ predicate ] = resources;
                        }
                        break;

                    default:
                        break;
                    }
                }
            }
            else
            {
                stderr.writeln("ERR! msgpack2individual: serialized object is invalid! src=[%s]", in_str);
                return -1;
            }

            return 1;
            // return cast(int)(ptr - cast(char *)in_str.ptr); //read_element(individual, cast(ubyte[])in_str, dummy);
        }
        catch (Throwable ex)
        {
            stderr.writeln("ERR! msgpack2individual ex=", ex.msg, ", in_str=", in_str);
            printPrettyTrace(stderr);
            //throw new Exception("invalid binobj");
            return -1;
        }
    } finally
    {
        //writeln ("@d msgpack2individual @E");
    }
}