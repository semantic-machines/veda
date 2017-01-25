/**
 * CBOR: cbor <-> individual

   Copyright: © 2014-2015 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev
 */

module veda.onto.bj8individual.cbor8individual;

private import std.outbuffer, std.stdio, std.string;
private import veda.common.type, veda.onto.resource, veda.onto.individual, veda.onto.lang, veda.onto.bj8individual.cbor;
import backtrace.backtrace;
import Backtrace = backtrace.backtrace;

string dummy;

private static int read_element(Individual *individual, ubyte[] src, out string _key, string subject_uri = null,
                                string predicate_uri = null)
{
    int           pos;
    ElementHeader header;

    pos = read_type_value(src[ 0..$ ], &header);

    if (pos == 0)
    {
        //writeln("EX! cbor8individual.read_element src.length=", src.length);
        throw new Exception("no content in pos");
    }

    if (header.type == MajorType.MAP)
    {
        //writeln("IS MAP, length=", header.len, ", pos=", pos);
        string new_subject_uri;
        string key;
        int    len = read_element(individual, src[ pos..$ ], key);
        if (len <= 0)
        {
            writeln("@^^^1 individual=", *individual);
            throw new Exception("no content in pos");
        }
        pos += len;

        string val;
        len = read_element(individual, src[ pos..$ ], val);
        if (len <= 0)
        {
            writeln("@^^^2 individual=", *individual);
            throw new Exception("no content in pos");
        }
        pos += len;

        if (key == "@")
        {
            if (subject_uri !is null)
            {
                Individual new_individual = Individual();
                individual = &new_individual;
            }
            individual.uri  = val.dup;
            new_subject_uri = val;

            //writeln ("@ id:", val);
        }

        foreach (i; 1 .. header.v_long)
        {
            int len1 = read_element(individual, src[ pos..$ ], key);
            if (len1 <= 0)
            {
                writeln("@^^^3 individual=", *individual);
                throw new Exception("no content in pos");
            }
            pos += len1;

            string new_predicate_uri = key;

            len1 = read_element(individual, src[ pos..$ ], dummy, new_subject_uri, new_predicate_uri);
            if (len1 <= 0)
            {
                writeln("@^^^4 individual=", *individual);
                throw new Exception("no content in pos");
            }
            pos += len1;
        }
    }
    else if (header.type == MajorType.TEXT_STRING)
    {
        //writeln ("IS STRING, length=", header.len, ", pos=", pos);
        int    ep = cast(int)(pos + header.v_long);

        string str = cast(string)src[ pos..ep ].dup;
        _key = str;

        if (subject_uri !is null && predicate_uri !is null)
        {
            Resources resources = individual.resources.get(predicate_uri, Resources.init);

            if (header.tag == TAG.TEXT_RU)
                resources ~= Resource(DataType.String, str, LANG.RU);
            else if (header.tag == TAG.TEXT_EN)
                resources ~= Resource(DataType.String, str, LANG.EN);
            else if (header.tag == TAG.URI)
            {
                resources ~= Resource(DataType.Uri, str);
            }
            else
                resources ~= Resource(DataType.String, str);

            individual.resources[ predicate_uri ] = resources;
        }

        pos = ep;
    }
    else if (header.type == MajorType.NEGATIVE_INTEGER)
    {
        long      value     = header.v_long;
        Resources resources = individual.resources.get(predicate_uri, Resources.init);
        if (header.tag == TAG.EPOCH_DATE_TIME)
        {
//          writeln ("@p #read_element TAG.EPOCH_DATE_TIME value=", value);
            resources ~= Resource(DataType.Datetime, value);
        }
        else
        {
            resources ~= Resource(value);
        }
        individual.resources[ predicate_uri ] = resources;
    }
    else if (header.type == MajorType.UNSIGNED_INTEGER)
    {
        //writeln ("@p #read_element MajorType.UNSIGNED_INTEGER #0");
        long      value     = header.v_long;
        Resources resources = individual.resources.get(predicate_uri, Resources.init);

        if (header.tag == TAG.EPOCH_DATE_TIME)
        {
//          writeln ("@p #read_element TAG.EPOCH_DATE_TIME value=", value);
            resources ~= Resource(DataType.Datetime, value);
        }
        else
        {
            resources ~= Resource(value);
        }
        individual.resources[ predicate_uri ] = resources;
        //writeln ("@p #read_element MajorType.UNSIGNED_INTEGER #end");
    }
    else if (header.type == MajorType.FLOAT_SIMPLE)
    {
        Resources resources = individual.resources.get(predicate_uri, Resources.init);
        if (header.v_long == TRUE)
        {
            resources ~= Resource(true);
            individual.resources[ predicate_uri ] = resources;
        }
        else if (header.v_long == FALSE)
        {
            resources ~= Resource(false);
            individual.resources[ predicate_uri ] = resources;
        }
        else
        {
        }
    }
    else if (header.type == MajorType.ARRAY)
    {
        if (header.tag == TAG.DECIMAL_FRACTION)
        {
            Resources     resources = individual.resources.get(predicate_uri, Resources.init);

            ElementHeader mantissa;
            pos += read_type_value(src[ pos..$ ], &mantissa);

            ElementHeader exponent;
            pos += read_type_value(src[ pos..$ ], &exponent);

            resources ~= Resource(decimal(mantissa.v_long, cast(byte)exponent.v_long));
            individual.resources[ predicate_uri ] = resources;
        }
        else
        {
            //writeln ("IS ARRAY, length=", header.len, ", pos=", pos);
            foreach (i; 0 .. header.v_long)
            {
                int len = read_element(individual, src[ pos..$ ], dummy, subject_uri, predicate_uri);
                if (len <= 0)
                {
                    writeln("@^^^5 individual=", *individual);
                    throw new Exception("no content in pos");
                }
                pos += len;
            }
        }
    }
    else if (header.type == MajorType.TAG)
    {
        //writeln ("IS TAG, length=", header.len, ", pos=", pos);
    }
    return pos;
}

private void write_individual(Individual *ii, ref OutBuffer ou)
{
    ulong     map_len = ii.resources.length + 1;
    MajorType type    = MajorType.MAP;

    write_type_value(type, map_len, ou);
    write_string("@", ou);
    write_string(ii.uri, ou);

    foreach (key, resources; ii.resources)
    {
        if (resources.length > 0)
            write_resources(key, resources, ou);
    }
}

private void write_resources(string uri, ref Resources vv, ref OutBuffer ou)
{
    write_string(uri, ou);

    if (vv.length > 1)
        write_type_value(MajorType.ARRAY, vv.length, ou);

    foreach (value; vv)
    {
        if (value.type == DataType.Uri)
        {
            string svalue = value.get!string;
            //if (svalue !is null && svalue.length > 0)
            {
                write_type_value(MajorType.TAG, TAG.URI, ou);
                write_string(svalue, ou);
            }
        }
        else if (value.type == DataType.Integer)
        {
            write_integer(value.get!long, ou);
        }
        else if (value.type == DataType.Datetime)
        {
            write_type_value(MajorType.TAG, TAG.EPOCH_DATE_TIME, ou);
            write_integer(value.get!long, ou);
        }
        else if (value.type == DataType.Decimal)
        {
            decimal x = value.get!decimal;

            write_type_value(MajorType.TAG, TAG.DECIMAL_FRACTION, ou);
            write_type_value(MajorType.ARRAY, 2, ou);
            write_integer(x.mantissa, ou);
            write_integer(x.exponent, ou);
        }
        else if (value.type == DataType.Boolean)
        {
            write_bool(value.get!bool, ou);
        }
        else
        {
            string svalue = value.get!string;
            //if (svalue !is null && svalue.length > 0)
            {
                if (value.lang != LANG.NONE)
                    write_type_value(MajorType.TAG, value.lang + 41, ou);
                write_string(svalue, ou);
            }
        }
    }
}

// ///////////////////////////////////////////////////////////////////////////////////
public int cbor2individual(Individual *individual, string in_str)
{
    try
    {
        return read_element(individual, cast(ubyte[])in_str, dummy);
    }
    catch (Throwable ex)
    {
        writeln("ERR! cbor2individual ex=", ex.msg, ", in_str=", in_str);
        //printPrettyTrace(stderr);
        //throw new Exception("invalid cbor");
        return -1;
    }
}

public string individual2cbor(Individual *in_obj)
{
    OutBuffer ou = new OutBuffer();

    write_individual(in_obj, ou);

    return ou.toString();
}
