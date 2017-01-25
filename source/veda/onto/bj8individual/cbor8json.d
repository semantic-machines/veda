/**
 * CBOR: cbor <-> JSONValue
 */
module veda.onto.bj8individual.cbor8json;

private import std.outbuffer, std.stdio, std.string, std.conv, std.datetime, std.json;
private import veda.common.type, veda.onto.resource, veda.onto.individual, veda.onto.bj8individual.cbor, veda.onto.lang;

string dummy;

private static int read_element(JSONValue *individual, ubyte[] src, out string _key, string subject_uri = null,
                                string predicate_uri = null)
{
    int           pos;
    ElementHeader header;

    pos = read_type_value(src[ 0..$ ], &header);

    if (pos == 0)
    {
        writeln("EX! cbor8json.read_element src.length=", src.length);
        throw new Exception("no content in pos");
    }

    if (header.type == MajorType.MAP)
    {
        //writeln("IS MAP, length=", header.v_long, ", pos=", pos);
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
                JSONValue new_individual;
                individual = &new_individual;
            }
            (*individual)[ "@" ] = val.dup;
            new_subject_uri      = val;

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
        //writeln ("IS STRING, length=", header.v_long, ", pos=", pos);
        int    ep = cast(int)(pos + header.v_long);

        string str = cast(string)src[ pos..ep ].dup;
        _key = str;

        //writeln ("#1 text=[", str, "]");

        if (subject_uri !is null && predicate_uri !is null)
        {
            //writeln ("*1 |", individual.toString (), "|");

            JSONValue resources = (*individual)[ predicate_uri ];

            //JSONValue resources = kk.get (predicate_uri, JSONValue.emptyArray);

            //writeln ("JSON0:", resources);
            //JSONValue* resources = individual[predicate_uri];
            JSONValue resource_json;

            if (header.tag == TAG.TEXT_RU)
            {
                resource_json[ "type" ] = text(DataType.String);

                resource_json[ "data" ] = str;
                resource_json[ "lang" ] = text(LANG.RU);
            }
            else if (header.tag == TAG.TEXT_EN)
            {
                resource_json[ "type" ] = text(DataType.String);

                resource_json[ "data" ] = str;
                resource_json[ "lang" ] = text(LANG.EN);
            }
            else if (header.tag == TAG.URI)
            {
                resource_json[ "type" ] = text(DataType.Uri);
//                if (str.indexOf('/') > 0)

                resource_json[ "data" ] = str;
            }
            else
            {
                resource_json[ "type" ] = text(DataType.String);
                resource_json[ "data" ] = str;
                resource_json[ "lang" ] = text(LANG.NONE);
            }
            //writeln ("JSON1:", resource_json.toString ());
            resources ~= resource_json;
            //kk[predicate_uri] = resources;
            //writeln ("JSON2:", resources.toString ());
            (*individual)[ predicate_uri ] = resources;
            //writeln ("JSON3:", individual.toString ());
        }

        //writeln ("#2 text=[", str, "]");
        pos = ep;
    }
    else if (header.type == MajorType.NEGATIVE_INTEGER)
    {
        long      value     = header.v_long;
        JSONValue resources = (*individual)[ predicate_uri ];
        JSONValue resource_json;

        //Resources resources = individual.resources.get(predicate_uri, Resources.init);
        if (header.tag == TAG.EPOCH_DATE_TIME)
        {
//          writeln ("@p #read_element TAG.EPOCH_DATE_TIME value=", value);
            resource_json[ "type" ] = text(DataType.Datetime);
            SysTime st = SysTime(unixTimeToStdTime(value), UTC());
            resource_json[ "data" ] = st.toISOExtString();
        }
        else
        {
            resource_json[ "type" ] = text(DataType.Integer);
            resource_json[ "data" ] = value;
        }
        resources ~= resource_json;
        (*individual)[ predicate_uri ] = resources;
    }
    else if (header.type == MajorType.UNSIGNED_INTEGER)
    {
        //writeln ("@p #read_element MajorType.UNSIGNED_INTEGER #0");
        long      value     = header.v_long;
        JSONValue resources = (*individual)[ predicate_uri ];
        JSONValue resource_json;
        //Resources resources = individual.resources.get(predicate_uri, Resources.init);

        if (header.tag == TAG.EPOCH_DATE_TIME)
        {
//          writeln ("@p #read_element TAG.EPOCH_DATE_TIME value=", value);
            resource_json[ "type" ] = text(DataType.Datetime);
            SysTime st = SysTime(unixTimeToStdTime(value), UTC());
            resource_json[ "data" ] = st.toISOExtString();
        }
        else
        {
            resource_json[ "type" ] = text(DataType.Integer);
            resource_json[ "data" ] = value;
        }
        resources ~= resource_json;
        (*individual)[ predicate_uri ] = resources;
        //writeln ("@p #read_element MajorType.UNSIGNED_INTEGER #end");
    }
    else if (header.type == MajorType.FLOAT_SIMPLE)
    {
        JSONValue resources = (*individual)[ predicate_uri ];
        JSONValue resource_json;
        //Resources resources = individual.resources.get(predicate_uri, Resources.init);
        if (header.v_long == TRUE)
        {
            resource_json[ "type" ] = text(DataType.Boolean);
            resource_json[ "data" ] = true;
            resources ~= resource_json;
            (*individual)[ predicate_uri ] = resources;
        }
        else if (header.v_long == FALSE)
        {
            resource_json[ "type" ] = text(DataType.Boolean);
            resource_json[ "data" ] = false;
            resources ~= resource_json;
            (*individual)[ predicate_uri ] = resources;
        }
        else
        {
        }
    }
    else if (header.type == MajorType.ARRAY)
    {
        if (header.tag == TAG.DECIMAL_FRACTION)
        {
            JSONValue resources = (*individual)[ predicate_uri ];
            JSONValue resource_json;
            //Resources     resources = individual.resources.get(predicate_uri, Resources.init);

            ElementHeader mantissa;
            pos += read_type_value(src[ pos..$ ], &mantissa);

            ElementHeader exponent;
            pos += read_type_value(src[ pos..$ ], &exponent);

            resource_json[ "type" ] = text(DataType.Decimal);
            resource_json[ "data" ] = decimal(mantissa.v_long, cast(byte)exponent.v_long).asString();

            resources ~= resource_json;
            (*individual)[ predicate_uri ] = resources;
        }
        else
        {
            //writeln ("IS ARRAY, length=", header.v_long, ", pos=", pos);
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

// ///////////////////////////////////////////////////////////////////////////////////
public int cbor2json(JSONValue *individual, string in_str)
{
    try
    {
        int res = read_element(individual, cast(ubyte[])in_str, dummy);
        // writeln ("JSON OUT:", individual.toString ());
        return res;
    }
    catch (Exception ex)
    {
        writeln("@@@ ex=", ex.msg);
        return -1;
    }
}

