module veda.frontend.individual8vjson;

import std.conv, std.stdio;
import vibe.d;
import veda.common.type, veda.onto.onto, veda.onto.individual, veda.onto.resource, veda.onto.lang;

static LANG[ string ] Lang;
static DataType[ string ] Resource_type;

static this() {
    Lang =
    [
        "NONE":LANG.NONE, "none":LANG.NONE,
        "RU":LANG.RU, "ru":LANG.RU,
        "EN":LANG.EN, "en":LANG.EN
    ];

    Resource_type =
    [
        "Uri":DataType.Uri,
        "String":DataType.String,
        "Integer":DataType.Integer,
        "Datetime":DataType.Datetime,
        "Decimal":DataType.Decimal,
        "Boolean":DataType.Boolean,
    ];
}


Json individual_to_json(Individual individual)
{
//    writeln ("\nINDIVIDUAL->:", individual);
    Json json = Json.emptyObject;

    json[ "@" ] = individual.uri;
    foreach (property_name, property_values; individual.resources)
    {
        Json resources_json = Json.emptyArray;
        foreach (property_value; property_values)
            resources_json ~= resource_to_json(cast(Resource)property_value);
        json[ property_name ] = resources_json;
    }
//    writeln ("->JSON:", json);
    return json;
}

Individual json_to_individual(const Json individual_json)
{
//    writeln ("\nJSON->:", individual_json);
    Individual individual = Individual.init;

    foreach (string property_name, ref const property_values; individual_json)
    {
        if (property_name == "@")
        {
            individual.uri = individual_json[ property_name ].get!string; continue;
        }
        Resource[] resources = Resource[].init;
        foreach (property_value; property_values)
            resources ~= json_to_resource(property_value);

        if (resources.length > 0)
            individual.resources[ property_name ] = resources;
    }
//    writeln ("->INDIVIDUAL:", individual);
    return individual;
}

Json resource_to_json(Resource resource)
{
    //writeln ("resource=", resource);

    Json   resource_json = Json.emptyObject;

    string data = resource.data;

    resource_json[ "type" ] = text(resource.type);

    if (resource.type == DataType.Uri)
    {
        resource_json[ "data" ] = data;
    }
    else if (resource.type == DataType.String)
    {
        resource_json[ "data" ] = data;
        resource_json[ "lang" ] = text(resource.lang);
    }
    else if (resource.type == DataType.Integer)
    {
        //writeln ("@v #resource.get!long=", resource.get!long);
        resource_json[ "data" ] = resource.get!long;
    }
    else if (resource.type == DataType.Decimal)
    {
        decimal dd = resource.get!decimal;
        resource_json[ "data" ] = dd.toDouble();
    }
    else if (resource.type == DataType.Boolean)
    {
        if (resource.get!bool == true)
            resource_json[ "data" ] = true;
        else
            resource_json[ "data" ] = false;
    }
    else if (resource.type == DataType.Datetime)
    {
        SysTime st = SysTime(unixTimeToStdTime(resource.get!long), UTC());
        resource_json[ "data" ] = st.toISOExtString();
    }
    else
        resource_json[ "data" ] = Json.undefined;

    return resource_json;
}

Resource json_to_resource(const Json resource_json)
{
    Resource resource = Resource.init;

    DataType type;

    if (resource_json[ "type" ].type is Json.Type.Int)
        type = cast(DataType)resource_json[ "type" ].get!long;
    else
        type = Resource_type.get(resource_json[ "type" ].get!string, DataType.String);

    auto data_type = resource_json[ "data" ].type;

    if (type == DataType.String)
    {
        if (resource_json[ "lang" ].type is Json.Type.string)
            resource.lang = Lang.get(resource_json[ "lang" ].get!string, Lang[ "NONE" ]);
        resource.data = resource_json[ "data" ].get!string;
    }
    else if (type == DataType.Boolean)
    {
        if (data_type is Json.Type.Bool)
        {
            bool bb = resource_json[ "data" ].get!bool;
            if (bb == true)
                resource = true;
            else
                resource = false;
        }
    }
    else if (type == DataType.Uri)
    {
        resource.data = resource_json[ "data" ].get!string;
    }
    else if (type == DataType.Decimal)
    {
        if (data_type is Json.Type.Float)
        {
            resource = decimal(resource_json[ "data" ].get!double);
        }
        else if (data_type is Json.Type.Int)
        {
            resource = resource_json[ "data" ].get!long;
        }
        else if (data_type is Json.Type.String)
        {
            resource = decimal(resource_json[ "data" ].get!string);
        }
    }
    else if (type == DataType.Integer)
    {
        resource = resource_json[ "data" ].get!long;
    }
    else if (type == DataType.Datetime)
    {
        try
        {
            string val = resource_json[ "data" ].get!string;
            long   tm;
//	    writeln ("@v j->r #0 ", val);

            if (val.indexOf('-') >= 1)
                tm = stdTimeToUnixTime(SysTime.fromISOExtString(val).stdTime());
            else
                tm = to!long (val);

            resource = tm;
//	    writeln ("@v j->r #1 ", tm);
        }
        catch (Exception ex)
        {
            writeln("EX! ", __FILE__, ", line:", __LINE__, ", [", ex.msg, "], in ", resource_json);
        }
//	writeln ("@v j->r #2");
    }

    resource.type = type;
    return resource;
}

