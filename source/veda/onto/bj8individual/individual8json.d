module veda.onto.bj8individual.individual8json;

import std.conv, std.stdio, std.json, std.datetime, std.string;
import veda.common.type, veda.onto.individual, veda.onto.resource, veda.onto.lang;

JSONValue individual_to_json(Individual individual, string[] filters = []){
//    writeln ("\nINDIVIDUAL->:", individual);
    JSONValue json;

    json[ "@" ] = individual.uri;

    if (filters.length > 0) {
        foreach (property_name; filters) {
            auto property_values = individual.resources.get(property_name, Resources.init);
            if (property_values != Resources.init) {
                JSONValue[] jsonVals;

                foreach (property_value; property_values)
                    jsonVals ~= resource_to_json(cast(Resource)property_value);

                JSONValue resources_json;
                resources_json.array = jsonVals;

                json[ property_name ] = resources_json;
            }
        }
    }else  {
        foreach (property_name, property_values; individual.resources) {
            JSONValue[] jsonVals;

            foreach (property_value; property_values)
                jsonVals ~= resource_to_json(cast(Resource)property_value);

            JSONValue resources_json;
            resources_json.array = jsonVals;

            json[ property_name ] = resources_json;
        }
    }
//    writeln ("->JSON:", json);
    return json;
}

JSONValue[] individuals_to_json(Individual[] individuals){
//    writeln ("\nINDIVIDUAL->:", individual);
    JSONValue[] res;
    JSONValue   json;

    foreach (individual; individuals) {
        json[ "@" ] = individual.uri;
        foreach (property_name, property_values; individual.resources) {
            JSONValue[] jsonVals;

            foreach (property_value; property_values)
                jsonVals ~= resource_to_json(cast(Resource)property_value);

            JSONValue resources_json;
            resources_json.array = jsonVals;

            json[ property_name ] = resources_json;
        }
        res ~= json;
    }
//    writeln ("->JSON:", json);
    return res;
}


JSONValue resource_to_json(Resource resource){
    JSONValue resource_json;

    string    data = resource.data;

    resource_json[ "type" ] = text(resource.type);

    if (resource.type == DataType.Uri) {
        resource_json[ "data" ] = data;
    }else if (resource.type == DataType.Binary) {
        resource_json[ "data" ] = data;
    }else if (resource.type == DataType.String) {
        resource_json[ "data" ] = data;
        resource_json[ "lang" ] = text(resource.lang);
    }else if (resource.type == DataType.Integer) {
        //writeln ("@v #resource.get!long=", resource.get!long);
        resource_json[ "data" ] = resource.get!long;
    }else if (resource.type == DataType.Decimal) {
        decimal dd = resource.get!decimal;
        resource_json[ "data" ] = dd.asString();
    }else if (resource.type == DataType.Boolean) {
        if (resource.get!bool == true)
            resource_json[ "data" ] = true;
        else
            resource_json[ "data" ] = false;
    }else if (resource.type == DataType.Datetime) {
//	writeln ("@v #r->j #1 resource.get!long=", resource.get!long);

        SysTime st = SysTime(unixTimeToStdTime(resource.get!long), UTC());
        resource_json[ "data" ] = st.toISOExtString();

//	writeln ("@v #r->j #2 val=", st.toISOExtString());
    }else
        resource_json[ "data" ] = JSONValue.init;

    return resource_json;
}

