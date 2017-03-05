module veda.util.tests_tools;

import std.uuid, std.datetime, std.file;
import veda.onto.individual, veda.onto.resource, veda.common.type, veda.onto.lang;

public Individual generate_new_test_individual()
{
    Individual new_indv_A;

    new_indv_A.uri = "td:" ~ randomUUID().toString();

    new_indv_A.addResource("v-s:isSuccess", Resource(true));
    new_indv_A.addResource("v-s:infoOfExecuting", Resource("text(res))"));
    new_indv_A.addResource("v-s:info1", Resource(DataType.Uri, "rdfs:label"));
    new_indv_A.addResource("v-s:info2", Resource("русский текст", LANG.RU));
    new_indv_A.addResource("v-s:info2", Resource("english text", LANG.EN));
    new_indv_A.addResource("v-s:info2", Resource("none lang text", LANG.NONE));
    new_indv_A.addResource("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));
    new_indv_A.addResource("rdfs:label", Resource(1234));
    new_indv_A.addResource("rdfs:label", Resource(decimal (cast(long)1234, cast(byte)25)));
    new_indv_A.addResource("rdfs:label", Resource(false));

    return new_indv_A;
}

public string get_test_path ()
{
    try
    {
        mkdir("./tmp");
    }
    catch (Exception ex)
    {
    }

    string path = "./tmp/" ~ randomUUID().toString();

    try
    {
        mkdir(path);
    }
    catch (Exception ex)
    {
    }

	return path;
}