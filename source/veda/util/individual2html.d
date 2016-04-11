/**
 * individual -> html
 */
module util.individual2html;

private import std.outbuffer, std.stdio, std.string, std.conv;
private import veda.type, veda.onto.resource, veda.onto.individual, veda.onto.lang;
import backtrace.backtrace;
import Backtrace = backtrace.backtrace;


private void write_individual(Individual *ii, ref OutBuffer ou)
{
    ou.write("\n<p><b><a name=\"");
    ou.write(ii.uri);
    ou.write("\"" "></a>");
    ou.write(ii.uri);
    ou.write("<b>\n");

    Resources type  = ii.resources.get("rdf:type", Resources.init);
    Resources label = ii.resources.get("rdfs:label", Resources.init);

    write_resources("rdf:type", type, ou);
    write_resources("rdfs:label", label, ou);
    foreach (key, pp; ii.resources)
    {
        if (key != "rdf:type" && key != "rdfs:label")
            write_resources(key, pp, ou);
    }
    ou.write("<br>.<br><br>");
}

private void write_resources(string uri, ref Resources vv, ref OutBuffer ou)
{
    if (vv == Resources.init)
    {
        ou.write("\n<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;???");
        return;
    }

    ou.write("\n<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    if (uri == "rdf:type")
        ou.write("<font color=\"#B22222\">");
    else
        ou.write("<font>");
    ou.write(uri);
    ou.write("</font>&nbsp;");

    bool f          = false;
    long str_b_size = ou.data.length;

    foreach (value; vv)
    {
        bool is_split_predicate = false;

        if (f)
        {
            if (value.type != DataType.Uri || ou.data.length - str_b_size > 80)
                is_split_predicate = true;


            if (is_split_predicate)
            {
                ou.write(";\n<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
                ou.write(uri);
                ou.write("&nbsp;");
                is_split_predicate = false;
                str_b_size         = ou.data.length;
            }
            else
                ou.write(", ");
        }



        if (value.type == DataType.Uri)
        {
            string svalue = value.get!string;
            if (svalue !is null && svalue.length > 0)
            {
                ou.write("<a href=\"");
                ou.write(value.prefix);
                ou.write(".html#");
                ou.write(svalue);
                ou.write("\">");
                ou.write(svalue);
                ou.write("</a>");
            }
        }
        else if (value.type == DataType.Integer)
        {
            ou.write("&quot;");
            ou.write(text(value.get!long));
            ou.write("&quot;");
        }
        else if (value.type == DataType.Datetime)
        {
            ou.write("&quot;");
            ou.write(text(value.get!long));
            ou.write("&quot;");
        }
        else if (value.type == DataType.Decimal)
        {
            ou.write("&quot;");
            ou.write(text(value.get!decimal.toDouble()));
            ou.write("&quot;");
        }
        else if (value.type == DataType.Boolean)
        {
            ou.write("&quot;");
            ou.write(text(value.get!bool));
            ou.write("&quot;");
        }
        else
        {
            string svalue = value.get!string;

            if (svalue !is null && svalue.length > 0)
            {
                if ((svalue.indexOf('{') > 0 &&
                     svalue.indexOf('}') > 0) || (svalue.indexOf('<') > 0 && svalue.indexOf(">") > 0 && svalue.indexOf("</") > 0))
                {
                    ou.write("<font color=\"#2A35A0\">");
                    ou.write("&quot;");
                    ou.write("<pre>");
                    svalue = svalue.replace("<", "&lt;").replace(">", "&gt;");
                    ou.write(svalue);
                    ou.write("</pre>");
                }
                else
                {
                    ou.write("<font color=\"#DAA520\">");
                    ou.write("&quot;");
                    ou.write(svalue);
                }

                ou.write("&quot;");
                ou.write("</font>");

                if (value.lang != LANG.NONE)
                {
                    ou.write("@");
                    ou.write(text(value.lang));
                }

                //write_type_value(MajorType.TAG, value.lang + 41, ou);
                //write_string(svalue, ou);
            }
        }
        f = true;
    }
    ou.write(";");
}

public string individual2html(Individual *in_obj)
{
    OutBuffer ou = new OutBuffer();

    write_individual(in_obj, ou);

    return ou.toString();
}
