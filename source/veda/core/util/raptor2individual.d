module util.raptor2individual;

import std.string, std.stdio : writeln;
import bind.libraptor_header;
import veda.type, veda.onto.individual, veda.onto.resource, veda.core.context, veda.core.define;
import onto.lang;
import util.utils;

string[ string ] prefixes;
Individual *[ string ] _individuals;
raptor_world *world = null;

extern (C) void prepare_prefixes(void *user_data, raptor_namespace *ns)
{
    auto uri_string = raptor_uri_as_string(ns.uri);

    if (ns.prefix !is null)
    {
        //writeln(fromStringz(ns.prefix), " -> ", fromStringz(uri_string));
        string prefix = fromStringz(ns.prefix).dup;
        prefix ~= ":";
        string url = fromStringz(uri_string).dup;
        prefixes[ url ]    = prefix;
        prefixes[ prefix ] = url;

        if (url.length > 3 && (url[ $ - 1 ] == '#' || url[ $ - 1 ] == '/'))
        {
            url             = url[ 0..$ - 1 ].dup;
            prefixes[ url ] = prefix;
//            prefixes[ prefix ] = url;
        }
    }
    else
        writeln("raptor2individual.prepare_prefixes, ns.prefix is null: " ~ fromStringz(uri_string));
}

private string replace_full_prefix(string url)
{
    string full_prefix;

    string short_prefix = prefixes.get(url, null);

    if (short_prefix !is null)
        return short_prefix;

    long aa = url.lastIndexOf('#');

    if (aa > 1)
    {
        full_prefix = url[ 0..aa + 1 ];
    }
    else
    {
        aa = url.lastIndexOf('/');
        if (aa > 1)
        {
            full_prefix = url[ 0..aa + 1 ];
        }
        else
            full_prefix = url;
    }

    //writeln ("FULL_PREFIX=[", full_prefix, "]");

    short_prefix = prefixes.get(full_prefix, null);

    if (short_prefix !is null)
    {
        if (url.length > full_prefix.length)
            url = short_prefix ~ url[ aa + 1..$ ];
        else
            url = short_prefix;
    }

    //writeln ("SHORT_PREFIX=", short_prefix);
    return url;
}

extern (C) void prepare_triple(void *user_data, raptor_statement *triple)
{
    auto _ss = raptor_term_to_string(triple.subject);
    auto _pp = raptor_term_to_string(triple.predicate);
    auto _oo = raptor_term_to_string(triple.object);


    string ss = fromStringz(_ss).dup;

    if (triple.subject.type == raptor_term_type.RAPTOR_TERM_TYPE_URI)
    {
        ss = replace_full_prefix(ss[ 1..$ - 1 ]);
    }

    Individual *ii = _individuals.get(ss, null);
    if (ii is null)
    {
        ii                 = new Individual();
        ii.uri             = ss;
        _individuals[ ss ] = ii;
    }

    string pp;
    if (triple.predicate.type == raptor_term_type.RAPTOR_TERM_TYPE_URI)
        pp = replace_full_prefix(cast(string)(fromStringz(_pp)[ 1..$ - 1 ])).dup;


    string oo = cast(string) fromStringz(_oo);

    if (triple.object.type == raptor_term_type.RAPTOR_TERM_TYPE_URI)
    {
        oo = replace_full_prefix(oo[ 1..$ - 1 ]).dup;
        ii.addResource(pp, Resource(DataType.Uri, oo));
    }
    else if (triple.object.type == raptor_term_type.RAPTOR_TERM_TYPE_LITERAL)
    {
        oo = uxxxx2utf8(oo).dup;
        long   end_pos_value = oo.lastIndexOf('"');

        string type = "xsd:string";
        long   aa   = oo.lastIndexOf("^^<");
        if (aa > 2)
        {
            type = replace_full_prefix(oo[ aa + 3..$ - 1 ]);
        }

        string _lang;
        aa = oo.lastIndexOf('@');
        if (aa > 2 && aa > end_pos_value)
        {
            _lang = oo[ aa + 1..aa + 3 ];
        }
        oo = oo[ 1..end_pos_value ];

        if (type == "xsd:dateTime")
        {
            ii.addResource(pp, Resource(DataType.Datetime, oo));
        }
        else if (type == "xsd:date")
        {
            ii.addResource(pp, Resource(DataType.Datetime, oo));
        }
        else if (type == "xsd:boolean")
        {
            ii.addResource(pp, Resource(DataType.Boolean, oo));
        }
        else if (type == "xsd:nonNegativeInteger")
        {
            ii.addResource(pp, Resource(DataType.Integer, oo));
        }
        else if (type == "xsd:integer")
        {
            ii.addResource(pp, Resource(DataType.Integer, oo));
        }
        else if (type == "xsd:decimal")
        {
            ii.addResource(pp, Resource(DataType.Decimal, oo));
        }
        else if (type == "xsd:string")
        {
            if (_lang !is null)
            {
                LANG lang = LANG.NONE;
                if (_lang[ 0 ] == 'r' && _lang[ 1 ] == 'u')
                    lang = LANG.RU;
                else if (_lang[ 0 ] == 'e' && _lang[ 1 ] == 'n')
                    lang = LANG.EN;

                ii.addResource(pp, Resource(oo, lang));
            }
            else
                ii.addResource(pp, Resource(oo));
        }
        else
        {
            ii.addResource(pp, Resource(oo));
        }
    }


    //writeln(ss, " ", pp, " ", oo);
    //writeln (*ii);
}


public Individual *[ string ] ttl2individuals(string file_name, Context context)
{
    Individual *[ string ] res;

    file_name ~= "\0";
    prefixes = context.get_prefix_map();

    raptor_parser *rdf_parser = null;
    char          *uri_string;
    raptor_uri    *uri;
    raptor_uri    *base_uri;

    //if (world is null)
    world = raptor_new_world_internal();

    rdf_parser = raptor_new_parser(world, "turtle\0".ptr);

    void *user_data = null;

    raptor_parser_set_statement_handler(rdf_parser, user_data, &prepare_triple);
    raptor_parser_set_namespace_handler(rdf_parser, user_data, &prepare_prefixes);

    uri_string = raptor_uri_filename_to_uri_string((file_name ~ "\0").ptr);
    uri        = raptor_new_uri(world, uri_string);
    base_uri   = raptor_uri_copy(uri);

    raptor_parser_parse_file(rdf_parser, uri, base_uri);
    context.add_prefix_map(prefixes);

    res = _individuals.dup;

    raptor_free_parser(rdf_parser);

    raptor_free_uri(base_uri);
    raptor_free_uri(uri);
    raptor_free_memory(uri_string);

    _individuals = (Individual *[ string ]).init;

    raptor_free_world(world);
    return res;
}

