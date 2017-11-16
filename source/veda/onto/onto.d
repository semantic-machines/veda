/**
 * кэш из индивидов относящихся к онтологии
 */

module veda.onto.onto;

// TODO сделать перезагрузку онтологии только в случае ее изменения (проверять CRC?)

private
{
    import std.stdio, std.datetime, std.conv, std.concurrency, std.outbuffer, std.exception : assumeUnique;
    import std.algorithm, std.algorithm.mutation                                            : SwapStrategy;
    import veda.onto.resource, veda.onto.individual;
    import veda.core.util.utils, veda.util.container, veda.common.logger, veda.common.type;
    import veda.core.common.know_predicates, veda.core.common.context, veda.core.common.log_msg, veda.core.common.define;
}

alias bool[ string ] Names;

// bi-directional access to hierarchical elements
private class Bdathe
{
    private Names[ string ] el_2_super_els;
    private Names[ string ] el_2_sub_els;
    private bool[ string ]  orphans;
    private bool[ string ] els;

    override string toString()
    {
        OutBuffer ob = new OutBuffer();

        int       idx;

        string[]  keys        = el_2_super_els.keys();
        auto      keys_sorted = sort!("toUpper(a) < toUpper(b)", SwapStrategy.stable)(keys);
        foreach (key; keys_sorted)
        {
            Names value = el_2_super_els[ key ];
            if (value.keys.length > 0)
            {
                idx++;
                ob.write(text(idx));
                ob.write(" el_2_super_els ");
                string kk = value.keys[ 0 ];
                bool   ff = value.values[ 0 ];
                ob.write(key);
                ob.write("->");
                ob.write(kk);

                if (kk)
                    ob.write("[*]");
                else
                    ob.write("[ ]");
                ob.write("\n");
            }
        }

        idx         = 0;
        keys        = el_2_sub_els.keys();
        keys_sorted = sort!("toUpper(a) < toUpper(b)", SwapStrategy.stable)(keys);
        foreach (key; keys_sorted)
        {
            Names value = el_2_sub_els[ key ];
            if (value.keys.length > 0)
            {
                idx++;
                ob.write(text(idx));
                ob.write(" el_2_sub_els ");
                string kk = value.keys[ 0 ];
                bool   ff = value.values[ 0 ];
                ob.write(key);
                ob.write("->");
                ob.write(kk);

                if (kk)
                    ob.write("[*]");
                else
                    ob.write("[ ]");
            }
            ob.write("\n");
        }

        ob.write("=== orphans ===\n");
        ob.write(text(orphans));
        ob.write("=== els ===\n");
        ob.write(text(els));

        return ob.toString();
    }
}

class Onto
{
    private Context context;
    private Logger  log;
    public int      reload_count = 0;

    private         Individual[ string ] individuals;

    private Bdathe  _class;
    private Bdathe  _property;

    public this(Context _context)
    {
        context   = _context;
        log       = context.get_logger();
        _class    = new Bdathe();
        _property = new Bdathe();
    }

    Individual[ string ] get_individuals()
    {
        return individuals;
    }

    public Names get_super_classes(string _class_uri)
    {
        return _class.el_2_super_els.get(_class_uri, null);
    }

    public Names get_sub_classes(string _class_uri)
    {
        return _class.el_2_sub_els.get(_class_uri, null);
    }

    public Names get_sub_properies(string _uri)
    {
        return _property.el_2_sub_els.get(_uri, null);
    }

    public string[] get_properies()
    {
        return _property.els.keys;
    }

    public bool isSubClasses(string _class_uri, string[] _subclasses_uri)
    {
        foreach (_subclass_uri; _subclasses_uri)
        {
            Names subclasses = _class.el_2_sub_els.get(_subclass_uri, null);

            if (subclasses !is null)
            {
                if (subclasses.get(_class_uri, false) == true)
                {
                    return true;
                }
            }
        }

        return false;
    }

    public void load()
    {
        StopWatch sw1;

        sw1.start();

        reload_count++;
        if (trace_msg[ 20 ] == 1)
            log.trace_log_and_console("[%s] load onto..", context.get_name);

        context.reopen_ro_individuals_storage_db();
        context.reopen_ro_fulltext_indexer_db();

        Ticket       sticket = context.sys_ticket();

        Individual[] l_individuals = context.get_individuals_via_query(
                                                                       &sticket,
                                                                       "'rdf:type' === 'rdfs:Class' || 'rdf:type' === 'rdf:Property' || 'rdf:type' === 'owl:Class' || 'rdf:type' === 'owl:ObjectProperty' || 'rdf:type' === 'owl:DatatypeProperty'",
                                                                       OptAuthorize.NO, 10000, 10000);

        sw1.stop();

        log.trace_log_and_console("[%s] load onto, count individuals: %d, time=%d µs", context.get_name, l_individuals.length, sw1.peek().usecs);


        foreach (indv; l_individuals)
            individuals[ indv.uri ] = indv;

        sw1.reset();
        sw1.start();

        foreach (indv; l_individuals)
            update_onto_hierarchy(indv);

        sw1.stop();

        //if (trace_msg[ 20 ] == 1)
        log.trace_log_and_console("[%s] update hierarhy in mem, time=%d µs", context.get_name, sw1.peek().usecs);

        //log.trace ("LOAD *** class *** \n%s", _class.toString());
        //log.trace ("LOAD *** property *** \n%s", _property.toString());
    }

    public void update_onto_hierarchy(ref Individual indv, bool replace = false)
    {
        string type_uri = indv.uri;
        Names  icl;
        bool   is_class = false;
        bool   is_prop  = false;

        bool   is_deleted = indv.exists("v-s:deleted", true);

        if (is_deleted)
        {
            individuals.remove(indv.uri);

            foreach (indv; individuals)
                update_onto_hierarchy(indv);
            return;
        }

        if (indv.anyExists("rdf:type", [ "rdf:Property", "owl:ObjectProperty", "owl:DatatypeProperty" ]))
        {
            if (replace == true)
                individuals[ indv.uri ] = indv;
            else
                icl = _property.el_2_super_els.get(type_uri, null);

            if (icl is null)
                _update_element(type_uri, _property, "rdfs:subPropertyOf");

            _property.els[ indv.uri ] = true;

            is_prop = true;
        }
        else if (indv.anyExists("rdf:type", [ "owl:Class", "rdfs:Class" ]))
        {
            if (replace == true)
                individuals[ indv.uri ] = indv;
            else
                icl = _class.el_2_super_els.get(type_uri, null);

            if (icl is null)
                _update_element(type_uri, _class, rdfs__subClassOf);

            //_class.els [indv.uri] = true;

            is_class = true;
        }

        // если этот класс числится в осиротевших ссылках, найти в подклассах где он упоминается и так-же обновить.
        if (is_class && _class.orphans.get(indv.uri, false) == true)
        {
            Names nuscs = _class.el_2_sub_els.get(indv.uri, null);

            foreach (cl; nuscs.keys)
            {
                _update_element(cl, _class, rdfs__subClassOf);
                _class.orphans[ cl ] = false;
            }

            //log.trace ("@0 need update [%s]->[%s]", indv.uri, nuscs);
        }
        else
        if (is_prop && _property.orphans.get(indv.uri, false) == true)
        {
            Names nuscs = _property.el_2_sub_els.get(indv.uri, null);

            foreach (cl; nuscs.keys)
            {
                _update_element(cl, _property, "rdfs:subPropertyOf");
                _property.orphans[ cl ] = false;
            }

            //log.trace ("@0 need update [%s]->[%s]", indv.uri, nuscs);
        }
    }

    private void _update_element(string type_uri, Bdathe elh, string parent_predicate)
    {
        // writeln ("@b1 update_element_in_hierarchy, uri=", indv.uri);
        Names superelementes = Names.init;

        prepare_superelements(parent_predicate, elh, superelementes, individuals, type_uri);
        elh.el_2_super_els[ type_uri ] = superelementes;

        foreach (elementz; superelementes.keys)
        {
            if (individuals.get(elementz, Individual.init) == Individual.init)
            {
                elh.orphans[ elementz ] = true;
                elh.els.remove(elementz);
            }

            Names subelementes = elh.el_2_sub_els.get(elementz, Names.init);
            subelementes[ type_uri ]     = true;
            elh.el_2_sub_els[ elementz ] = subelementes;
        }
    }

    private void prepare_superelements(string parent_predicate, Bdathe elh,
                                       ref Names superelementes, ref Individual[ string ] elementes, string look_cl,
                                       int level = 0)
    {
        //log.trace ("#1 prepare_superelementes=%s", look_cl);

        Individual ii = elementes.get(look_cl, Individual.init);

        Resource[] list = ii.getResources(parent_predicate);
        foreach (elementz; list)
        {
            superelementes[ elementz.uri ] = true;
            if (elementz.uri != look_cl)
	            prepare_superelements(parent_predicate, elh, superelementes, elementes, elementz.uri, level + 1);
        }
    }
}

