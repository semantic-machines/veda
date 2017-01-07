/**
 * кэш из индивидов относящихся к онтологии
 */

module veda.onto.onto;

// TODO сделать перезагрузку онтологии только в случае ее изменения (проверять CRC?)

private
{
    import std.stdio, std.datetime, std.conv, std.concurrency, std.exception : assumeUnique;
    import veda.onto.resource, veda.onto.individual;
    import veda.core.util.utils, veda.util.container, veda.common.logger;
    import veda.core.common.know_predicates, veda.core.common.context, veda.core.common.log_msg, veda.core.common.define;
}

alias bool[ string ] Names;

class Onto
{
    private Context context;
    Logger          log;
    public int      reload_count = 0;

    private         Individual[ string ] individuals;

    private         Names[ string ] class2superclasses;
    private         Names[ string ] class2subclasses;

    private         Names[ string ] property2subproperties;
    private         Names[ string ] property2superproperties;

    private         bool[ string ]    orphans;

    public this(Context _context)
    {
        context = _context;
        log     = context.get_logger();
    }

    Individual[ string ] get_individuals()
    {
        return individuals;
    }

    public Names get_super_classes(string _class_uri)
    {
        return class2superclasses.get(_class_uri, null);
    }

    public Names get_sub_classes(string _class_uri)
    {
        return class2subclasses.get(_class_uri, null);
    }

    public Names get_sub_properties(string _property_uri)
    {
        return property2subproperties.get(_property_uri, null);
    }

    public bool isSubClasses(string _class_uri, string[] _subclasses_uri)
    {
        foreach (_subclass_uri; _subclasses_uri)
        {
            Names subclasses = class2subclasses.get(_subclass_uri, null);

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
        reload_count++;
        if (trace_msg[ 20 ] == 1)
            log.trace_log_and_console("[%s] load onto..", context.get_name);

        context.reopen_ro_subject_storage_db();
        context.reopen_ro_fulltext_indexer_db();

        Ticket       sticket = context.sys_ticket();

        Individual[] l_individuals = context.get_individuals_via_query(
                                                                       &sticket,
                                                                       "'rdf:type' === 'rdfs:Class' || 'rdf:type' === 'rdf:Property' || 'rdf:type' === 'owl:Class' || 'rdf:type' === 'owl:ObjectProperty' || 'rdf:type' === 'owl:DatatypeProperty'",
                                                                       true, 10000, 10000);

        log.trace_log_and_console("[%s] load onto, count individuals: %d", context.get_name, l_individuals.length);

        foreach (indv; l_individuals)
            individuals[ indv.uri ] = indv;

        foreach (indv; l_individuals)
            update_onto_hierarchy(indv);

        //foreach (key, value; class2subclasses)
        //{
        //    writeln("@ class=", key, ", subclasses=", value);
        //}

        if (trace_msg[ 20 ] == 1)
            log.trace_log_and_console("[%s] load onto..Ok", context.get_name);
    }

    public void update_onto_hierarchy(ref Individual indv, bool replace = false)
    {
        //log.trace ("@1#update_onto_hierarchy[%s] replace=%s", indv.uri, text (replace));
        bool is_class    = false;
        bool is_property = false;

        if (indv.anyExists("rdf:type", [ "rdf:Property", "owl:ObjectProperty", "owl:DatatypeProperty" ]))
            is_property = true;

        if (indv.anyExists("rdf:type", [ "owl:Class", "rdfs:Class" ]))
            is_class = true;

        if (replace == true && is_property)
            individuals[ indv.uri ] = indv;

        if (is_class || is_property)
        {
            if (replace == true)
                individuals[ indv.uri ] = indv;

            string type_uri = indv.uri;

            Names  icl;

            if (replace == false)
            {
                if (is_class)
                {
                    icl = class2superclasses.get(type_uri, null);

                    if (icl is null)
                        _update_element(type_uri, class2superclasses, class2subclasses, rdfs__subClassOf);
                }

                if (is_property)
                {
                    icl = property2subproperties.get(type_uri, null);

//		            if (icl is null)
//		                _update_property(type_uri);
                }
            }
        }


        // если этот класс числится в осиротевших ссылках, найти в подклассах где он упоминается и так-же обновить.
        if (orphans.get(indv.uri, false) == true)
        {
            Names nuscs = class2subclasses.get(indv.uri, null);

            foreach (cl; nuscs.keys)
            {
                if (is_class)
                    _update_element(cl, class2superclasses, class2subclasses, rdfs__subClassOf);

                //if (is_property)
                //      _update_property(cl);

                orphans[ cl ] = false;
            }

            //log.trace ("@0 need update [%s]->[%s]", indv.uri, nuscs);
        }
    }

    private void _update_element(string type_uri, ref Names[ string ] element2superelementes, ref Names[ string ] element2subelementes,
                                 string parent_predicate)
    {
        Names superelementes = Names.init;

        prepare_superelementes(parent_predicate, element2superelementes, element2subelementes, superelementes, individuals, type_uri);
        element2superelementes[ type_uri ] = superelementes;

        foreach (el; superelementes.keys)
        {
            if (individuals.get(el, Individual.init) == Individual.init)
                orphans[ el ] = true;

            Names subelementes = element2subelementes.get(el, Names.init);
            subelementes[ type_uri ]   = true;
            element2subelementes[ el ] = subelementes;
        }
    }

    private void prepare_superelementes(string parent_predicate, ref Names[ string ] element2superelementes, ref Names[ string ] element2subelementes,
                                        ref Names superelementes, ref Individual[ string ] elementes, string look_cl,
                                        int level = 0)
    {
        //log.trace ("#1 prepare_superelementes=%s", look_cl);

        Individual ii = elementes.get(look_cl, Individual.init);

        Resource[] list = ii.getResources(parent_predicate);
        foreach (elementz; list)
        {
            superelementes[ elementz.uri ] = true;
            prepare_superelementes(parent_predicate, element2superelementes, element2subelementes, superelementes, elementes, elementz.uri, level + 1);
        }
    }
}
