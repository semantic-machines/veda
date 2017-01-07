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

    private         Names[ string ] _class2superclasses;
    private         Names[ string ] _class2subclasses;

    private         Names[ string ] _prop2superprops;
    private         Names[ string ] _prop2subprops;

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
        return _class2superclasses.get(_class_uri, null);
    }

    public Names get_sub_classes(string _class_uri)
    {
        return _class2subclasses.get(_class_uri, null);
    }

    public Names get_sub_properies(string _uri)
    {
        return _prop2subprops.get(_uri, null);
    }

    public bool isSubClasses(string _class_uri, string[] _subclasses_uri)
    {
        foreach (_subclass_uri; _subclasses_uri)
        {
            Names subclasses = _class2subclasses.get(_subclass_uri, null);

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
        //log.trace ("@1#update_class_in_hierarchy[%s] replace=%s", indv.uri, text (replace));
        string type_uri = indv.uri;
        Names  icl;
        bool   is_class = false;
        bool   is_prop  = false;

        if (indv.anyExists("rdf:type", [ "rdf:Property", "owl:ObjectProperty", "owl:DatatypeProperty" ]))
        {
            if (replace == true)
                individuals[ indv.uri ] = indv;
            else
                icl = _prop2superprops.get(type_uri, null);

            if (icl is null)
                _update_element(type_uri, _prop2superprops, _prop2subprops, "rdfs:subPropertyOf");
            is_prop = true;
        }
        else if (indv.anyExists("rdf:type", [ "owl:Class", "rdfs:Class" ]))
        {
            if (replace == true)
                individuals[ indv.uri ] = indv;
            else
                icl = _class2superclasses.get(type_uri, null);

            if (icl is null)
                _update_element(type_uri, _class2superclasses, _class2subclasses, rdfs__subClassOf);

            is_class = true;
        }

        // если этот класс числится в осиротевших ссылках, найти в подклассах где он упоминается и так-же обновить.
        if ((is_class || is_prop) && orphans.get(indv.uri, false) == true)
        {
            Names nuscs = _class2subclasses.get(indv.uri, null);

            foreach (cl; nuscs.keys)
            {
                if (is_class)
                    _update_element(cl, _class2superclasses, _class2subclasses, rdfs__subClassOf);
                else if (is_prop)
                    _update_element(cl, _prop2superprops, _prop2subprops, "rdfs:subPropertyOf");

                orphans[ cl ] = false;
            }

            //log.trace ("@0 need update [%s]->[%s]", indv.uri, nuscs);
        }
    }

    private void _update_element(string type_uri, ref Names[ string ] element2superelementes, ref Names[ string ] element2subelementes,
                                 string parent_predicate)
    {
        // writeln ("@b1 update_element_in_hierarchy, uri=", indv.uri);
        Names superelementes = Names.init;

        prepare_superelements(parent_predicate, element2superelementes, element2subelementes, superelementes, individuals, type_uri);
        element2superelementes[ type_uri ] = superelementes;

        foreach (elementz; superelementes.keys)
        {
            if (individuals.get(elementz, Individual.init) == Individual.init)
                orphans[ elementz ] = true;

            Names subelementes = element2subelementes.get(elementz, Names.init);
            subelementes[ type_uri ]         = true;
            element2subelementes[ elementz ] = subelementes;
        }
    }

    private void prepare_superelements(string parent_predicate, ref Names[ string ] element2superelementes, ref Names[ string ] element2subelementes,
                                       ref Names superelementes, ref Individual[ string ] elementes, string look_cl,
                                       int level = 0)
    {
        //log.trace ("#1 prepare_superelementes=%s", look_cl);

        Individual ii = elementes.get(look_cl, Individual.init);

        Resource[] list = ii.getResources(parent_predicate);
        foreach (elementz; list)
        {
            superelementes[ elementz.uri ] = true;
            prepare_superelements(parent_predicate, element2superelementes, element2subelementes, superelementes, elementes, elementz.uri, level + 1);
        }
    }
}

