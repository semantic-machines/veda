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

// ////// Logger ///////////////////////////////////////////
import veda.common.logger;
Logger _log;
Logger log()
{
    if (_log is null)
        _log = new Logger("veda-core-" ~ process_name, "log", "ONTO");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

alias bool[ string ] Classes;

class Onto
{
    private Context context;
    public int      reload_count = 0;

    private         Individual[ string ] individuals;
    private         Classes[ string ] class2superclasses;
    private         Classes[ string ] class2subclasses;
    private         bool[ string ]    orphans;

    public this(Context _context)
    {
        context = _context;
    }

    Individual[ string ] get_individuals()
    {
        return individuals;
    }

    public Classes get_super_classes(string _class_uri)
    {
        return class2superclasses.get(_class_uri, null);
    }

    public Classes get_sub_classes(string _class_uri)
    {
        return class2subclasses.get(_class_uri, null);
    }

    public bool isSubClasses(string _class_uri, string[] _subclasses_uri)
    {
        foreach (_subclass_uri; _subclasses_uri)
        {
            Classes subclasses = class2subclasses.get(_subclass_uri, null);

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

//        if (context.getTid(P_MODULE.acl_manager) != Tid.init)
//            context.wait_thread(P_MODULE.acl_manager);

        context.reopen_ro_subject_storage_db();
        context.reopen_ro_fulltext_indexer_db();

        Ticket       sticket = context.sys_ticket();

        Individual[] l_individuals = context.get_individuals_via_query(
                                                                       &sticket,
                                                                       "'rdf:type' === 'rdfs:Class' || 'rdf:type' === 'rdf:Property' || 'rdf:type' === 'owl:Class' || 'rdf:type' === 'owl:ObjectProperty' || 'rdf:type' === 'owl:DatatypeProperty'",
                                                                       true, 10000, 10000);

        //if (trace_msg[ 20 ] == 1)
        log.trace_log_and_console("[%s] load onto, count individuals: %d", context.get_name, l_individuals.length);

        foreach (indv; l_individuals)
        {
            individuals[ indv.uri ] = indv;
        }

        foreach (indv; l_individuals)
        {
            update_class_in_hierarchy(indv);
        }

        //foreach (key, value; class2subclasses)
        //{
        //    writeln("@ class=", key, ", subclasses=", value);
        //}

        if (trace_msg[ 20 ] == 1)
            log.trace_log_and_console("[%s] load onto..Ok", context.get_name);
    }

    public void update_class_in_hierarchy(ref Individual indv, bool replace = false)
    {
        //log.trace ("@1#update_class_in_hierarchy[%s] replace=%s", indv.uri, text (replace));

        if (replace == true && indv.anyExists("rdf:type", [ "rdf:Property", "owl:ObjectProperty", "owl:DatatypeProperty" ]))
            individuals[ indv.uri ] = indv;

        if (indv.anyExists("rdf:type", [ "owl:Class", "rdfs:Class" ]))
        {
            if (replace == true)
                individuals[ indv.uri ] = indv;

            string  type_uri = indv.uri;

            Classes icl;

            if (replace == false)
                icl = class2superclasses.get(type_uri, null);

            if (icl is null)
                _update_class(type_uri);
        }

        // если этот класс числится в осиротевших ссылках, найти в подклассах где он упоминается и так-же обновить.
        if (orphans.get(indv.uri, false) == true)
        {
            Classes nuscs = class2subclasses.get(indv.uri, null);

            foreach (cl; nuscs.keys)
            {
                _update_class(cl);
                orphans[ cl ] = false;
            }

            //log.trace ("@0 need update [%s]->[%s]", indv.uri, nuscs);
        }
    }

    private void _update_class(string type_uri)
    {
        //                  writeln ("@b1 update_class_in_hierarchy, uri=", indv.uri);
        Classes superclasses = Classes.init;

        prepare_superclasses(superclasses, individuals, type_uri);
        class2superclasses[ type_uri ] = superclasses;

        foreach (classz; superclasses.keys)
        {
            if (individuals.get(classz, Individual.init) == Individual.init)
                orphans[ classz ] = true;

            Classes subclasses = class2subclasses.get(classz, Classes.init);
            subclasses[ type_uri ]     = true;
            class2subclasses[ classz ] = subclasses;
        }
    }

    private void prepare_superclasses(ref Classes superclasses, ref Individual[ string ] classes, string look_cl, int level = 0)
    {
        //log.trace ("#1 prepare_superclasses=%s", look_cl);

        Individual ii = classes.get(look_cl, Individual.init);

        Resource[] list = ii.getResources(rdfs__subClassOf);
        foreach (classz; list)
        {
            superclasses[ classz.uri ] = true;
            prepare_superclasses(superclasses, classes, classz.uri, level + 1);
        }
    }
}
