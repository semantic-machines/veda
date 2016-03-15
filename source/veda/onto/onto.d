/**
 * кэш из индивидов относящихся к онтологии
 */

module veda.onto.onto;

// TODO сделать перезагрузку онтологии только в случае ее изменения (проверять CRC?)

private
{
    import std.stdio, std.datetime, std.conv, std.concurrency, std.exception : assumeUnique;
    import veda.onto.resource, veda.onto.individual;
    import util.utils, veda.util.container, util.logger;
    import veda.core.know_predicates, veda.core.context, veda.core.log_msg, veda.core.define;
    import search.vql;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "ONTO");
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
        Individual[] l_individuals;

        //if (trace_msg[ 20 ] == 1)
        log.trace_log_and_console("[%s] load onto to context..", context.get_name);

//        if (context.getTid(P_MODULE.acl_manager) != Tid.init)
//            context.wait_thread(P_MODULE.acl_manager);

        context.reopen_ro_subject_storage_db();
        context.reopen_ro_fulltext_indexer_db();

        Ticket sticket = context.sys_ticket();

        context.vql().get(
                          &sticket,
                          "return { '*'}
            filter { 'rdf:type' === 'rdfs:Class' || 'rdf:type' === 'rdf:Property' || 'rdf:type' === 'owl:Class' || 'rdf:type' === 'owl:ObjectProperty' || 'rdf:type' === 'owl:DatatypeProperty' }",
                          l_individuals);

        //if (trace_msg[ 20 ] == 1)
        log.trace_log_and_console("[%s] count individuals: %d", context.get_name, l_individuals.length);

        foreach (indv; l_individuals)
        {
            individuals[ indv.uri ] = indv;
        }

        foreach (indv; l_individuals)
        {
            if (indv.anyExists("rdf:type", [ "owl:Class", "rdfs:Class" ]))
            {
                string  type_uri = indv.uri;

                Classes icl = class2superclasses.get(type_uri, null);
                if (icl is null)
                {
                    Classes superclasses = Classes.init;
                    prepare_superclasses(superclasses, individuals, type_uri);
                    class2superclasses[ type_uri ] = superclasses;

                    foreach (classz; superclasses.keys)
                    {
                        Classes subclasses = class2subclasses.get(classz, Classes.init);
                        subclasses[ type_uri ]     = true;
                        class2subclasses[ classz ] = subclasses;
                    }
                }
            }
        }

        //foreach (key, value; class2subclasses)
        //{
        //    writeln("@ class=", key, ", subclasses=", value);
        //}

        if (trace_msg[ 20 ] == 1)
            log.trace_log_and_console("[%s] load onto to graph..Ok", context.get_name);
    }

    private void prepare_superclasses(ref Classes superclasses, ref Individual[ string ] classes, string look_cl, int level = 0)
    {
        Individual ii = classes.get(look_cl, Individual.init);

        Resource[] list = ii.getResources(rdfs__subClassOf);
        foreach (classz; list)
        {
            superclasses[ classz.uri ] = true;
            prepare_superclasses(superclasses, classes, classz.uri, level + 1);
        }
    }
}
