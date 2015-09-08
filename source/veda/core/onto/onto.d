/**
 * кэш из индивидов относящихся к онтологии
 */

module onto.onto;

// TODO сделать перезагрузку онтологии только в случае ее изменения (проверять CRC?)

private
{
    import std.stdio, std.datetime, std.conv, std.concurrency, std.exception : assumeUnique;
    import onto.resource, onto.individual;
    import util.utils, util.container, util.logger;
    import veda.core.know_predicates, veda.core.context, veda.core.interthread_signals, veda.core.log_msg;
    import search.vql;
}

logger log;

static this()
{
    log = new logger("pacahon", "log", "onto");
}

alias bool[ string ] OfSubClasses;

class Onto
{
    private Context context;
    public int      reload_count = 0;

    private         Individual[ string ] individuals;
    private         OfSubClasses[ string ] ofClass;

    public this(Context _context)
    {
        //interthread_signal_id = "onto";
        context = _context;
    }

    Individual[ string ] get_individuals()
    {
        //writeln ("@$1");

        return individuals;
    }

    public bool isSubClass(string _class_uri, string _subclass_uri)
    {
        OfSubClasses subclasses = ofClass.get(_class_uri, null);

        if (subclasses !is null)
        {
            return subclasses.get(_subclass_uri, false);
        }
        return false;
    }

    public void load()
    {
        if (log is null)
            log = new logger("pacahon", "log", "onto");

        reload_count++;
        Individual[] l_individuals;

        //if (trace_msg[ 20 ] == 1)
        log.trace_log_and_console("[%s] load onto to context..", context.get_name);

//        if (context.getTid(P_MODULE.acl_manager) != Tid.init)
//            context.wait_thread(P_MODULE.acl_manager);

        context.reopen_ro_subject_storage_db();
        context.reopen_ro_fulltext_indexer_db();

        context.vql().get(
                          null,
                          "return { '*'}
            filter { 'rdf:type' == 'rdfs:Class' || 'rdf:type' == 'rdf:Property' || 'rdf:type' == 'owl:Class' || 'rdf:type' == 'owl:ObjectProperty' || 'rdf:type' == 'owl:DatatypeProperty' }",
                          l_individuals);

        //if (trace_msg[ 20 ] == 1)
        log.trace_log_and_console("[%s] count individuals: %d", context.get_name, l_individuals.length);

        foreach (indv; l_individuals)
        {
            individuals[ indv.uri ] = indv;
            if (indv.anyExist("rdf:type", [ "owl:Class", "rdfs:Class" ]))
            {
                string       type_uri = indv.uri;
//				writeln ("# class=", type_uri);
                OfSubClasses icl = ofClass.get(type_uri, null);
                if (icl is null)
                {
                    OfSubClasses sc = OfSubClasses.init;
                    ofClass[ type_uri ] = sc;
                    prepare_subclasses(sc, individuals, type_uri);
//                      writeln ("# subClasses for class ", type_uri, ", = ", sc.data);
                }
            }
        }

        if (trace_msg[ 20 ] == 1)
            log.trace_log_and_console("[%s] load onto to graph..Ok", context.get_name);
    }

    private void prepare_subclasses(ref OfSubClasses subclasses, ref Individual[ string ] classes, string look_cl, int level = 0)
    {
        Individual ii = classes.get(look_cl, Individual.init);

        Resource[] list_subClassOf = ii.getResources(rdfs__subClassOf);
        foreach (subClassOf; list_subClassOf)
        {
            subclasses[ subClassOf.uri ] = true;
            prepare_subclasses(subclasses, classes, subClassOf.uri, level + 1);
        }
    }
}
