/**
 * indexer property
 */

module veda.core.search.indexer_property;

private import std.conv, std.stdio;
private import veda.core.common.context, veda.core.common.log_msg;
private import veda.onto.resource, veda.onto.lang, veda.onto.individual, veda.core.common.define, veda.common.type;
private import veda.common.logger;

class IndexerProperty
{
    private Context context;

    private         Individual[ string ] class_property__2__indiviual;
    private         string[ string ] class__2__database;
    private         Individual[ string ] uri__2__indiviual;
    private         bool[ string ]  database__2__true;
    private Logger  log;

    this(Context _context)
    {
        context = _context;
        log     = context.get_logger();
    }

    bool[ string ] get_dbnames()
    {
        return database__2__true.dup;
    }

    string get_dbname_of_class(string uri)
    {
        return class__2__database.get(uri, "base");
    }

    Individual get_index(string uri)
    {
        return uri__2__indiviual.get(uri, Individual.init);
    }

    Individual get_index(string uri, string predicate)
    {
        return class_property__2__indiviual.get(uri ~ predicate, Individual.init);
    }

    Individual get_index_of_property(string predicate)
    {
        return class_property__2__indiviual.get(predicate, Individual.init);
    }

    void add_schema_data(Individual indv)
    {
        uri__2__indiviual[ indv.uri ] = indv;
        Resources forClasses    = indv.resources.get("vdi:forClass", Resources.init);
        Resources forProperties = indv.resources.get("vdi:forProperty", Resources.init);

        Resources indexed_to = indv.resources.get("vdi:indexed_to", Resources.init);

        if (forClasses.length == 0)
            forClasses ~= Resource.init;

        if (forProperties.length == 0)
            forProperties ~= Resource.init;

        foreach (forClass; forClasses)
        {
            if (indexed_to.length > 0)
            {
//                      writeln ("@1 indexed_as_system=", indexed_as_system, ", indexed_as_system[0]=", indexed_as_system[0]);
                class__2__database[ forClass.uri ]              = indexed_to[ 0 ].get!string;
                database__2__true[ indexed_to[ 0 ].get!string ] = true;
            }

            foreach (forProperty; forProperties)
            {
                string key = forClass.uri ~ forProperty.uri;
                class_property__2__indiviual[ key ] = indv;

                if (trace_msg[ 214 ] == 1)
                    log.trace("search indexes, key=%s, uri=%s", key, indv.uri);
            }
        }
    }

    void load(bool force = false)
    {
        if (class_property__2__indiviual.length == 0 || force)
        {
        	log.trace("load indexes:start");
            context.reopen_ro_individuals_storage_db();
            context.reopen_ro_fulltext_indexer_db();

//            context.vql().reopen_db();
            Ticket       sticket = context.sys_ticket();

            Individual[] l_individuals = context.get_individuals_via_query(&sticket, "'rdf:type' === 'vdi:ClassIndex'", OptAuthorize.NO, 10000, 10000);

            foreach (indv; l_individuals)
            {
                add_schema_data(indv);
            }
            database__2__true[ "base" ] = true;

            if (l_individuals.length > 0)
                log.trace("load indexes: class_property__2__indiviual=%s", class_property__2__indiviual);
        }
    }


    override public string toString()
    {
        return text(class__2__database);
    }
}

