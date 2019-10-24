module veda.core.impl.app_context_creator;

private
{
    import std.stdio;
    import veda.util.properd;
    import veda.core.impl.thread_context, veda.core.common.context, veda.common.logger;
    import veda.storage.lmdb.lmdb_storage, veda.storage.tarantool.tarantool_storage;
}

public static Context create_new_ctx(string context_name, Logger _log, string _main_module_url = null)
{
    PThreadContext ctx = new PThreadContext();

    ctx.node_id = "cfg:standart_node";
    ctx.log     = _log;

    if (ctx.log is null)
        writefln("context_name [%s] log is null", context_name);

    string tarantool_url;

    if (_main_module_url !is null)
        ctx.main_module_url = _main_module_url;
    else
    {
        try
        {
            string[ string ] properties = readProperties("./veda.properties");
            tarantool_url               = properties.as!(string)("tarantool_url");
            ctx.main_module_url         = properties.as!(string)("main_module_url") ~ "\0";
        }
        catch (Throwable ex)
        {
            ctx.log.trace("ERR! unable read ./veda.properties");
            return null;
        }
    }

    if (tarantool_url !is null)
    {
        ctx.storage = new TarantoolStorage(context_name, ctx.log);
    }
    else
    {
        ctx.storage = new LmdbStorage(context_name, ctx.log);
    }

    ctx.name = context_name;

    ctx.get_configuration();

    ctx.log.trace_log_and_console("NEW CONTEXT [%s]", context_name);

    return ctx;
}

