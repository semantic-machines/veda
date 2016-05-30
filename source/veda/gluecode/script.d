module veda.gluecode.script;

import std.stdio, std.path;
import veda.gluecode.v8d_header, veda.core.common.context, veda.onto.individual, veda.core.log_msg, veda.core.common.know_predicates,
       veda.onto.resource;

struct ScriptInfo
{
    string id;
    string str_script;
    bool[ string ] filters;
    Script compiled_script;
}

void prepare_script(ref ScriptInfo[ string ] scripts, Individual ss, ScriptVM script_vm, string vars_env)
{
    if (trace_msg[ 310 ] == 1)
        log.trace("prepare_script uri=%s", ss.uri);

    try
    {
        if (ss.isExists(veda_schema__deleted, true) || ss.isExists("v-s:disabled", true))
        {
            //writeln ("@S0 SCRIPT OFF, uri=", ss.uri);
            ScriptInfo script = scripts.get(ss.uri, ScriptInfo.init);

            if (script !is ScriptInfo.init)
            {
                script.compiled_script = null;
                scripts[ ss.uri ]      = script;
            }
            return;
        }

        string scripts_text = ss.getFirstResource(veda_schema__script).literal;
        if (scripts_text.length <= 0)
            return;

        string str_script =
            "var ticket = get_env_str_var ('$ticket');"
            ~ "var document = get_individual (ticket, '$document');"
            ~ "if (document) {"
            ~ "var _script_id = '" ~ ss.uri ~ "';"
            ~ vars_env
            ~ "script();"
            ~ "};"
            ~ "function script() {" ~ scripts_text ~ "};"
        ;
        try
        {
            ScriptInfo script = ScriptInfo.init;
            script.id         = ss.uri;
            script.str_script = str_script;

            script.compiled_script = script_vm.compile(script.str_script);
            if (trace_msg[ 310 ] == 1)
                log.trace("#compile event script.id=%s, text=%s", script.id, script.str_script);

            //writeln("scripts_text:", scripts_text);

            Resources filters = ss.getResources(veda_schema__filter);

            foreach (filter; filters)
            {
                script.filters[ filter.uri ] = true;
            }

            scripts[ ss.uri ] = script;
        }
        catch (Exception ex)
        {
            log.trace_log_and_console("error:compile event script :%s", ex.msg);
        }
    }
    catch (Exception ex)
    {
        log.trace_log_and_console("error:load event script :%s", ex.msg);
    }
    finally
    {
    }
}
