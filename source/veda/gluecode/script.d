module veda.gluecode.script;

import std.stdio, std.path, std.container.array, std.algorithm, std.conv, std.range;
import veda.gluecode.v8d_header, veda.core.common.context, veda.onto.individual, veda.core.common.log_msg, veda.core.common.know_predicates,
       veda.onto.resource;

struct ScriptInfo
{
    string id;
    string str_script;
    bool[ string ] trigger_by_type;
    bool[ string ] trigger_by_uid;
    bool[ string ] dependency;
    Script compiled_script;
    string run_at;
}

class ScriptsWorkPlace
{
    public          ScriptInfo[ string ] scripts;
    public string[] scripts_order;
}

void prepare_script(ScriptsWorkPlace wpl, Individual ss, ScriptVM script_vm, string first_section,
                    string before_vars,
                    string vars_env, string after_vars,
                    bool trace)
{
    //if (trace)
    //log.trace("prepare_script uri=%s, scripts_order.length=%d", ss.uri, wpl.scripts_order.length);

    try
    {
        if (ss.exists(veda_schema__deleted, true) || ss.exists("v-s:disabled", true))
        {
            log.trace("disable script %s", ss.uri);
            ScriptInfo script = wpl.scripts.get(ss.uri, ScriptInfo.init);

            if (script !is ScriptInfo.init)
            {
                script.compiled_script = null;
                wpl.scripts[ ss.uri ]  = script;
            }
            return;
        }

        string scripts_text = ss.getFirstResource(veda_schema__script).literal;
        if (scripts_text.length <= 0)
        {
            log.trace("script %s empty, skip", ss.uri);
            return;
        }

        if (trace)
            log.trace("script_text=%s", scripts_text);

        string str_script = first_section
                            ~ "try { var ticket = get_env_str_var ('$ticket');"
                            ~ before_vars
                            ~ "var _script_id = '" ~ ss.uri ~ "';"
                            ~ vars_env
                            ~ "script();"
                            ~ after_vars
                            ~ "function script() {" ~ scripts_text ~ "}; } catch (e) { log_trace (e); }"
        ;
        try
        {
            ScriptInfo script = ScriptInfo.init;
            script.id         = ss.uri;
            script.str_script = str_script;

            script.run_at = ss.getFirstLiteral("v-s:runAt");

            if (script.run_at is null)
                script.run_at = "main";

            if (script.run_at != g_vm_id)
                return;

            if (trace)
                log.trace("compile script.id=%s, text=%s", script.id, script.str_script);

            script.compiled_script = script_vm.compile(script.str_script);

            //writeln("scripts_text:", scripts_text);

            Resources trigger_by_type = ss.getResources("v-s:triggerByType");

            foreach (filter; trigger_by_type)
                script.trigger_by_type[ filter.uri ] = true;

            Resources trigger_by_uid = ss.getResources("v-s:triggerByUid");

            foreach (filter; trigger_by_uid)
                script.trigger_by_uid[ filter.uri ] = true;

            Resources dependency = ss.getResources("v-s:dependency");
            foreach (dp; dependency)
                script.dependency[ dp.uri ] = true;

            wpl.scripts[ ss.uri ] = script;

            int      count_find_dependency = 0;
            bool     inserted              = false;

            string[] new_scripts_order;

            foreach (oo; wpl.scripts_order)
            {
                if (count_find_dependency < script.dependency.length)
                {
                    auto soo = wpl.scripts[ oo ];
                    foreach (dp; soo.dependency.keys)
                    {
                        if (script.id == dp)
                            count_find_dependency++;
                    }
                }

                if (inserted == false && count_find_dependency >= script.dependency.length)
                {
                    new_scripts_order ~= script.id;
                    inserted = true;
                }

                if (oo != script.id)
                    new_scripts_order ~= oo;
            }
            if (inserted == false)
                new_scripts_order ~= script.id;

            wpl.scripts_order = new_scripts_order;
        }
        catch (Exception ex)
        {
            log.trace("error:compile event script :%s", ex.msg);
        }

//        log.trace("scripts_order.length=%d", wpl.scripts_order.length);
    }
    catch (Exception ex)
    {
        log.trace("error:load event script :%s", ex.msg);
    }
}
