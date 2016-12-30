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

void prepare_script(ref ScriptInfo[ string ] scripts, ref Array!string event_scripts_order, Individual ss, ScriptVM script_vm, string vars_env)
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
            "try { var ticket = get_env_str_var ('$ticket');"
            ~ "var document = get_individual (ticket, '$document');"
            ~ "if (document) {"
            ~ "var _script_id = '" ~ ss.uri ~ "';"
            ~ vars_env
            ~ "script();"
            ~ "};"
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

            script.compiled_script = script_vm.compile(script.str_script);
            if (trace_msg[ 310 ] == 1)
                log.trace("#compile event script.id=%s, text=%s", script.id, script.str_script);

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

            scripts[ ss.uri ] = script;


            // удалить вхождение
            auto rr = event_scripts_order[].find(ss.uri);

            if (rr.length > 0)
                event_scripts_order.linearRemove(rr);

            // найти новую позицию с учетом зависимостей
            long max_pos = 0;

            long idx                = 0;
            bool need_before_insert = false;

            foreach (oo; event_scripts_order)
            {
                // проверить зависит ли [oo] от [script]
                auto soo = scripts[ oo ];
                foreach (dp; soo.dependency.keys)
                {
                    if (script.id == dp)
                    {
                        need_before_insert = true;
                        max_pos            = idx;
                    }
                }

                if (need_before_insert == true)
                    break;

                // проверить зависит ли [script] от [oo]
                foreach (dp; script.dependency.keys)
                {
                    if (oo == dp && max_pos < idx)
                        max_pos = idx;
                }
                idx++;
            }
            if (max_pos > 0)
            {
                rr = event_scripts_order[].find(event_scripts_order[ max_pos ]);

                // добавить новое вхождение
                if (rr.length > 0)
                {
                    if (need_before_insert)
                        event_scripts_order.insertBefore(rr, ss.uri);
                    else
                        event_scripts_order.insertAfter(rr, ss.uri);
                }
                else
                    event_scripts_order.insertBack(ss.uri);
            }
            else
                event_scripts_order.insertBack(ss.uri);
        }
        catch (Exception ex)
        {
            log.trace("error:compile event script :%s", ex.msg);
        }
    }
    catch (Exception ex)
    {
        log.trace("error:load event script :%s", ex.msg);
    }
    finally
    {
    }
}
