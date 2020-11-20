use crate::Context;
use rusty_v8 as v8;
use rusty_v8::ContextScope;
use std::sync::Mutex;
use v_api::app::ResultCode;
use v_ft_xapian::xapian_reader::XapianReader;
use v_module::module::Module;
use v_onto::individual::Individual;
use v_search::common::FTQuery;
use v_v8::callback::*;
use v_v8::common::*;
use v_v8::scripts_workplace::ScriptsWorkPlace;
use v_v8::session_cache::CallbackSharedData;

lazy_static! {
    static ref INIT_LOCK: Mutex<u32> = Mutex::new(0);
}

#[must_use]
pub struct SetupGuard {}

impl Drop for SetupGuard {
    fn drop(&mut self) {
        // TODO shutdown process cleanly.
    }
}

pub fn is_exportable(_module: &mut Module, ctx: &mut Context, _prev_state_indv: &mut Individual, new_state_indv: &mut Individual) -> Option<String> {
    let rdf_types = new_state_indv.get_literals("rdf:type").unwrap_or_default();

    for script_id in ctx.workplace.scripts_order.iter() {
        let mut session_data = CallbackSharedData::default();
        session_data.g_key2indv.insert("$document".to_owned(), Individual::new_from_obj(new_state_indv.get_obj()));
        session_data.g_key2attr.insert("$ticket".to_owned(), ctx.sys_ticket.to_owned());

        if let Some(script) = ctx.workplace.scripts.get(script_id) {
            if let Some(compiled_script) = script.compiled_script {
                if !is_filter_pass(script, new_state_indv.get_id(), &rdf_types, &mut ctx.onto) {
                    debug!("skip (filter) script:{}", script_id);
                    continue;
                }

                let mut sh_g_vars = G_VARS.lock().unwrap();
                let g_vars = sh_g_vars.get_mut();
                *g_vars = session_data;
                drop(sh_g_vars);

                let hs = ContextScope::new(&mut ctx.workplace.scope, ctx.workplace.context);
                let mut local_scope = hs;

                if let Some(res) = compiled_script.run(&mut local_scope) {
                    if res.is_string() {
                        if let Some(s) = res.to_string(local_scope.as_mut()) {
                            return Some(s.to_rust_string_lossy(&mut local_scope));
                        }
                    }
                }
            }
        }
    }
    None
}

//pub fn get_script_identity(id: &str, text: &str) -> String {
//    format!("{}+{}", id, text)
//}

pub(crate) fn load_exim_filter_scripts(wp: &mut ScriptsWorkPlace<ScriptInfoContext>, xr: &mut XapianReader) {
    let res = xr.query(FTQuery::new_with_user("cfg:VedaSystem", "'rdf:type' === 'bpmn:ExternalTaskHandler'"), &mut wp.module.storage);

    if res.result_code == ResultCode::Ok && res.count > 0 {
        for id in &res.result {
            if let Some(ev_indv) = wp.module.get_individual(id, &mut Individual::default()) {
                prepare_script(wp, ev_indv);
            }
        }
    }
    info!("load scripts from db: {:?}", wp.scripts_order);
}

pub(crate) fn prepare_script(wp: &mut ScriptsWorkPlace<ScriptInfoContext>, ev_indv: &mut Individual) {
    if ev_indv.is_exists_bool("v-s:deleted", true) || ev_indv.is_exists_bool("v-s:disabled", true) {
        info!("disable script {}", ev_indv.get_id());
        if let Some(scr_inf) = wp.scripts.get_mut(ev_indv.get_id()) {
            scr_inf.compiled_script = None;
        }
        return;
    }

    if let Some(script_text) = ev_indv.get_first_literal("bpmn:script") {
        let str_script = "\
      (function () { \
        try { \
          var ticket = get_env_str_var ('$ticket'); \
          var task = JSON.parse(get_env_str_var ('$task')); \
          "
        .to_owned()
            + &script_text
            + " \
         } catch (e) { log_trace (e); } \
      })();";

        let topic = ev_indv.get_first_literal("bpmn:triggerByTopic").unwrap_or_default();

        let mut scr_inf: ScriptInfo<ScriptInfoContext> = ScriptInfo::new_with_src(&topic, &str_script);

        wp.add_to_order(&scr_inf);

        let scope = &mut v8::ContextScope::new(&mut wp.scope, wp.context);
        scr_inf.compile_script(ev_indv.get_id(), scope);
        wp.scripts.insert(scr_inf.id.to_string(), scr_inf);
    } else {
        error!("v-s:script no found");
    }
}
