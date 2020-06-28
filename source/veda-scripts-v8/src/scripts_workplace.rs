use crate::common::*;
use crate::script_info::ScriptInfo;
use rusty_v8 as v8;
use rusty_v8::scope::Entered;
use rusty_v8::{Context, HandleScope, Isolate, Local, OwnedIsolate};
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use v_api::app::ResultCode;
use v_ft_xapian::xapian_reader::XapianReader;
use v_module::module::Module;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_search::common::FTQuery;

const BEFORE_VARS: &str = "var document = get_individual (ticket, '$document'); if (document) {";
const AFTER_VARS: &str = "};";
const VARS_FOR_EVENT_SCRIPT: &str = "\
var user_uri = get_env_str_var ('$user');
var parent_script_id = get_env_str_var ('$parent_script_id');
var parent_document_id = get_env_str_var ('$parent_document_id');
var prev_state = get_individual (ticket, '$prev_state');
var super_classes = get_env_str_var ('$super_classes');
var queue_elements_count = get_env_num_var ('$queue_elements_count');
var queue_elements_processed = get_env_num_var ('$queue_elements_processed');
var _event_id = '?';";

pub(crate) struct ScriptsWorkPlace<'a> {
    pub scripts: HashMap<String, ScriptInfo<'a>>,
    pub scripts_order: Vec<String>,
    pub module: Module,
    pub(crate) scope: &'a mut Entered<'a, HandleScope, OwnedIsolate>,
    pub(crate) context: Local<'a, Context>,
}

impl<'a> ScriptsWorkPlace<'a> {
    pub fn load_ext_scripts(&mut self) {
        let mut modules_de = vec![];
        let mut o_files = vec![];

        collect_module_dirs("./public/modules", &mut modules_de);

        for path in ["./public/js/common/", "./public/js/server/"].iter() {
            let seq = path.to_string() + ".seq";

            if Path::new(&seq).exists() {
                match File::open(&seq) {
                    Ok(f) => {
                        let file = BufReader::new(&f);
                        for line in file.lines() {
                            match line {
                                Ok(file_name) => {
                                    if file_name == "$modules" {
                                        for x in modules_de.iter() {
                                            o_files.push(x.to_owned());
                                        }
                                    } else {
                                        collect_js_files(&(path.to_string() + &file_name), &mut o_files);
                                    }
                                }
                                Err(e) => error!("{:?}", e),
                            }
                        }
                    }
                    Err(e) => error!("{:?}", e),
                }
            } else {
                collect_js_files(&path, &mut o_files);
            }
        }

        let params = Isolate::create_params();
        let mut isolate = Isolate::new(params);

        for x in o_files.iter() {
            match fs::read_to_string(x) {
                Ok(f) => {
                    info!("{}", x);
                    let mut scr_inf = ScriptInfo::new_with_src(x, &f);

                    scr_inf.compile_script(self.scope, self.context);

                    if let Some(mut i_script) = scr_inf.compiled_script {
                        let mut hs = v8::HandleScope::new(&mut isolate);
                        i_script.run(hs.enter(), self.context);
                    }

                    //self.scripts.insert(x.to_owned(), scr_inf);
                }
                Err(e) => error!("{:?}", e),
            }
        }
    }

    pub fn load_event_scripts(&mut self, xr: &mut XapianReader) {
        let res = xr.query(FTQuery::new_with_user("cfg:VedaSystem", "'rdf:type' === 'v-s:Event'"), &mut self.module.storage);
        if res.result_code == ResultCode::Ok && res.count > 0 {
            for id in &res.result {
                if let Some(ev_indv) = self.module.get_individual(id, &mut Individual::default()) {
                    self.prepare_script(ev_indv);
                }
            }
        }
        info!("load scripts from db: {:?}", self.scripts_order);
    }

    pub(crate) fn prepare_script(&mut self, ev_indv: &mut Individual) {
        let first_section = "".to_owned();

        if ev_indv.is_exists_bool("v-s:deleted", true) || ev_indv.is_exists_bool("v-s:disabled", true) {
            info!("disable script {}", ev_indv.get_id());
            if let Some(scr_inf) = self.scripts.get_mut(ev_indv.get_id()) {
                scr_inf.compiled_script = None;
            }
            return;
        }

        if let Some(script_text) = ev_indv.get_first_literal("v-s:script") {
            let str_script = first_section
                + "try { var ticket = get_env_str_var ('$ticket');"
                + BEFORE_VARS
                + "var _script_id = '"
                + ev_indv.get_id()
                + "';"
                + VARS_FOR_EVENT_SCRIPT
                + "script();"
                + AFTER_VARS
                + "function script() {"
                + &script_text
                + "}; } catch (e) { log_trace (e); }";

            let mut scr_inf = ScriptInfo::new_with_src(ev_indv.get_id(), &str_script);

            if let Some(v) = ev_indv.get_first_bool("v-s:unsafe") {
                scr_inf.is_unsafe = v;
            }

            if let Some(v) = ev_indv.get_first_literal("v-s:runAt") {
                scr_inf.run_at = v;
            }

            if let Some(v) = ev_indv.get_first_bool("v-s:disallowChangingSource") {
                scr_inf.disallow_changing_source = v;
            }

            if scr_inf.run_at.is_empty() {
                scr_inf.run_at = "main".to_owned();
            }

            scr_inf.prevent_by_type = HashVec::new(ev_indv.get_literals("v-s:preventByType").unwrap_or_default());
            scr_inf.trigger_by_uid = HashVec::new(ev_indv.get_literals("v-s:triggerByUid").unwrap_or_default());
            scr_inf.trigger_by_type = HashVec::new(ev_indv.get_literals("v-s:triggerByType").unwrap_or_default());
            scr_inf.dependency = HashVec::new(ev_indv.get_literals("v-s:dependency").unwrap_or_default());

            self.add_to_order(&scr_inf);

            scr_inf.compile_script(self.scope, self.context);
            self.scripts.insert(scr_inf.id.to_string(), scr_inf);
        } else {
            error!("v-s:script no found");
        }
    }

    fn add_to_order(&mut self, scr_inf: &ScriptInfo) {
        let mut count_find_dependency = 0;
        let mut inserted = false;

        let mut new_scripts_order = vec![];

        for oo in self.scripts_order.iter() {
            if count_find_dependency < scr_inf.dependency.vec.len() {
                if let Some(soo) = self.scripts.get(oo) {
                    for dp in soo.dependency.vec.iter() {
                        if scr_inf.id == *dp {
                            count_find_dependency += 1;
                        }
                    }
                }
            }

            if inserted == false && count_find_dependency >= scr_inf.dependency.vec.len() {
                new_scripts_order.push(scr_inf.id.to_owned());
                inserted = true;
            }

            if *oo != scr_inf.id {
                new_scripts_order.push(oo.to_owned());
            }
        }
        if inserted == false {
            new_scripts_order.push(scr_inf.id.to_owned());
        }

        self.scripts_order = new_scripts_order;
    }

    pub(crate) fn new(scope: &'a mut Entered<'a, HandleScope, OwnedIsolate>, context: Local<'a, Context>) -> Self {
        Self {
            scripts: Default::default(),
            scripts_order: vec![],
            module: Module::default(),
            scope,
            context,
        }
    }
}

pub(crate) fn is_filter_pass(script: &ScriptInfo, individual_id: &str, indv_types: &Vec<String>, onto: &mut Onto) -> bool {
    let mut is_pass = false;

    if !script.prevent_by_type.vec.is_empty() {
        for indv_type in indv_types.iter() {
            if script.prevent_by_type.hash.contains(indv_type) {
                return false;
            }

            if onto.is_some_entered_it(indv_type, script.prevent_by_type.vec.iter()) {
                return false;
            }
        }
    }

    if script.trigger_by_uid.vec.is_empty() && script.trigger_by_type.vec.is_empty() {
        return true;
    }

    if !script.trigger_by_uid.vec.is_empty() && script.trigger_by_uid.hash.contains(individual_id) {
        is_pass = true;
    }

    if !is_pass && !script.trigger_by_type.vec.is_empty() {
        for indv_type in indv_types.iter() {
            if script.trigger_by_type.hash.contains(indv_type) {
                is_pass = true;
                break;
            }

            if onto.is_some_entered_it(indv_type, script.trigger_by_type.vec.iter()) {
                is_pass = true;
                break;
            }
        }
    }

    return is_pass;
}
