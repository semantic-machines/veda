use rusty_v8 as v8;
use rusty_v8::HandleScope;
use v_v8::common::{str_2_v8, HashVec};

pub(crate) struct ScriptInfo<'a> {
    pub id: String,
    pub str_script: String,
    pub trigger_by_type: HashVec<String>,
    pub prevent_by_type: HashVec<String>,
    pub trigger_by_uid: HashVec<String>,
    pub dependency: HashVec<String>,
    pub run_at: String,
    pub disallow_changing_source: bool,
    pub is_unsafe: bool,
    pub compiled_script: Option<v8::Local<'a, v8::Script>>,
}

impl<'a> ScriptInfo<'a> {
    pub fn new_with_src(id: &str, src: &str) -> Self {
        Self {
            id: id.to_string(),
            str_script: src.to_string(),
            trigger_by_type: Default::default(),
            prevent_by_type: Default::default(),
            trigger_by_uid: Default::default(),
            dependency: Default::default(),
            run_at: "".to_string(),
            disallow_changing_source: false,
            is_unsafe: false,
            compiled_script: None,
        }
    }

    pub fn compile_script(&mut self, parent_scope: &mut HandleScope<'a>) {
        let source = str_2_v8(parent_scope, &self.str_script);

        match v8::Script::compile(parent_scope, source, None) {
            Some(script) => {
                self.compiled_script = Some(script);
            }
            None => {
                error!("fail compile script {}", self.str_script);
                self.compiled_script = None;
            }
        }
    }
}
