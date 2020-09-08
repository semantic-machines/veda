use crate::callback::init_context_with_callback;
use crate::common::{collect_js_files, collect_module_dirs, str_2_v8, HashVec};
use rusty_v8 as v8;
use rusty_v8::{Context, HandleScope, Isolate, Local};
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use v_module::module::Module;

pub struct ScriptInfoz<'a, T> {
    pub id: String,
    pub str_script: String,
    pub compiled_script: Option<v8::Local<'a, v8::Script>>,
    pub dependency: HashVec<String>,
    pub context: T,
}

impl<'a, T: Default> ScriptInfoz<'a, T> {
    pub fn new_with_src(id: &str, src: &str) -> Self {
        Self {
            id: id.to_string(),
            str_script: src.to_string(),
            compiled_script: None,
            dependency: Default::default(),
            context: Default::default(),
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

pub struct ScriptsWorkPlace<'a, T> {
    pub scripts: HashMap<String, ScriptInfoz<'a, T>>,
    pub scripts_order: Vec<String>,
    pub module: Module,
    pub scope: HandleScope<'a, ()>,
    pub context: Local<'a, Context>,
}

impl<'a, T: Default> ScriptsWorkPlace<'a, T> {
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

        for x in o_files.iter() {
            match fs::read_to_string(x) {
                Ok(f) => {
                    info!("{}", x);
                    let mut scr_inf: ScriptInfoz<T> = ScriptInfoz::new_with_src(x, &f);

                    let scope = &mut v8::ContextScope::new(&mut self.scope, self.context);
                    scr_inf.compile_script(scope);

                    if let Some(i_script) = scr_inf.compiled_script {
                        i_script.run(scope);
                    }

                    //self.scripts.insert(x.to_owned(), scr_inf);
                }
                Err(e) => error!("{:?}", e),
            }
        }
    }

    pub fn new(isolate: &'a mut Isolate) -> Self {
        let mut scope = v8::HandleScope::new(isolate);

        let context = init_context_with_callback(&mut scope);
        Self {
            scripts: Default::default(),
            scripts_order: vec![],
            module: Module::default(),
            scope,
            context,
        }
    }

    pub fn add_to_order(&mut self, scr_inf: &ScriptInfoz<T>) {
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
}
