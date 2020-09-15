#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate maplit;
extern crate nanoid;
extern crate serde;
extern crate serde_derive;

mod activity;
mod common;
mod decision_form;
mod process_instance;
mod process_source;
mod script;
mod token;
mod work_order;

use crate::common::{get_individual, is_start_form};
use crate::decision_form::{is_decision_form, prepare_decision_form};
use crate::process_instance::start_process;
use crate::process_source::get_process_source;
use crate::script::ScriptInfoContext;
use crate::token::{is_token, prepare_token};
use crate::work_order::{is_work_order, prepare_work_order};
use rusty_v8 as v8;
use rusty_v8::Isolate;
use std::error::Error;
use std::sync::Mutex;
use std::thread;
use v_module::info::ModuleInfo;
use v_module::module::{get_cmd, get_inner_binobj_as_individual, init_log, Module, PrepareError};
use v_module::onto::load_onto;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_queue::consumer::Consumer;
use v_storage::remote_indv_r_storage::inproc_storage_manager;
use v_v8::scripts_workplace::ScriptsWorkPlace;

pub struct Context<'a> {
    sys_ticket: String,
    onto: Onto,
    workplace: ScriptsWorkPlace<'a, ScriptInfoContext>,
}

lazy_static! {
    static ref INIT_LOCK: Mutex<u32> = Mutex::new(0);
}

#[must_use]
struct SetupGuard {}

impl Drop for SetupGuard {
    fn drop(&mut self) {
        // TODO shutdown process cleanly.
    }
}

fn setup() -> SetupGuard {
    let mut g = INIT_LOCK.lock().unwrap();
    *g += 1;
    if *g == 1 {
        v8::V8::initialize_platform(v8::new_default_platform().unwrap());
        v8::V8::initialize();
    }
    SetupGuard {}
}

fn get_storage_init_param() -> String {
    let tarantool_addr = if let Some(p) = Module::get_property("tarantool_url") {
        p
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    if !tarantool_addr.is_empty() {
        info!("tarantool addr={}", &tarantool_addr);
    }
    tarantool_addr
}

fn main() -> Result<(), i32> {
    init_log("BPMN-NANO-ENGINE");
    thread::spawn(move || inproc_storage_manager(get_storage_init_param()));

    let _setup_guard = setup();

    let isolate = &mut v8::Isolate::new(Default::default());

    main0(isolate)
}

fn main0<'a>(isolate: &'a mut Isolate) -> Result<(), i32> {
    let mut module = Module::default();
    let systicket;
    if let Ok(t) = module.get_sys_ticket_id() {
        systicket = t;
    } else {
        error!("fail get systicket");
        return Ok(());
    }

    let mut onto = Onto::default();

    info!("load onto start");
    load_onto(&mut module.storage, &mut onto);
    info!("load onto end");

    let module_info = ModuleInfo::new("./data", "bpmn_nano_engine", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    //wait_load_ontology();

    let mut queue_consumer = Consumer::new("./data/queue", "bpmn_nano_engine", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let mut ctx = Context {
        sys_ticket: systicket,
        onto,
        workplace: ScriptsWorkPlace::new(isolate),
    };

    ctx.workplace.load_ext_scripts();

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_batch as fn(&mut Module, &mut Context<'a>, batch_size: u32) -> Option<u32>),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context<'a>, &mut Individual, my_consumer: &Consumer) -> Result<bool, PrepareError>),
        &mut (after_batch as fn(&mut Module, &mut ModuleInfo, &mut Context<'a>, prepared_batch_size: u32) -> bool),
        &mut (heartbeat as fn(&mut Module, &mut ModuleInfo, &mut Context<'a>)),
    );
    Ok(())
}

fn heartbeat(_module: &mut Module, _module_info: &mut ModuleInfo, _ctx: &mut Context) {}

fn before_batch(_module: &mut Module, _ctx: &mut Context, _size_batch: u32) -> Option<u32> {
    None
}

fn after_batch(_module: &mut Module, _module_info: &mut ModuleInfo, _ctx: &mut Context, _prepared_batch_size: u32) -> bool {
    false
}

fn prepare(module: &mut Module, _module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual, _my_consumer: &Consumer) -> Result<bool, PrepareError> {
    match prepare_and_err(module, _module_info, ctx, queue_element, _my_consumer) {
        Ok(r) => Ok(r),
        Err(e) => {
            error!("prepare err={:?}", e);
            Err(PrepareError::Recoverable)
        }
    }
}

fn prepare_and_err(
    module: &mut Module,
    _module_info: &mut ModuleInfo,
    ctx: &mut Context,
    queue_element: &mut Individual,
    _my_consumer: &Consumer,
) -> Result<bool, Box<dyn Error>> {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return Ok(true);
    }
    let signal = queue_element.get_first_literal("src").unwrap_or_default();

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    let rdf_types = new_state.get_literals("rdf:type").unwrap_or_default();

    if is_start_form(&rdf_types, &mut ctx.onto) && signal == "?" {
        if new_state.any_exists("bpmn:hasStatusWorkflow", &["bpmn:ToBeStarted"]) {
            if let Some(process_uri) = new_state.get_first_literal("bpmn:instanceOf") {
                let mut process = get_individual(module, &process_uri)?;
                let nt = get_process_source(&mut process)?;
                let start_form_id = new_state.get_id();
                start_process(start_form_id, nt, ctx, module)?;
            }
        }
        return Ok(true);
    }

    if is_token(&rdf_types) {
        prepare_token(&mut new_state, ctx, module, &signal)?;
        return Ok(true);
    }

    if is_decision_form(&rdf_types) {
        prepare_decision_form(&mut new_state, ctx, module, &signal)?;
        return Ok(true);
    }

    if is_work_order(&rdf_types) {
        prepare_work_order(&mut new_state, ctx, module, &signal)?;
        return Ok(true);
    }

    Ok(true)
}
