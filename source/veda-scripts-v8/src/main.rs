#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

use crate::callback::*;
use crate::scripts_workplace::{is_filter_pass, ScriptsWorkPlace};
use crate::session_cache::{commit, CallbackSharedData, Transaction};
use rusty_v8 as v8;
use rusty_v8::scope::Entered;
use rusty_v8::{Context, HandleScope, Local, OwnedIsolate};
use std::sync::Mutex;
use std::{env, thread, time};
use v_api::app::ResultCode;
use v_api::{APIClient, IndvOp};
use v_module::info::ModuleInfo;
use v_module::module::{get_cmd, get_info_of_module, get_inner_binobj_as_individual, init_log, wait_load_ontology, wait_module, Module, PrepareError};
use v_module::onto::load_onto;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_queue::consumer::Consumer;
use v_queue::record::Mode;
use v_storage::inproc_indv_r_storage::storage_manager;

mod callback;
mod common;
mod script_info;
mod scripts_workplace;
mod session_cache;

const MAX_COUNT_LOOPS: i32 = 100;
const MAIN_QUEUE_CS: &str = "scripts_main0";

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
        p.to_owned()
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    if !tarantool_addr.is_empty() {
        info!("tarantool addr={}", &tarantool_addr);
    }
    tarantool_addr
}

pub struct MyContext<'a> {
    api_client: APIClient,
    onto: Onto,
    workplace: ScriptsWorkPlace<'a>,
    vm_id: String,
    sys_ticket: String,
    main_queue_cs: Option<Consumer>,
    queue_name: String,
    count_exec: i64,
}

fn main() -> Result<(), i32> {
    init_log();
    thread::spawn(move || storage_manager(get_storage_init_param()));

    let _setup_guard = setup();

    let mut isolate = v8::Isolate::new(Default::default());

    let mut hs = v8::HandleScope::new(&mut isolate);
    let hs_scope = hs.enter();

    let context = init_context_with_callback(hs_scope);
    main0(hs_scope, context)
}

fn main0<'a>(parent_scope: &'a mut Entered<'a, HandleScope, OwnedIsolate>, context: Local<'a, Context>) -> Result<(), i32> {
    if get_info_of_module("input-onto").unwrap_or((0, 0)).0 == 0 {
        wait_module("fulltext_indexer", wait_load_ontology());
    }

    let mut module = Module::default();

    while !module.api.connect() {
        error!("main module not ready, sleep and repeat");
        thread::sleep(time::Duration::from_millis(1000));
    }

    let mut onto = Onto::default();
    load_onto(&mut module.storage, &mut onto);

    let w_sys_ticket = module.get_sys_ticket_id();
    if w_sys_ticket.is_err() {
        error!("system ticket not found");
        return Ok(());
    }

    let mut vm_id = "main";
    let args: Vec<String> = env::args().collect();
    for el in args.iter() {
        if el == "main" || el == "lp" || el == "lp1" {
            vm_id = el;
            break;
        }
    }

    let process_name = "scripts_".to_owned() + vm_id;
    let consumer_name = &(process_name.to_owned() + "0");
    let main_queue_name = "individuals-flow";

    let mut ctx = MyContext {
        api_client: APIClient::new(Module::get_property("main_module_url").unwrap_or_default()),
        workplace: ScriptsWorkPlace::new(parent_scope, context),
        onto,
        vm_id: "main".to_owned(),
        sys_ticket: w_sys_ticket.unwrap(),
        main_queue_cs: None,
        queue_name: consumer_name.to_owned(),
        count_exec: 0,
    };

    info!("use VM id={}", process_name);

    if vm_id == "lp" {
        ctx.vm_id = "V8.LowPriority".to_owned();
    } else if vm_id == "lp1" {
        ctx.vm_id = "V8.LowPriority1".to_owned();
    } else {
        ctx.vm_id = "main".to_owned();
    }

    ctx.workplace.load_ext_scripts();
    ctx.workplace.load_event_scripts();

    let module_info = ModuleInfo::new("./data", &process_name, true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    let mut queue_consumer = Consumer::new("./data/queue", consumer_name, main_queue_name).expect("!!!!!!!!! FAIL OPEN RW CONSUMER");

    if vm_id == "lp" || vm_id == "lp1" {
        loop {
            if let Ok(cs) = Consumer::new_with_mode("./data/queue", MAIN_QUEUE_CS, main_queue_name, Mode::Read) {
                ctx.main_queue_cs = Some(cs);
                break;
            }
            warn!("main queue consumer not open, sleep and repeat");
            thread::sleep(time::Duration::from_millis(1000));
        }
    }

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_batch as fn(&mut Module, &mut MyContext<'a>, batch_size: u32) -> Option<u32>),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut MyContext<'a>, &mut Individual, my_consumer: &Consumer) -> Result<bool, PrepareError>),
        &mut (after_batch as fn(&mut Module, &mut ModuleInfo, &mut MyContext<'a>, prepared_batch_size: u32) -> bool),
        &mut (heartbeat as fn(&mut Module, &mut ModuleInfo, &mut MyContext<'a>)),
    );
    Ok(())
}

fn heartbeat(_module: &mut Module, _module_info: &mut ModuleInfo, _ctx: &mut MyContext) {}

fn before_batch(_module: &mut Module, _ctx: &mut MyContext, _size_batch: u32) -> Option<u32> {
    None
}

fn after_batch(_module: &mut Module, _module_info: &mut ModuleInfo, _ctx: &mut MyContext, _prepared_batch_size: u32) -> bool {
    false
}

fn prepare(
    _module: &mut Module,
    module_info: &mut ModuleInfo,
    ctx: &mut MyContext,
    queue_element: &mut Individual,
    my_consumer: &Consumer,
) -> Result<bool, PrepareError> {
    if let Some(main_cs_r) = &mut ctx.main_queue_cs {
        while my_consumer.count_popped > main_cs_r.count_popped && main_cs_r.id == my_consumer.id || my_consumer.id > main_cs_r.id {
            main_cs_r.get_info();

            if my_consumer.count_popped > main_cs_r.count_popped && main_cs_r.id == my_consumer.id || my_consumer.id > main_cs_r.id {
                info!("sleep, scripts_main={}:{}, my={}:{}", main_cs_r.id, main_cs_r.count_popped, my_consumer.id, my_consumer.count_popped);
                thread::sleep(time::Duration::from_millis(1000));
            }
        }
    }

    match prepare_for_js(ctx, queue_element) {
        Ok(op_id) => {
            if let Err(e) = module_info.put_info(op_id, op_id) {
                error!("fail write module_info, op_id={}, err={:?}", op_id, e);
                return Err(PrepareError::Fatal);
            }
        }
        Err(e) => {
            return Err(e);
        }
    }
    Ok(true)
}

fn prepare_for_js(ctx: &mut MyContext, queue_element: &mut Individual) -> Result<i64, PrepareError> {
    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let src = queue_element.get_first_literal("src").unwrap_or_default();
    if src != "?" && ctx.queue_name != src {
        return Ok(op_id);
    }

    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return Ok(op_id);
    }

    if cmd.unwrap() == IndvOp::Remove {
        return Ok(op_id);
    }

    let event_id = queue_element.get_first_literal("event_id").unwrap_or_default();
    let user_id = queue_element.get_first_literal("user_uri").unwrap_or_default();
    let transaction_id = queue_element.get_first_integer("tnx_id").unwrap_or_default();
    let run_at = queue_element.get_first_literal("v-s:runAt").unwrap_or_default();

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    debug!("!!!! prepare document {}", new_state.get_id());

    let mut prepare_if_is_script = false;
    let rdf_types = new_state.get_literals("rdf:type").unwrap_or_default();
    let doc_id = new_state.get_id().to_owned();

    for t in rdf_types.iter() {
        if t == "v-s:PermissionStatement" || t == "v-s:Membership" {
            return Ok(op_id);
        } else if t == "v-s:Event" {
            prepare_if_is_script = true;
        }
        //else if t == "owl:Class" || t == "rdfs:Class" || t == "rdf:Property" || t == "rdfs:Datatype" || t == "owl:ObjectProperty" || t == "owl:DatatypeProperty" {
        //    info! ("update onto: id={}", new_state.get_id());
        //   ctx.onto.update(&mut new_state);
        //}
    }

    if !prepare_if_is_script {
        if ctx.workplace.scripts.contains_key(&doc_id) {
            prepare_if_is_script = true;
        }
    }

    if prepare_if_is_script {
        ctx.workplace.prepare_script(&mut new_state);
    }

    let mut session_data = CallbackSharedData::default();

    session_data.set_g_parent_script_id_etc(&event_id);

    let mut prev_state = Individual::default();
    if get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state) {
        session_data.g_prev_state = Some(prev_state);
    }

    session_data.g_ticket = ctx.sys_ticket.to_owned();

    session_data.set_g_super_classes(&rdf_types, &ctx.onto);

    let mut last_part_event_id = "";
    let te = event_id.to_owned();
    let full_path_els: Vec<&str> = te.split(";").collect();

    if let Some(s) = full_path_els.get(0) {
        last_part_event_id = s;
    }

    session_data.g_document = Some(new_state);

    if !user_id.is_empty() {
        session_data.g_user = user_id;
    } else {
        session_data.g_user = "cfg:VedaSystem".to_owned();
    }

    let mut sh_g_vars = G_VARS.lock().unwrap();
    let g_vars = sh_g_vars.get_mut();
    *g_vars = session_data;
    drop(sh_g_vars);

    let mut hs = v8::HandleScope::new(ctx.workplace.scope);
    let local_scope = hs.enter();

    for script_id in ctx.workplace.scripts_order.iter() {
        let run_script_id = doc_id.to_owned() + "+" + script_id;
        if let Some(script) = ctx.workplace.scripts.get(script_id) {
            if let Some(mut compiled_script) = script.compiled_script {
                if src == "?" {
                    if !run_at.is_empty() && run_at != ctx.vm_id {
                        continue;
                    } else if run_at.is_empty() && script.run_at != ctx.vm_id {
                        continue;
                    }
                }

                if !is_filter_pass(script, &doc_id, &rdf_types, &mut ctx.onto) {
                    debug!("skip (filter) script:{}", script_id);
                    continue;
                }

                if script.is_unsafe {
                    warn!("this script is UNSAFE!, {}", script.id);
                } else {
                    if !event_id.is_empty() {
                        if last_part_event_id == run_script_id || last_part_event_id == "IGNORE" {
                            error!("skip script, found looped sequence, path: {}", last_part_event_id);
                            continue;
                        }

                        let mut count_loops = 0;
                        for el in full_path_els.iter() {
                            if **el == run_script_id {
                                count_loops += 1;
                            }
                        }

                        if count_loops > MAX_COUNT_LOOPS {
                            error!("skip script, counted ({}) loops in sequencee > {}, path: [{}]", count_loops, MAX_COUNT_LOOPS, event_id);
                            continue;
                        }

                        if count_loops > 1 {
                            warn!("found [{}] loops in sequence, path: [{}]", count_loops, event_id);
                        }
                    }
                }

                info!("start: {}, {}, src={}, op_id={}, tnx_id={}, event_id={}", script_id, doc_id, src, op_id, transaction_id, event_id);

                let mut sh_tnx = G_TRANSACTION.lock().unwrap();
                let tnx = sh_tnx.get_mut();
                *tnx = Transaction::default();
                tnx.id = transaction_id;
                tnx.event_id = run_script_id.to_owned() + ";" + &event_id;
                tnx.sys_ticket = ctx.sys_ticket.to_owned();
                drop(sh_tnx);

                compiled_script.run(local_scope, ctx.workplace.context);
                ctx.count_exec += 1;

                sh_tnx = G_TRANSACTION.lock().unwrap();
                let tnx = sh_tnx.get_mut();

                if script.disallow_changing_source {
                    tnx.src = src.to_owned();
                } else {
                    tnx.src = ctx.queue_name.to_owned();
                }

                let res = commit(tnx, &mut ctx.api_client);

                for item in tnx.queue.iter() {
                    info!("tnx item: cmd={:?}, uri={}, res={:?}", item.cmd, item.indv.get_id(), item.rc);
                }

                drop(sh_tnx);

                info!("{} end: {}", ctx.count_exec, script_id);

                if res != ResultCode::Ok {
                    info!("fail exec event script : {}", script_id);
                    return Err(PrepareError::Fatal);
                }
            }
        }
    }

    Ok(op_id)
}
