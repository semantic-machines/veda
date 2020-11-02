#[macro_use]
extern crate log;

use crate::common::is_start_form;
use crate::start_form::prepare_start_form;
use camunda_client::apis::client::APIClient;
use camunda_client::apis::configuration::Configuration;
use std::error::Error;
use v_module::info::ModuleInfo;
use v_module::module::{get_cmd, get_inner_binobj_as_individual, init_log, Module, PrepareError, get_info_of_module, wait_module, wait_load_ontology};
use v_module::onto::load_onto;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_queue::consumer::Consumer;

mod common;
mod start_form;

pub struct Context {
    sys_ticket: String,
    onto: Onto,
    pub api_client: APIClient,
}

fn main() -> Result<(), i32> {
    init_log("VEDA-CAMUNDA-CONNECTOR");

    if get_info_of_module("fulltext_indexer").unwrap_or((0, 0)).0 == 0 {
        wait_module("fulltext_indexer", wait_load_ontology());
    }

    listen_queue()
}

fn listen_queue<'a>() -> Result<(), i32> {
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

    let module_info = ModuleInfo::new("./data", "camunda-connector", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    //wait_load_ontology();

    let mut queue_consumer = Consumer::new("./data/queue", "camunda_connector", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let configuration = Configuration::default();

    let mut ctx = Context {
        sys_ticket: systicket,
        onto,
        api_client: APIClient::new(configuration),
    };

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_batch as fn(&mut Module, &mut Context, batch_size: u32) -> Option<u32>),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual, my_consumer: &Consumer) -> Result<bool, PrepareError>),
        &mut (after_batch as fn(&mut Module, &mut ModuleInfo, &mut Context, prepared_batch_size: u32) -> bool),
        &mut (heartbeat as fn(&mut Module, &mut ModuleInfo, &mut Context)),
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
        prepare_start_form(&mut new_state, ctx, module, &signal)?;
        return Ok(true);
    }

    Ok(true)
}
