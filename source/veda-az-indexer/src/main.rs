#[macro_use]
extern crate log;

use chrono::Local;
use env_logger::Builder;
use ini::Ini;
use log::LevelFilter;
use std::io::Write;
use std::{thread, time};
use v_api::*;
use v_module::module::*;
use v_onto::individual::*;
use v_queue::consumer::*;

struct Context {
    id: u32,
}

fn main() -> Result<(), i32> {
    info!("AZ-INDEXER");
    let env_var = "RUST_LOG";
    match std::env::var_os(env_var) {
        Some(val) => println!("use env var: {}: {:?}", env_var, val.to_str()),
        None => std::env::set_var(env_var, "info"),
    }

    Builder::new()
        .format(|buf, record| writeln!(buf, "{} [{}] - {}", Local::now().format("%Y-%m-%dT%H:%M:%S%.3f"), record.level(), record.args()))
        .filter(None, LevelFilter::Info)
        .init();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let mut module = Module::default();

    let mut queue_consumer = Consumer::new("./data/queue", "az-indexer", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let mut ctx = Context {
        id: 0,
    };

    module.listen_queue(
        &mut queue_consumer,
        &mut ctx,
        &mut (before_bath as fn(&mut Module, &mut Context)),
        &mut (prepare as fn(&mut Module, &mut Context, &mut Individual)),
        &mut (after_bath as fn(&mut Module, &mut Context)),
    );
    Ok(())
}

fn before_bath(module: &mut Module, ctx: &mut Context) {}

fn after_bath(module: &mut Module, ctx: &mut Context) {
    thread::sleep(time::Duration::from_millis(3000));
}

fn prepare(module: &mut Module, ctx: &mut Context, queue_element: &mut Individual) {
    let mut prev_state = Individual::default();
    let mut new_state = Individual::default();

    get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);
    let cmd = get_cmd(queue_element);

    if cmd.is_none() {
        return;
    }

    if new_state.any_exists("rdf:type", &["v-s:PermissionStatement"]) || prev_state.any_exists("rdf:type", &["v-s:PermissionStatement"]) {
        prepare_permission_statement(&mut prev_state, &mut new_state, cmd.unwrap(), module);
    } else if new_state.any_exists("rdf:type", &["v-s:Membership"]) || prev_state.any_exists("rdf:type", &["v-s:Membership"]) {
        prepare_membership(&mut prev_state, &mut new_state, cmd.unwrap(), module);
    } else if new_state.any_exists("rdf:type", &["v-s:PermissionFilter"]) || prev_state.any_exists("rdf:type", &["v-s:PermissionFilter"]) {
        prepare_permission_filter(&mut prev_state, &mut new_state, cmd.unwrap(), module);
    }

    ctx.id += 1;
}

fn prepare_permission_statement(new_state: &mut Individual, prev_state: &mut Individual, cmd: IndvOp, module: &mut Module) {}

fn prepare_membership(new_state: &mut Individual, prev_state: &mut Individual, cmd: IndvOp, module: &mut Module) {}

fn prepare_permission_filter(new_state: &mut Individual, prev_state: &mut Individual, cmd: IndvOp, module: &mut Module) {}
