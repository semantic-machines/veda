#[macro_use]
extern crate log;

use crate::common::*;
use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use std::io::Write;
use v_module::info::ModuleInfo;
use v_module::module::*;
use v_onto::individual::*;
use v_queue::consumer::*;
use v_storage::storage::*;

mod common;

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

    //let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    //let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let mut module = Module::default();

    let mut queue_consumer = Consumer::new("./data/queue", "az-indexer", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let mut ctx = Context {
        permission_statement_counter: 0,
        membership_counter: 0,
        storage: VStorage::new_lmdb("./data", StorageMode::ReadWrite),
    };

    let module_info = ModuleInfo::new("./data", "acl_preparer", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_bath as fn(&mut Module, &mut Context)),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual)),
        &mut (after_bath as fn(&mut Module, &mut Context)),
    );
    Ok(())
}

fn before_bath(_module: &mut Module, _ctx: &mut Context) {}

fn after_bath(_module: &mut Module, ctx: &mut Context) {
    if (ctx.permission_statement_counter + ctx.membership_counter) % 100 == 0 {
        info!("count prepared: permissions={}, memberships={}", ctx.permission_statement_counter, ctx.membership_counter);
    }
}

fn prepare(_module: &mut Module, module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return;
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let mut prev_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if new_state.any_exists("rdf:type", &["v-s:PermissionStatement"]) || prev_state.any_exists("rdf:type", &["v-s:PermissionStatement"]) {
        prepare_permission_statement(&mut prev_state, &mut new_state, ctx);
        ctx.permission_statement_counter += 1;
    } else if new_state.any_exists("rdf:type", &["v-s:Membership"]) || prev_state.any_exists("rdf:type", &["v-s:Membership"]) {
        prepare_membership(&mut prev_state, &mut new_state, ctx);
        ctx.membership_counter += 1;
    } else if new_state.any_exists("rdf:type", &["v-s:PermissionFilter"]) || prev_state.any_exists("rdf:type", &["v-s:PermissionFilter"]) {
        prepare_permission_filter(&mut prev_state, &mut new_state, ctx);
    }

    if let Err(e) = module_info.put_info(op_id, op_id) {
        error!("fail write module_info, op_id={}, err={:?}", op_id, e)
    }
}

fn prepare_permission_statement(prev_state: &mut Individual, new_state: &mut Individual, ctx: &mut Context) {
    prepare_right_set(prev_state, new_state, "v-s:permissionObject", "v-s:permissionSubject", PERMISSION_PREFIX, 0, ctx);
}

fn prepare_membership(prev_state: &mut Individual, new_state: &mut Individual, ctx: &mut Context) {
    prepare_right_set(
        prev_state,
        new_state,
        "v-s:resource",
        "v-s:memberOf",
        MEMBERSHIP_PREFIX,
        Access::CanCreate as u8 | Access::CanRead as u8 | Access::CanUpdate as u8 | Access::CanDelete as u8,
        ctx,
    );
}

fn prepare_permission_filter(prev_state: &mut Individual, new_state: &mut Individual, ctx: &mut Context) {
    prepare_right_set(prev_state, new_state, "v-s:permissionObject", "v-s:resource", FILTER_PREFIX, 0, ctx);
}
