#[macro_use]
extern crate log;

use lettre::{EmailAddress, Envelope, SendableEmail, SmtpClient, Transport};
use v_module::info::ModuleInfo;
use v_module::module::*;
use v_onto::individual::*;
use v_queue::consumer::*;

pub struct Context {}

fn main() -> Result<(), i32> {
    init_log();

    let mut module = Module::default();

    let mut queue_consumer = Consumer::new("./data/queue", "fanout-email2", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let module_info = ModuleInfo::new("./data", "fanout_email2", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    let mut ctx = Context {};

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

fn after_bath(_module: &mut Module, ctx: &mut Context) {}

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

    if let Err(e) = module_info.put_info(op_id, op_id) {
        error!("fail write module_info, op_id={}, err={:?}", op_id, e)
    }
}
