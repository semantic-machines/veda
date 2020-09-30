use crate::common::{store_is_completed_into, store_work_order_into};
use crate::process_source::IndexedNodeTree;
use crate::v8_script::{execute_js, OutValue};
use crate::work_order::create_work_order;
use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn token_ingoing_to_script_task(
    token: &mut Individual,
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    let element_idx = nt.get_idx_of_id(&element_id)?;

    let work_order = create_work_order(&process_instance.get_id(), token.get_id(), &element_id, None, None, ctx, module)?;
    store_work_order_into(token.get_id(), work_order.get_id(), &ctx.sys_ticket, module)?;

    let script_id = format!("{}+{}", process_uri, element_id);
    execute_js(token, process_instance, &script_id, Some(&work_order.get_id()), nt.get_values_of_tag(&element_idx, "bpmn:script").get(0), ctx, &mut OutValue::None);
    store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

    Ok(())
}
