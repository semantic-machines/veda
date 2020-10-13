use crate::common::store_is_completed_into;
use crate::process_instance::end_process;
use crate::process_source::IndexedNodeTree;
use crate::token::check_tokens_of_activity;
use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn token_ingoing_to_start_event(
    token: &mut Individual,
    _element_id: &str,
    _process_uri: &str,
    _process_instance: &mut Individual,
    _nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

    Ok(())
}

pub fn token_ingoing_to_end_event(
    token: &mut Individual,
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    _nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

    if let Some(token_ids) = process_instance.get_literals("bpmn:hasToken") {
        if check_tokens_of_activity(element_id, token_ids, module, Some(token.get_id()))? {
            // all tokens going into END EVENT
            end_process(process_uri, process_instance, ctx, module)?;
        }
    }

    Ok(())
}
