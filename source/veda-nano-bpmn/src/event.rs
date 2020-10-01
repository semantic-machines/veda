use crate::common::{get_individual, store_is_completed_into};
use crate::process_instance::end_process;
use crate::process_source::IndexedNodeTree;
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
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

    if let Some(token_ids) = process_instance.get_literals("bpmn:hasToken") {
        // check all tokens
        for t_id in token_ids.iter() {
            if t_id != token.get_id() {
                let mut t = get_individual(module, &t_id)?;
                if let Some(el_id) = &t.get_first_literal("bpmn:elementId") {
                    if *el_id != element_id {
                        return Ok(());
                    }
                }
            }
        }

        // all tokens going into END EVENT
        end_process(&element_id, &process_uri, process_instance, &nt, ctx, module)?;
    }

    Ok(())
}
