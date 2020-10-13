use crate::common::{get_individual, set_and_store_token_into, store_is_completed_into};
use crate::process_instance::end_process;
use crate::process_source::IndexedNodeTree;
use crate::token::check_tokens_of_activity;
use crate::v8_script::{execute_js, OutValue};
use crate::Context;
use std::error::Error;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn token_ingoing_to_parallel_gateway(
    token: &mut Individual,
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    if let Some(token_ids) = process_instance.get_literals("bpmn:hasToken") {
        let element_idx = nt.get_idx_of_id(&element_id)?;
        let incoming = nt.get_values_of_tag(&element_idx, "bpmn:incoming");
        let mut incoming_tokens = vec![];
        let mut other_token_uris = vec![];
        other_token_uris.push(token.get_id().to_owned());

        // check all tokens
        for t_id in token_ids.iter() {
            if t_id != token.get_id() {
                let mut t = get_individual(module, &t_id)?;
                if let Some(el_id) = &t.get_first_literal("bpmn:elementId") {
                    if *el_id == element_id {
                        warn!("found token={} in el_id={}", t_id, el_id);
                        incoming_tokens.push(t);
                    } else {
                        other_token_uris.push(t_id.to_owned());
                    }
                }
            }
        }

        if incoming_tokens.len() + 1 >= incoming.len() {
            // forward first token
            store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

            // remove other incoming tokens
            for t in incoming_tokens {
                module.api.update_or_err(&ctx.sys_ticket, "", "", IndvOp::Remove, &t)?;
            }

            // remove other incoming tokens from process instance
            set_and_store_token_into(process_instance.get_id(), &other_token_uris, &ctx.sys_ticket, module)?;

            if check_tokens_of_activity("bpmn:endEvent", other_token_uris, module, None)? {
                // all tokens going into END EVENT
                end_process(process_uri, process_instance, ctx, module)?;
            }
        }
    }

    Ok(())
}

pub fn token_ingoing_to_xxclusive_gateway(
    token: &mut Individual,
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    let element_idx = nt.get_idx_of_id(&element_id)?;

    for el in nt.get_idxs_of_path(&element_idx, &["bpmn:extensionElements", "camunda:executionListener"]) {
        let event_id = nt.get_attribute_of_idx(el, "event")?;
        match event_id {
            "start" | "end" => {
                // calculate listener
                let script_id = format!("{}+{}+{}", process_uri, element_id, event_id);
                execute_js(token, process_instance, &script_id, None, nt.get_values_of_tag(&el, "camunda:script").get(0), ctx, &mut OutValue::List(vec![]));
            }
            _ => {}
        }
    }
    store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

    Ok(())
}
