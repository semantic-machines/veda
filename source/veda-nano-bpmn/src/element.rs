use crate::common::{get_individual, set_and_store_token_into, store_is_completed_into, MyError};
use crate::process_instance::start_process;
use crate::process_source::get_process_source;
use crate::script::{execute_js, OutValue};
use crate::script_task::token_ingoing_to_script_task;
use crate::user_task::token_ingoing_to_user_task;
use crate::Context;
use std::error::Error;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_element(token: &mut Individual, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    let process_uri = token.get_first_literal("bpmn:hasProcess").unwrap_or_default();
    let process = &mut get_individual(module, &process_uri)?;
    let process_instance = &mut get_individual(module, &token.get_first_literal("bpmn:hasProcessInstance").unwrap_or_default())?;
    let nt = get_process_source(process)?;

    if let Some(element_id) = token.get_first_literal("bpmn:elementId") {
        info!("TOKEN={} INGOING TO [{}]", token.get_id(), element_id);
        let element_idx = nt.get_idx_of_id(&element_id)?;
        let type_ = nt.get_type_of_idx(element_idx)?;
        match type_ {
            "bpmn:callActivity" => {
                if let Ok(called_element) = nt.get_attribute_of_idx(element_idx, "calledElement") {
                    warn!("bpmn:callActivity, calledElement={}", called_element);
                    let mut process = get_individual(module, &called_element)?;
                    let nt = get_process_source(&mut process)?;
                    let start_form_id = process_instance.get_id();
                    start_process(start_form_id, nt, ctx, module)?;
                }
            }
            "bpmn:startEvent" | "bpmn:endEvent" => {
                store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;
            }
            "bpmn:scriptTask" => {
                token_ingoing_to_script_task(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:userTask" => {
                token_ingoing_to_user_task(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:parallelGateway" => {
                if let Some(token_ids) = process_instance.get_literals("bpmn:hasToken") {
                    let incoming = nt.get_values_of_tag(&element_idx, "bpmn:incoming");
                    let mut incoming_tokens = vec![];
                    let mut other_token_uris = vec![];
                    other_token_uris.push(token.get_id());

                    // check all tokens
                    for t_id in token_ids.iter() {
                        if t_id != token.get_id() {
                            let mut t = get_individual(module, &t_id)?;
                            if let Some(el_id) = &t.get_first_literal("bpmn:elementId") {
                                if *el_id == element_id {
                                    warn!("found token={} el_id={}", t_id, el_id);
                                    incoming_tokens.push(t);
                                } else {
                                    other_token_uris.push(t_id.as_str());
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
                    }
                }
            }
            "bpmn:exclusiveGateway" | "bpmn:inclusiveGateway" => {
                for el in nt.get_idxs_of_path(&element_idx, &["bpmn:extensionElements", "camunda:executionListener"]) {
                    let event_id = nt.get_attribute_of_idx(el, "event")?;
                    match event_id {
                        "start" | "end" => {
                            // calculate listener
                            let script_id = format!("{}+{}+{}", process_uri, element_id, event_id);
                            execute_js(token, process_instance, &script_id, "camunda:script", &el, None, &nt, ctx, &mut OutValue::List(vec![]));
                        }
                        _ => {}
                    }
                }
                store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;
            }
            _ => {
                return Err(Box::new(MyError(format!("unknown element type [{}]", type_))));
            }
        }
    }

    Ok(())
}
