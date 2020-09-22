use crate::common::{get_individual, store_is_completed_into, MyError};
use crate::process_source::get_process_source;
use crate::script::{execute_js, OutValue};
use crate::script_task::token_ingoing_to_script_task;
use crate::user_task::token_ingoing_to_user_task;
use crate::Context;
use std::error::Error;
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
                let mut count_full_ingoing = 0;

                if let Some(token_ids) = process_instance.get_literals("bpmn:hasToken") {
                    for t_id in token_ids.iter() {
                        if t_id == token.get_id() {
                            count_full_ingoing += 1;
                            continue;
                        }
                        let mut t = get_individual(module, &t_id)?;
                        if let Some(el_id) = &t.get_first_literal("bpmn:elementId") {
                            warn!("el_id={}", el_id);
                            if *el_id == element_id {
                                count_full_ingoing += 1;
                            }
                        }
                        if count_full_ingoing + 1 >= token_ids.len() {
                            break;
                        }
                    }

                    if count_full_ingoing + 1 >= token_ids.len() {
                        store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;
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
