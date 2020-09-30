use crate::common::{get_individual, store_is_completed_into, MyError};
use crate::gateway::{token_ingoing_to_parallel_gateway, token_ingoing_to_xxclusive_gateway};
use crate::process_instance::start_process;
use crate::process_source::get_process_source;
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
                token_ingoing_to_parallel_gateway(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:exclusiveGateway" | "bpmn:inclusiveGateway" => {
                token_ingoing_to_xxclusive_gateway(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            _ => {
                return Err(Box::new(MyError(format!("unknown element type [{}]", type_))));
            }
        }
    }

    Ok(())
}
