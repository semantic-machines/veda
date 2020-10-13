use crate::call_activity::token_ingoing_to_call_activity;
use crate::common::{add_and_store_token_into, get_individual, MyError};
use crate::event::{token_ingoing_to_end_event, token_ingoing_to_start_event};
use crate::gateway::{token_ingoing_to_parallel_gateway, token_ingoing_to_xxclusive_gateway};
use crate::process_source::get_process_source;
use crate::script_task::token_ingoing_to_script_task;
use crate::user_task::{token_ingoing_to_task, token_ingoing_to_user_task};
use crate::v8_script::{execute_js, OutValue};
use crate::Context;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

pub fn create_token_and_store(
    new_token_uri: Option<String>,
    process_uri: &str,
    process_instance_uri: &str,
    incoming_element_id: &str,
    element_id: &str,
    ctx: &Context,
    module: &mut Module,
) -> Result<String, Box<dyn Error>> {
    info!("CREATE TOKEN, ELEMENT={} PROCESS={}", element_id, process_uri);

    // generate token instance
    let token = &mut Individual::default();
    if let Some(t) = new_token_uri {
        token.set_id(&t);
    } else {
        token.set_id(&generate_unique_uri("wd:tk_", ""));
    }

    token.add_uri("rdf:type", "bpmn:Token");
    token.add_uri("bpmn:hasProcess", process_uri);
    token.add_uri("bpmn:hasProcessInstance", process_instance_uri);
    token.add_string("bpmn:inElementId", incoming_element_id, Lang::NONE);
    token.add_string("bpmn:elementId", element_id, Lang::NONE);

    module.api.update_or_err(&ctx.sys_ticket, "", "go-prepare", IndvOp::Put, token)?;
    Ok(token.get_id().to_owned())
}

pub(crate) fn is_token(rdf_types: &[String]) -> bool {
    for i_type in rdf_types {
        if i_type == "bpmn:Token" {
            return true;
        }
    }
    false
}

fn forward_token(token: &mut Individual, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    let process_uri = token.get_first_literal("bpmn:hasProcess").unwrap_or_default();
    let process_instance_uri = token.get_first_literal("bpmn:hasProcessInstance").unwrap_or_default();
    let process = &mut get_individual(module, &process_uri)?;
    let nt = get_process_source(process)?;

    if let Some(element_id) = token.get_first_literal("bpmn:elementId") {
        let mut prev_token_uri = Some(token.get_id().to_owned());
        let element_idx = nt.get_idx_of_id(&element_id)?;
        let type_ = nt.get_type_of_idx(element_idx)?;
        let default_flow = nt.get_attribute_of_idx(element_idx, "default").unwrap_or_default();

        let mut out_ids = vec![];
        for outgoing_id in nt.get_values_of_tag(&element_idx, "bpmn:outgoing") {
            if outgoing_id != default_flow {
                out_ids.push(outgoing_id.to_owned());
            }
        }
        if !default_flow.is_empty() {
            out_ids.push(default_flow.to_string());
        }

        for outgoing_id in out_ids {
            let outgoing_idx = nt.get_idx_of_id(&outgoing_id)?;
            let script_id = format!("{}+{}+{}", process_uri, element_id, outgoing_id);

            let mut is_forward = true;
            let mut res = OutValue::Bool(false);
            if execute_js(token, process, &script_id, None, nt.get_values_of_tag(&outgoing_idx, "bpmn:conditionExpression").get(0), ctx, &mut res) {
                if let OutValue::Bool(true) = res {
                    is_forward = true;
                } else {
                    is_forward = false;
                }
            }

            if is_forward {
                if let Ok(target_ref) = nt.get_attribute_of_idx(outgoing_idx, "targetRef") {
                    let forwarder_token_id = create_token_and_store(prev_token_uri.clone(), &process_uri, &process_instance_uri, &element_id, target_ref, ctx, module)?;
                    if prev_token_uri.is_none() {
                        add_and_store_token_into(&process_instance_uri, &forwarder_token_id, &ctx.sys_ticket, module)?;
                    }
                    info!("FORWARD TOKEN {} FROM {} TO {}->{}", forwarder_token_id, element_id, outgoing_id, target_ref);
                    prev_token_uri = None;
                }

                if type_ == "bpmn:exclusiveGateway" {
                    break;
                }
            }
        }
    }
    Ok(())
}

pub fn prepare_token(token: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal != "go-prepare" {
        return Ok(());
    }

    info!("PREPARE TOKEN {}", token.get_id());

    if token.is_exists_bool("bpmn:isCompleted", true) {
        forward_token(token, ctx, module)?;
    } else {
        token_ingoing_to(token, ctx, module)?;
    }
    Ok(())
}

fn token_ingoing_to(token: &mut Individual, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
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
                token_ingoing_to_call_activity(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:startEvent" => {
                token_ingoing_to_start_event(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:endEvent" => {
                token_ingoing_to_end_event(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:scriptTask" => {
                token_ingoing_to_script_task(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:userTask" => {
                token_ingoing_to_user_task(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
            }
            "bpmn:task" => {
                token_ingoing_to_task(token, &element_id, &process_uri, process_instance, &nt, ctx, module)?;
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

pub fn check_tokens_of_activity(activity_id: &str, token_ids: Vec<String>, module: &mut Module, skipped_id: Option<&str>) -> Result<bool, Box<dyn Error>> {
    for t_id in token_ids.iter() {
        if let Some(s) = skipped_id {
            if t_id == s {
                continue;
            }
        }
        let mut t = get_individual(module, &t_id)?;
        if let Some(el_id) = &t.get_first_literal("bpmn:elementId") {
            if *el_id != activity_id {
                return Ok(false);
            }
        }
    }
    Ok(true)
}
