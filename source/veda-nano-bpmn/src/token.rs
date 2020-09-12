use crate::activity::prepare_activity;
use crate::process_source::get_process_source;
use crate::script::{execute_js, OutValue};
use crate::Context;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

pub fn create_token_and_store(new_token_uri: Option<String>, process_uri: &str, activity_id: &str, ctx: &Context, module: &mut Module) -> Result<String, Box<dyn Error>> {
    info!("CREATE TOKEN {} ON {}", activity_id, process_uri);

    // generate token instance
    let token = &mut Individual::default();
    if let Some(t) = new_token_uri {
        token.set_id(&t);
    } else {
        token.set_id(&generate_unique_uri("wd:tk_", ""));
    }

    token.add_uri("rdf:type", "bpmn:Token");
    token.add_uri("bpmn:hasProcess", process_uri);
    token.add_string("bpmn:activityId", activity_id, Lang::NONE);

    module.api.update_or_err(&ctx.sys_ticket, "", "go-prepare", IndvOp::Put, token)?;
    info!("success update, uri={}", token.get_id());

    Ok(token.get_id().to_owned())
}

pub(crate) fn is_token(rdf_types: &[String]) -> bool {
    for i_type in rdf_types {
        if i_type == "bpmn:Token" {
            return true;
        }
    }
    return false;
}

pub fn prepare_token(token: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal != "go-prepare" {
        return Ok(());
    }

    info!("PREPARE TOKEN {}", token.get_id());

    if token.is_exists_bool("bpmn:is_completed", true) {
        forward_token(token, ctx, module)?;
    } else {
        prepare_activity(token, ctx, module)?;
    }
    Ok(())
}

fn forward_token(token: &mut Individual, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    let process_uri = token.get_first_literal("bpmn:hasProcess").unwrap_or_default();
    let nt = get_process_source(&process_uri, module)?;

    if let Some(activity_id) = token.get_first_literal("bpmn:activityId") {
        let mut prev_token_uri = Some(token.get_id().to_owned());
        let activity_idx = &nt.get_idx_of_id(&activity_id)?;
        for outgoing_id in nt.get_values_of_tag(activity_idx, "bpmn:outgoing") {
            let script_id = format!("{}+{}", process_uri, activity_id);

            let mut is_forward = true;
            let res: bool = false;
            if execute_js(token, &script_id, "bpmn:conditionExpression", &activity_idx, &process_uri, None, &nt, ctx, &mut OutValue::Bool(res)) {
                is_forward = res;
            }

            if is_forward {
                if let Ok(target_ref) = nt.get_attribute_of_idx(nt.get_idx_of_id(&outgoing_id)?, "targetRef") {
                    let forwarder_token_id = create_token_and_store(prev_token_uri, &process_uri, target_ref, ctx, module)?;
                    info!("FORWARD TOKEN {} FROM {} TO {}->{}", forwarder_token_id, activity_id, outgoing_id, target_ref);
                    prev_token_uri = None;
                }
            }
        }
    }
    Ok(())
}
