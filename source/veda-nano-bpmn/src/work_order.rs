use crate::common::store_is_completed_into;
use crate::Context;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

pub fn create_work_order(
    process_uri: &str,
    token_uri: &str,
    activity_id: &str,
    executor_uri: Option<&str>,
    decision_form_uri: Option<&str>,
    ctx: &Context,
    module: &mut Module,
) -> Result<String, Box<dyn Error>> {
    info!("CREATE WORK ORDER, TOKEN={}, PROCESS={}", token_uri, process_uri);

    // generate work order instance
    let work_order = &mut Individual::default();
    work_order.set_id(&generate_unique_uri("wd:wo_", ""));

    work_order.add_uri("rdf:type", "bpmn:WorkOrder");
    work_order.add_uri("bpmn:hasProcess", process_uri);
    work_order.add_uri("bpmn:hasToken", token_uri);
    work_order.add_string("bpmn:activityId", activity_id, Lang::NONE);

    if let Some(r) = executor_uri {
        work_order.add_uri("bpmn:hasExecutor", r);
    }

    if let Some(r) = decision_form_uri {
        work_order.add_uri("bpmn:hasDecisionForm", r);
    }

    module.api.update_or_err(&ctx.sys_ticket, "", "", IndvOp::Put, work_order)?;
    info!("success update, uri={}", work_order.get_id());

    Ok(work_order.get_id().to_owned())
}

pub fn prepare_work_order(work_order: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal != "prepare_decision_form" {
        return Ok(());
    }

    info!("PREPARE WORK ORDER {}", work_order.get_id());

    if !work_order.is_exists_bool("bpmn:isCompleted", true) {
        return Ok(());
    }

    // check if all tasks are completed
    for wo_uri in module.get_literals_of_link(work_order, "bpmn:hasToken", "bpmn:hasWorkOrder") {
        if let Some(wo) = module.get_individual(&wo_uri, &mut Individual::default()) {
            if wo.get_id() != work_order.get_id() {
                if !wo.is_exists_bool("bpmn:isCompleted", true) {
                    return Ok(());
                }
            }
        }
    }

    if let Some(token_uri) = work_order.get_first_literal("bpmn:hasToken") {
        // all orders is completed, forward token
        store_is_completed_into(&token_uri, true, "go-prepare", &ctx.sys_ticket, module)?;
    }

    Ok(())
}

pub(crate) fn is_work_order(rdf_types: &[String]) -> bool {
    for i_type in rdf_types {
        if i_type == "bpmn:WorkOrder" {
            return true;
        }
    }
    return false;
}
