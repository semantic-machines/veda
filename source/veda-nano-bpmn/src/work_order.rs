use crate::common::{get_individual, store_is_completed_into};
use crate::Context;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

pub fn create_work_order(
    process_instance_uri: &str,
    token_uri: &str,
    element_id: &str,
    executor_uri: Option<&str>,
    decision_form_uri: Option<&str>,
    ctx: &Context,
    module: &mut Module,
) -> Result<Individual, Box<dyn Error>> {
    info!(
        "CREATE WORK ORDER, ELEMENT={}, TOKEN={}, EXEC.={}, FORM={}, PROC.INST.={}",
        element_id,
        token_uri,
        executor_uri.unwrap_or_default(),
        decision_form_uri.unwrap_or_default(),
        process_instance_uri
    );

    // generate work order instance
    let mut work_order = Individual::default();
    work_order.set_id(&generate_unique_uri("wd:wo_", ""));

    work_order.add_uri("rdf:type", "bpmn:WorkOrder");
    work_order.add_uri("bpmn:hasProcessInstance", process_instance_uri);
    work_order.add_uri("bpmn:hasToken", token_uri);
    work_order.add_string("bpmn:elementId", element_id, Lang::NONE);

    if let Some(r) = executor_uri {
        work_order.add_uri("bpmn:hasExecutor", r);
    }

    if let Some(r) = decision_form_uri {
        work_order.add_uri("bpmn:hasDecisionForm", r);
    }

    module.api.update_or_err(&ctx.sys_ticket, "", "create-wo", IndvOp::Put, &work_order)?;
    Ok(work_order)
}

pub fn prepare_work_order(work_order: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal != "prepare-decision-form" {
        return Ok(());
    }

    info!("PREPARE WORK ORDER {}", work_order.get_id());

    if !work_order.is_exists_bool("bpmn:isCompleted", true) {
        return Ok(());
    }

    // check if all tasks are completed
    for wo_uri in module.get_literals_of_link(work_order, "bpmn:hasToken", "bpmn:hasWorkOrder") {
        let wo = &mut get_individual(module, &wo_uri)?;
        if wo.get_id() != work_order.get_id() && !wo.is_exists_bool("bpmn:isCompleted", true) {
            return Ok(());
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
    false
}
