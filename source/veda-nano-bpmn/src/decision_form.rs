use crate::common::store_is_completed_into;
use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_decision_form(decision_form: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal != "?" {
        return Ok(());
    }

    let taken_decision = decision_form.get_first_literal("v-wf:takenDecision");
    if taken_decision.is_none() {
        return Ok(());
    }

    let possible_decisions = decision_form.get_literals("v-wf:possibleDecisionClass");
    if possible_decisions.is_none() {
        return Ok(());
    }

    if !possible_decisions.unwrap().contains(&taken_decision.unwrap()) {
        error!("v-wf:takenDecision not content variant of v-wf:possibleDecisionClass");
        return Ok(());
    }

    info!("PREPARE DECISION FORM {}", decision_form.get_id());

    if let Some(wo_uri) = decision_form.get_first_literal("bpmn:hasWorkOrder") {
        store_is_completed_into(decision_form.get_id(), true, &ctx.sys_ticket, module)?;
        store_is_completed_into(&wo_uri, true, &ctx.sys_ticket, module)?;
    }

    Ok(())
}

pub(crate) fn is_decision_form(rdf_types: &[String]) -> bool {
    for i_type in rdf_types {
        if i_type == "v-wf:DecisionForm" {
            return true;
        }
    }
    return false;
}

/*
// check if all tasks are completed
let mut work_order_form = Individual::default();
if let Some (token_uri) = module.get_literal_of_link(decision_form, "bpmn:hasWorkOrder", "bpmn:hasToken", &mut work_order_form) {
if let Some (token) = module.get_individual(&token_uri, &mut Individual::default()) {
for wo_uri in token.get_literals("bpmn:hasWorkOrder").unwrap_or_default() {
let mut cur_work_order;
if wo_uri == work_order_form.id {
cur_work_order = &work_order_form;
//v-wf:takenDecision
} else {
cur_work_order = module.
}
}
}
}
*/
