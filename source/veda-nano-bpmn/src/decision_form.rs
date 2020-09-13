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
        store_is_completed_into(decision_form.get_id(), true, "prepare_decision_form", &ctx.sys_ticket, module)?;
        store_is_completed_into(&wo_uri, true, "prepare_decision_form", &ctx.sys_ticket, module)?;
    }

    Ok(())
}

pub(crate) fn is_decision_form(rdf_types: &[String]) -> bool {
    for i_type in rdf_types {
        if i_type == "bpmn:DecisionForm" {
            return true;
        }
    }
    return false;
}
