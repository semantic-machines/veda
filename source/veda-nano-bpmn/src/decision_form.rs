use crate::common::{get_individual, store_is_completed_into};
use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_decision_form(decision_form: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal != "?" {
        return Ok(());
    }

    let taken_decision_uri = decision_form.get_first_literal("v-wf:takenDecision");
    if taken_decision_uri.is_none() {
        return Ok(());
    }
    let taken_decision_uri = taken_decision_uri.unwrap();

    let possible_decisions = decision_form.get_literals("v-wf:possibleDecisionClass");
    if possible_decisions.is_none() {
        return Ok(());
    }

    let mut taken_decision = get_individual(module, &taken_decision_uri)?;

    let tdt = taken_decision.get_first_literal("rdf:type");
    if tdt.is_none() {
        error!("individual {} not content type", taken_decision_uri);
        return Ok(());
    }

    if !possible_decisions.unwrap().contains(&tdt.unwrap()) {
        error!("v-wf:takenDecision not content variant of v-wf:possibleDecisionClass");
        return Ok(());
    }

    info!("PREPARE DECISION FORM {}", decision_form.get_id());

    if let Some(wo_uri) = decision_form.get_first_literal("bpmn:hasWorkOrder") {
        store_is_completed_into(decision_form.get_id(), true, "prepare-decision-form", &ctx.sys_ticket, module)?;
        store_is_completed_into(&wo_uri, true, "prepare-decision-form", &ctx.sys_ticket, module)?;
    }

    Ok(())
}

pub(crate) fn is_decision_form(rdf_types: &[String]) -> bool {
    for i_type in rdf_types {
        if i_type == "bpmn:DecisionForm" {
            return true;
        }
    }
    false
}
