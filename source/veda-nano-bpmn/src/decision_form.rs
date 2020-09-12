use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_decision_form(decision_form: &mut Individual, ctx: &mut Context, module: &mut Module, signal: &str) -> Result<(), Box<dyn Error>> {
    if signal == "no-prepare" {
        return Ok(());
    }

    info!("PREPARE DECISION FORM {}", decision_form.get_id());

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
