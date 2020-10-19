use crate::common::get_individual;
use crate::process_instance::start_process;
use crate::process_source::{get_process_source, IndexedNodeTree};
use crate::v8_script::{execute_js, OutValue};
use crate::Context;
use indextree::NodeId;
use std::borrow::BorrowMut;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn token_ingoing_to_call_activity(
    token: &mut Individual,
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    let element_idx = nt.get_idx_of_id(&element_id)?;
    if let Ok(called_element) = nt.get_attribute_of_idx(element_idx, "calledElement") {
        warn!("bpmn:callActivity, calledElement={}", called_element);

        let mut input_variables = Individual::default();
        set_vars("camunda:in", token, element_id, element_idx, process_instance, process_uri, &mut input_variables, nt, ctx)?;

        let mut process = get_individual(module, &called_element)?;
        let nt = get_process_source(&mut process)?;
        start_process(None, Some(token), &mut input_variables, nt, ctx, module)?;
    }

    Ok(())
}

pub fn set_vars(
    var_tag: &str,
    token: &mut Individual,
    element_id: &str,
    element_idx: NodeId,
    process_instance: &mut Individual,
    process_uri: &str,
    variables: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
) -> Result<(), Box<dyn Error>> {
    for in_var_idx in nt.get_idxs_of_path(&element_idx, &["bpmn:extensionElements", var_tag]) {
        if let Ok(source_expression) = nt.get_attribute_of_idx(in_var_idx, "sourceExpression") {
            let source_expression = source_expression.replace("&#39;", "'");
            let target = nt.get_attribute_of_idx(in_var_idx, "target")?;

            let script_id = format!("{}+{}+{}", process_uri, element_id, var_tag);
            let mut res = OutValue::Individual(Individual::default());

            execute_js(token, process_instance, &script_id, None, Some(&source_expression.to_owned()), ctx, &mut res);
            if let OutValue::Individual(l) = res.borrow_mut() {
                debug!("var mapping={}", l.to_string());
                if let Some(resources_to_set) = l.get_resources("0") {
                    warn!("SET VARIABLE {}, VALUE={:?}, source_expression={}", target, resources_to_set, source_expression);
                    variables.set_resources(target, &resources_to_set);
                }
            }
        }
    }
    Ok(())
}
