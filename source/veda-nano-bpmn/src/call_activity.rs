use crate::common::get_individual;
use crate::process_instance::start_process;
use crate::process_source::{get_process_source, IndexedNodeTree};
use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn token_ingoing_to_call_activity(
    _token: &mut Individual,
    element_id: &str,
    _process_uri: &str,
    process_instance: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    let element_idx = nt.get_idx_of_id(&element_id)?;
    if let Ok(called_element) = nt.get_attribute_of_idx(element_idx, "calledElement") {
        warn!("bpmn:callActivity, calledElement={}", called_element);
        let mut process = get_individual(module, &called_element)?;
        let nt = get_process_source(&mut process)?;
        let start_form_id = process_instance.get_id();
        start_process(start_form_id, nt, ctx, module)?;
    }

    Ok(())
}
