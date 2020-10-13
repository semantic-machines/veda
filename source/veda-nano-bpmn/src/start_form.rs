use crate::common::get_individual;
use crate::process_instance::start_process;
use crate::process_source::get_process_source;
use crate::Context;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_start_form(start_form: &mut Individual, ctx: &mut Context, module: &mut Module, _signal: &str) -> Result<(), Box<dyn Error>> {
    if start_form.any_exists("bpmn:hasStatus", &["bpmn:ToBeStarted"]) {
        if let Some(process_uri) = start_form.get_first_literal("bpmn:startProcess") {
            let mut process = get_individual(module, &process_uri)?;
            let nt = get_process_source(&mut process)?;
            let start_form_id = start_form.get_id().to_owned();

            start_form.parse_all();

            start_form.remove("bpmn:startProcess");
            start_form.remove("rdfs:label");
            start_form.remove("rdfs:isDefinedBy");
            start_form.remove("bpmn:hasStatus");

            start_process(Some(&start_form_id), None, start_form, nt, ctx, module)?;
        }
    }

    Ok(())
}
