use crate::common::get_individual;
use crate::Context;
use camunda_client::apis::client::APIClient;
use camunda_client::apis::Error as CamundaError;
use std::error::Error;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_start_form(start_form: &mut Individual, ctx: &mut Context, module: &mut Module, _signal: &str) -> Result<(), Box<dyn Error>> {
    if start_form.any_exists("bpmn:hasStatus", &["bpmn:ToBeStarted"]) {
        if let Some(process_id) = start_form.get_first_literal("bpmn:startProcessId") {
            let start_form_id = start_form.get_id().to_owned();

            start_form.parse_all();

            ctx.api_client.process_definition_api().start_process_instance(&process_id, None);
        }
    }

    Ok(())
}
