use crate::common::get_individual;
use crate::Context;

use camunda_client::apis::client::APIClient;
use camunda_client::apis::Error as CamundaError;
use camunda_client::models::{ProcessInstanceWithVariablesDto, StartProcessInstanceDto, VariableValueDto};
use std::collections::HashMap;
use std::error::Error;
use v_module::module::Module;
use v_onto::datatype::DataType;
use v_onto::individual::Individual;

pub fn prepare_start_form(start_form: &mut Individual, ctx: &mut Context, module: &mut Module, _signal: &str) -> Result<(), Box<dyn Error>> {
    if start_form.any_exists("bpmn:hasStatus", &["bpmn:ToBeStarted"]) {
        if let Some(process_id) = start_form.get_first_literal("bpmn:startProcessId") {
            let start_form_id = start_form.get_id().to_owned();
            start_form.parse_all();
            start_form.remove("bpmn:startProcess");
            start_form.remove("rdfs:label");
            start_form.remove("rdfs:isDefinedBy");
            start_form.remove("bpmn:hasStatus");

            let mut params = StartProcessInstanceDto::new();

            let mut vars = HashMap::new();
            let mut var = VariableValueDto::new();
            var.value = Some(start_form.get_obj().as_json());
            var._type = Some("json".to_owned());
            vars.insert("startForm".to_owned(), var);

            for p in start_form.get_predicates_of_type(DataType::Uri) {
                if let Some(rs) = start_form.get_resources(&p) {
                    for r in rs {
                        if let Ok(indv) = get_individual(module, r.get_uri()) {
                            let mut var = VariableValueDto::new();
                            var.value = Some(indv.get_obj().as_json());
                            var._type = Some("json".to_owned());
                            vars.insert(indv.get_id().to_owned(), var);
                        }
                    }
                }
            }

            params.variables = Some(vars);
            match ctx.api_client.process_definition_api().start_process_instance_by_key(&process_id, Some(params)) {
                Ok(res) => info!("res={:?}", res),
                Err(e) => {
                    error!("fail execute process instance, err={:?}", e);
                }
            };
        }
    }

    Ok(())
}
