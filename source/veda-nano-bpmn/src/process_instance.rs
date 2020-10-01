use crate::call_activity::get_variables;
use crate::common::get_individual;
use crate::process_source::IndexedNodeTree;
use crate::token::create_token_and_store;
use crate::Context;
use std::error::Error;
use std::str;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn start_process(
    start_form_uri: Option<&str>,
    parent_process_info: Option<(&str, &str)>,
    input_variables: &mut Individual,
    process_src: IndexedNodeTree,
    ctx: &Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    info!("START PROCESS {}, START_FROM={:?}", process_src.id, start_form_uri);

    // generate process instance
    let process_instance = input_variables;
    process_instance.set_id(&generate_unique_uri("wd:pr_", ""));
    process_instance.set_uri("rdf:type", "bpmn:ProcessInstance");
    process_instance.add_uri("bpmn:instanceOf", &process_src.id);

    if let Some((element_id, proc_inst_uri)) = parent_process_info {
        process_instance.add_uri("bpmn:parentProcessInstance", proc_inst_uri);
        process_instance.add_uri("bpmn:parentProcessElementId", element_id);
    }

    if let Some(s) = start_form_uri {
        process_instance.add_uri("bpmn:hasStartForm", s);

        // set status [STARTED] into start form
        let mut updated_start_form = Individual::default();
        updated_start_form.set_id(s);
        updated_start_form.set_uri("bpmn:hasStatus", "bpmn:Started");
        updated_start_form.set_uri("bpmn:startedProcessInstance", process_instance.get_id());
        module.api.update_or_err(&ctx.sys_ticket, "", "start-process", IndvOp::SetIn, &updated_start_form)?;
    }

    let start_events_idx = process_src.get_idx_of_type("bpmn:startEvent");

    for el in start_events_idx {
        let activity_id = process_src.get_id_of_idx(el)?;
        info!("FOUND START EVENT {}", activity_id);
        let token_uri = create_token_and_store(None, &process_src.id, process_instance.get_id(), activity_id, activity_id, ctx, module)?;
        process_instance.add_uri("bpmn:hasToken", &token_uri);
    }

    module.api.update_or_err(&ctx.sys_ticket, "", "start-process", IndvOp::Put, process_instance)?;

    Ok(())
}

pub fn end_process(
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    _nt: &IndexedNodeTree,
    _ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    info!("END PROCESS {}, PROCESS INSTANCE {}", process_uri, process_instance.get_id());

    if let Some(id) = process_instance.get_first_literal("bpmn:parentProcessInstance") {
        // is sub process

        //let element_idx = nt.get_idx_of_id(&element_id)?;
        //if let Ok(called_element) = nt.get_attribute_of_idx(element_idx, "calledElement") {
        //    warn!("bpmn:callActivity, calledElement={}", called_element);
        //}

        let mut parent_process = get_individual(module, &id)?;
        //let mut output_variables = get_variables("camunda:out", "", element_id, element_idx, process_instance, process_uri, nt, ctx)?;
    }

    Ok(())
}
