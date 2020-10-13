use crate::call_activity::set_vars;
use crate::common::{get_individual, store_is_completed_into};
use crate::process_source::{get_process_source, IndexedNodeTree};
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
    parent_token: Option<&mut Individual>,
    input_variables: &mut Individual,
    process_src: IndexedNodeTree,
    ctx: &Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    // generate process instance
    let process_instance = input_variables;
    process_instance.set_id(&generate_unique_uri("wd:pr_", ""));
    process_instance.set_uri("rdf:type", "bpmn:ProcessInstance");
    process_instance.add_uri("bpmn:instanceOf", &process_src.id);

    info!("START PROCESS {}, START_FROM={:?}, ID={}", process_src.id, start_form_uri, process_instance.get_id());

    if let Some(t) = parent_token {
        process_instance.add_uri("bpmn:parentToken", t.get_id());
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

pub fn end_process(process_uri: &str, process_instance: &mut Individual, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    info!("END PROCESS {}, PROCESS INSTANCE {}", process_uri, process_instance.get_id());

    if let Some(parent_token_id) = process_instance.get_first_literal("bpmn:parentToken") {
        // is sub process
        let mut parent_token = get_individual(module, &parent_token_id)?;

        let mut parent_proc_inst = get_individual(module, &parent_token.get_first_literal_or_err("bpmn:hasProcessInstance")?)?;
        let parent_proc_id = parent_proc_inst.get_first_literal_or_err("bpmn:instanceOf")?;

        let mut parent_proc = get_individual(module, &parent_proc_id)?;
        let nt = get_process_source(&mut parent_proc)?;
        let parent_proc_el_id = parent_token.get_first_literal_or_err("bpmn:elementId")?;
        let parent_proc_el_idx = nt.get_idx_of_id(&parent_proc_el_id)?;

        if let Ok(called_element) = nt.get_attribute_of_idx(parent_proc_el_idx, "calledElement") {
            warn!("bpmn:callActivity, calledElement={}", called_element);
            let mut out_vars = Individual::default();
            out_vars.set_id(parent_proc_inst.get_id());
            set_vars("camunda:out", &mut Individual::default(), &parent_proc_el_id, parent_proc_el_idx, process_instance, process_uri, &mut out_vars, &nt, ctx)?;

            if !out_vars.is_empty() {
                // set out variables into parent process instance
                module.api.update_or_err(&ctx.sys_ticket, "", "start-process", IndvOp::SetIn, &mut out_vars)?;
            }
        }

        store_is_completed_into(&parent_token_id, true, "go-prepare", &ctx.sys_ticket, module)?;
    }

    Ok(())
}
