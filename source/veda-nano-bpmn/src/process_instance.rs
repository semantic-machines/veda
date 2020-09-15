use crate::process_source::IndexedNodeTree;
use crate::token::create_token_and_store;
use crate::Context;
use std::error::Error;
use std::str;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

pub(crate) fn start_process(start_form_id: &str, route: IndexedNodeTree, ctx: &Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    info!("START PROCESS, ROUTE={}, START_FROM={}", route.id, start_form_id);

    // generate process instance
    let process_instance = &mut Individual::default();
    process_instance.set_id(&generate_unique_uri("wd:pr_", ""));
    process_instance.add_uri("rdf:type", "bpmn:ProcessInstance");
    process_instance.add_uri("bpmn:hasStartForm", start_form_id);
    process_instance.add_uri("bpmn:instanceOf", &route.id);

    // set status [STARTED] into start form
    let mut updated_start_form = Individual::default();
    updated_start_form.set_id(start_form_id);
    updated_start_form.set_uri("bpmn:status", "bpmn:Started");
    updated_start_form.set_uri("bpmn:startProcess", &route.id);
    updated_start_form.set_uri("bpmn:startedProcessInstance", process_instance.get_id());

    module.api.update_or_err(&ctx.sys_ticket, "", "start-process", IndvOp::SetIn, &updated_start_form)?;
    info!("success update, uri={}", updated_start_form.get_id());

    let start_events_idx = route.get_idx_of_type("bpmn:startEvent");

    for el in start_events_idx {
        let activity_id = route.get_id_of_idx(el)?;
        info!("FOUND START EVENT {}", activity_id);
        let token_uri = create_token_and_store(None, &route.id, process_instance.get_id(), activity_id, ctx, module)?;
        process_instance.add_uri("bpmn:hasToken", &token_uri);
    }

    module.api.update_or_err(&ctx.sys_ticket, "", "start-process", IndvOp::Put, process_instance)?;
    info!("success update, uri={}", process_instance.get_id());

    Ok(())
}
