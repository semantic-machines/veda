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
    info!("START PROCESS {}", route.id);

    // generate process instance
    let process = &mut Individual::default();
    process.set_id(&generate_unique_uri("wd:pr_", ""));
    process.add_uri("rdf:type", "bpmn:ProcessInstance");
    process.add_uri("bpmn:hasStartForm", start_form_id);
    process.add_uri("bpmn:instanceOf", &route.id);

    module.api.update_or_err(&ctx.sys_ticket, "", "", IndvOp::Put, process)?;
    info!("success update, uri={}", process.get_id());

    // set status [STARTED] into start form
    let mut updated_start_form = Individual::default();
    updated_start_form.set_id(start_form_id);
    updated_start_form.set_uri("bpmn:hasStatusWorkflow", "bpmn:Started");

    module.api.update_or_err(&ctx.sys_ticket, "", "", IndvOp::SetIn, &updated_start_form)?;
    info!("success update, uri={}", updated_start_form.get_id());

    let start_events_idx = route.get_idx_of_type("bpmn:startEvent");

    for el in start_events_idx {
        let activity_id = route.get_id_of_idx(el)?;
        info!("FOUND START EVENT {}", activity_id);
        create_token_and_store(None, &route.id, activity_id, ctx, module)?;
    }

    Ok(())
}
