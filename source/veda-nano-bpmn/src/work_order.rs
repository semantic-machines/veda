use crate::Context;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

pub fn create_work_order(
    process_uri: &str,
    token_uri: &str,
    activity_id: &str,
    executor_uri: Option<&str>,
    decision_form_uri: Option<&str>,
    ctx: &Context,
    module: &mut Module,
) -> Result<String, Box<dyn Error>> {
    info!("CREATE WORK ORDER {} ON {}", token_uri, process_uri);

    // generate work order instance
    let work_order = &mut Individual::default();
    work_order.set_id(&generate_unique_uri("wd:wo_", ""));

    work_order.add_uri("rdf:type", "bpmn:WorkOrder");
    work_order.add_uri("bpmn:hasProcess", process_uri);
    work_order.add_uri("bpmn:hasToken", token_uri);
    work_order.add_string("bpmn:activityId", activity_id, Lang::NONE);

    if let Some(r) = executor_uri {
        work_order.add_uri("bpmn:hasExecutor", r);
    }

    if let Some(r) = decision_form_uri {
        work_order.add_uri("bpmn:hasWorkOrder", r);
    }

    module.api.update_or_err(&ctx.sys_ticket, "", "", IndvOp::Put, work_order)?;
    info!("success update, uri={}", work_order.get_id());

    Ok(work_order.get_id().to_owned())
}
