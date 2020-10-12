use crate::common::{add_right, get_individual, store_is_completed_into, store_work_order_into, MyError};
use crate::process_source::IndexedNodeTree;
use crate::v8_script::{execute_js, OutValue};
use crate::work_order::create_work_order;
use crate::Context;
use std::borrow::BorrowMut;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn token_ingoing_to_task(
    token: &mut Individual,
    _element_id: &str,
    _process_uri: &str,
    _process_instance: &mut Individual,
    _nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;

    Ok(())
}

pub fn token_ingoing_to_user_task(
    token: &mut Individual,
    element_id: &str,
    process_uri: &str,
    process_instance: &mut Individual,
    nt: &IndexedNodeTree,
    ctx: &mut Context,
    module: &mut Module,
) -> Result<(), Box<dyn Error>> {
    let element_idx = nt.get_idx_of_id(&element_id)?;
    let mut gen_decision_form_script_id = None;
    let mut executors = vec![];
    for el in nt.get_idxs_of_path(&element_idx, &["bpmn:extensionElements", "camunda:taskListener"]) {
        match nt.get_attribute_of_idx(el, "event")? {
            "create" => {
                gen_decision_form_script_id = Some(el);
            }
            "assignment" => {
                // calculate executors
                let script_id = format!("{}+{}+assigment", process_uri, element_id);
                let mut res = OutValue::List(vec![]);
                execute_js(token, process_instance, &script_id, None, nt.get_values_of_tag(&el, "camunda:script").get(0), ctx, &mut res);
                if let OutValue::List(l) = res.borrow_mut() {
                    executors.append(l);
                }
            }
            _ => {}
        }
    }

    for executor in executors {
        if let Some(el) = gen_decision_form_script_id {
            let script_id = format!("{}+{}+create", process_uri, element_id);
            let mut res = OutValue::Individual(Individual::default());
            if execute_js(token, process_instance, &script_id, None, nt.get_values_of_tag(&el, "camunda:script").get(0), ctx, &mut res) {
                if let OutValue::Individual(form) = res.borrow_mut() {
                    if form.get_id().is_empty() {
                        form.set_id(&generate_unique_uri("wd:f_", ""));
                    }
                    form.set_bool("v-wf:isCompleted", false);
                    let work_order = create_work_order(&process_instance.get_id(), token.get_id(), &element_id, Some(&executor), Some(form.get_id()), ctx, module)?;

                    // extract person and occupation
                    let mut appointment = get_individual(module, &executor)?;

                    form.clear("v-wf:to");
                    //form.set_uri("v-wf:to", &executor);
                    if let Some(p) = appointment.get_first_literal("v-s:employee") {
                        form.add_uri("v-wf:to", &p);
                        add_right(&p, form.get_id(), ctx, module)?;
                    }
                    if let Some(p) = appointment.get_first_literal("v-s:occupation") {
                        form.add_uri("v-wf:to", &p);
                        add_right(&p, form.get_id(), ctx, module)?;
                    }

                    form.set_uri("bpmn:hasWorkOrder", work_order.get_id());

                    module.api.update_or_err(&ctx.sys_ticket, "", "no-prepare", IndvOp::Put, form)?;

                    store_work_order_into(token.get_id(), work_order.get_id(), &ctx.sys_ticket, module)?;
                } else {
                    return Err(Box::new(MyError(format!("the script [{}] returned an empty result", script_id))));
                }
            } else {
                return Err(Box::new(MyError(format!("fail execute script [{}]", script_id))));
            }
        } else {
            return Err(Box::new(MyError(format!("script create not found for element [{}]", element_id))));
        }
    }
    Ok(())
}
