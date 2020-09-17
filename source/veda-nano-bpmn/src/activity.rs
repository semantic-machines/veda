use crate::common::{add_right, get_individual, store_is_completed_into, store_work_order_into, MyError};
use crate::process_source::get_process_source;
use crate::script::{execute_js, OutValue};
use crate::work_order::create_work_order;
use crate::Context;
use std::borrow::BorrowMut;
use std::error::Error;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

pub fn prepare_activity(token: &mut Individual, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    let process_uri = token.get_first_literal("bpmn:hasProcess").unwrap_or_default();
    let process = &mut get_individual(module, &process_uri)?;
    let process_instance = &mut get_individual(module, &token.get_first_literal("bpmn:hasProcessInstance").unwrap_or_default())?;
    let nt = get_process_source(process)?;

    if let Some(activity_id) = token.get_first_literal("bpmn:activityId") {
        info!("PREPARE ACTIVITY TOKEN={} ACTIVITY={}", token.get_id(), activity_id);
        let activity_idx = nt.get_idx_of_id(&activity_id)?;
        let type_ = nt.get_type_of_idx(activity_idx)?;
        match type_ {
            "bpmn:startEvent" | "bpmn:endEvent" => {
                store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;
            }
            "bpmn:scriptTask" => {
                let work_order = create_work_order(&process_uri, token.get_id(), &activity_id, None, None, ctx, module)?;
                store_work_order_into(token.get_id(), work_order.get_id(), &ctx.sys_ticket, module)?;

                let script_id = format!("{}+{}", process_uri, activity_id);
                execute_js(token, process_instance, &script_id, "bpmn:script", &activity_idx, Some(&work_order.get_id()), &nt, ctx, &mut OutValue::None);
                store_is_completed_into(token.get_id(), true, "go-prepare", &ctx.sys_ticket, module)?;
            }
            "bpmn:userTask" => {
                let mut gen_decision_form_script_id = None;
                let mut executors = vec![];
                for el in nt.get_idxs_of_path(&activity_idx, &["bpmn:extensionElements", "camunda:taskListener"]) {
                    match nt.get_attribute_of_idx(el, "event")? {
                        "create" => {
                            gen_decision_form_script_id = Some(el);
                        }
                        "assignment" => {
                            // calculate executors
                            let script_id = format!("{}+{}+assigment", process_uri, activity_id);
                            let mut res = OutValue::List(vec![]);
                            execute_js(token, process_instance, &script_id, "camunda:script", &el, None, &nt, ctx, &mut res);
                            if let OutValue::List(l) = res.borrow_mut() {
                                executors.append(l);
                            }
                        }
                        _ => {}
                    }
                }

                for executor in executors {
                    if let Some(el) = gen_decision_form_script_id {
                        let script_id = format!("{}+{}+create", process_uri, activity_id);
                        let mut res = OutValue::Individual(Individual::default());
                        if execute_js(token, process_instance, &script_id, "camunda:script", &el, None, &nt, ctx, &mut res) {
                            if let OutValue::Individual(form) = res.borrow_mut() {
                                if form.get_id().is_empty() {
                                    form.set_id(&generate_unique_uri("wd:f_", ""));
                                }
                                form.set_bool("v-wf:isCompleted", false);
                                let work_order = create_work_order(&process_uri, token.get_id(), &activity_id, Some(&executor), Some(form.get_id()), ctx, module)?;

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
                                info!("success update, uri={}", form.get_id());

                                store_work_order_into(token.get_id(), work_order.get_id(), &ctx.sys_ticket, module)?;
                            } else {
                                return Err(Box::new(MyError(format!("the script [{}] returned an empty result", script_id))));
                            }
                        } else {
                            return Err(Box::new(MyError(format!("fail execute script [{}]", script_id))));
                        }
                    } else {
                        return Err(Box::new(MyError(format!("script create not found for activity [{}]", activity_id))));
                    }
                }
            }
            "bpmn:exclusiveGateway" => {}
            _ => {
                return Err(Box::new(MyError(format!("unknown activity type [{}]", type_))));
            }
        }
    }

    Ok(())
}
