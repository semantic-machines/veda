use chrono::prelude::*;
use std::ops::Sub;
use time::Duration;
use v_api::app::ResultCode;
//use v_api::IndvOp;
use crate::CleanerContext;
use std::collections::HashMap;
use v_module::info::ModuleInfo;
use v_onto::individual::Individual;

const MAX_SIZE_BATCH: i64 = 1000;

pub fn clean_process(ctx: &mut CleanerContext) {
    let module_info = ModuleInfo::new("./data", "clean_process", true);
    if module_info.is_err() {
        error!("{:?}", &module_info.err());
        return;
    }
    let mut module_info = module_info.unwrap();

    if let Some((mut pos, _)) = module_info.read_info() {
        let query = get_query_for_work_item(ctx);
        let res = ctx.ch_client.select(&ctx.systicket.user_uri, &query, MAX_SIZE_BATCH, MAX_SIZE_BATCH, pos);

        if res.result_code == ResultCode::Ok {
            for id in res.result.iter() {
                pos += 1;
                if let Some(rindv) = ctx.module.get_individual(id, &mut Individual::default()) {
                    if let Some(process) = ctx.module.get_individual(&rindv.get_first_literal("v-wf:forProcess").unwrap_or_default(), &mut Individual::default()) {
                        if !process.is_exists("v-wf:parentWorkOrder") {
                            let mut process_elements = HashMap::new();
                            collect_process_elements("", process, &mut process_elements, ctx);
                        }
                    } else {
                        error!("invalid WorkItem {}", id);
                    }
                }
            }
        }
    }
}

fn get_query_for_work_item(ctx: &mut CleanerContext) -> String {
    let output_conditions_list =
        ctx.ch_client.select(&ctx.systicket.user_uri, "SELECT DISTINCT id FROM veda_tt.`v-wf:OutputCondition`", MAX_SIZE_BATCH, MAX_SIZE_BATCH, 0);

    let mut q0 = String::default();
    for el in output_conditions_list.result.iter() {
        if q0.len() > 0 {
            q0.push_str(" OR ");
        }
        q0.push_str("v_wf_forNetElement_str[1] = '");
        q0.push_str(el);
        q0.push_str("'")
    }

    let date_before = Utc::now().naive_utc().sub(Duration::days(30));

    format!(
        "SELECT distinct id
    FROM veda_tt.`v-wf:WorkItem`
    WHERE ({})
    AND v_wf_isCompleted_int[1] = 1
    AND v_s_created_date[1] < toDateTime ({}) ORDER BY v_s_created_date DESC",
        q0,
        date_before.timestamp()
    )
}

fn collect_process_elements(parent_id: &str, process: &mut Individual, process_elements: &mut HashMap<String, (String, String)>, ctx: &mut CleanerContext) {
    info!(
        "{}, created = {}, id = {}",
        process.get_first_literal("rdf:type").unwrap_or_default(),
        NaiveDateTime::from_timestamp(process.get_first_datetime("v-s:created").unwrap_or_default(), 0).format("%d.%m.%Y %H:%M:%S"),
        process.get_id()
    );
    process_elements.insert(process.get_id().to_owned(), ("Process".to_owned(), parent_id.to_owned()));
    collect_work_items(process, process_elements, ctx);

    info!("process_elements={:?}", process_elements);
}

fn collect_work_items(process: &mut Individual, process_elements: &mut HashMap<String, (String, String)>, ctx: &mut CleanerContext) {
    for w_id in ctx
        .ch_client
        .select(
            &ctx.systicket.user_uri,
            &format!("SELECT DISTINCT id FROM veda_tt.`v-wf:WorkItem` WHERE v_wf_forProcess_str[1] = '{}'", process.get_id()),
            1000000,
            1000000,
            0,
        )
        .result
        .iter()
    {
        process_elements.insert(w_id.to_owned(), ("WorkItem".to_owned(), process.get_id().to_owned()));

        collect_work_orders_and_vars(w_id, process_elements, ctx);
    }
}

fn collect_work_orders_and_vars(work_item_id: &str, process_elements: &mut HashMap<String, (String, String)>, ctx: &mut CleanerContext) {
    if let Some(work_item) = ctx.module.get_individual(work_item_id, &mut Individual::default()) {
        if let Some(v_ids) = work_item.get_literals("v-wf:inVars") {
            for v_id in v_ids {
                process_elements.insert(v_id.to_owned(), ("InVar".to_owned(), work_item.get_id().to_owned()));
            }
        }
        for work_order_id in work_item.get_literals("v-wf:workOrderList").unwrap_or_default().iter() {
            process_elements.insert(work_order_id.to_owned(), ("WorkOrder".to_owned(), work_item.get_id().to_owned()));

            if let Some(work_order) = ctx.module.get_individual(&work_order_id, &mut Individual::default()) {
                if let Some(v_ids) = work_order.get_literals("v-wf:outVars") {
                    for v_id in v_ids {
                        process_elements.insert(v_id.to_owned(), ("OutVar".to_owned(), work_order.get_id().to_owned()));
                    }
                }

                if let Some(inner_process_id) = work_order.get_first_literal("v-wf:isProcess") {
                    if let Some(process) = ctx.module.get_individual(&inner_process_id, &mut Individual::default()) {
                        collect_process_elements(work_order.get_id(), process, process_elements, ctx);
                    }
                }
            }
        }
    }
}
