use chrono::prelude::*;
use std::ops::Sub;
use time::Duration;
use v_api::app::ResultCode;
use v_api::IndvOp;
use v_module::module::Module;
use v_module::ticket::Ticket;
use v_onto::individual::Individual;
use v_search::clickhouse_client::CHClient;

const MAX_SIZE_BATCH: i64 = 1000;

pub fn clean_email(systicket: &Ticket, ch_client: &mut CHClient, module: &mut Module) {
    let date_before = Utc::now().naive_utc().sub(Duration::days(30));

    let query =
        format!("SELECT DISTINCT id FROM veda_tt.`v-s:Email` FINAL WHERE v_s_deleted_int[1] = 0 AND v_s_created_date[1] < toDateTime ({})", date_before.timestamp());
    let res = ch_client.select(&systicket.user_uri, &query, MAX_SIZE_BATCH, MAX_SIZE_BATCH, 0);

    if res.result_code == ResultCode::Ok {
        for el in res.result.iter() {
            info!("email id for remove: {}", el);
            module.api.update(&systicket.id, IndvOp::Remove, &Individual::default().set_id(el));
        }
    }
}
