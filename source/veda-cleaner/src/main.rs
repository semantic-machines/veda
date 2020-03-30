mod clean_email;

#[macro_use]
extern crate log;

use crate::clean_email::*;
use ini::Ini;
use std::thread;
use std::time::*;
use v_module::module::{init_log, Module};
use v_search::clickhouse_client::*;

#[tokio::main]
async fn main() {
    init_log();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");
    let query_search_db = section.get("query_search_db").expect("param [query_search_db_url] not found in veda.properties");

    let mut module = Module::default();

    let mut ch_client = CHClient::new(query_search_db.to_owned());

    loop {
        if ch_client.connect() {
            break;
        }
        thread::sleep(Duration::from_millis(10000));
    }

    info!("cleaner started");

    if let Ok(t) = module.get_sys_ticket_id() {
        let systicket = module.get_ticket_from_db(&t);

        loop {
            clean_email(&systicket, &mut ch_client, &mut module);
            thread::sleep(Duration::from_millis(10000));
        }
    }

    // Ok(())
}
