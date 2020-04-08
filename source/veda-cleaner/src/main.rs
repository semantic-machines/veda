mod v_s_email;
mod v_s_membership;
mod v_s_permissionstatement;

#[macro_use]
extern crate log;

use crate::v_s_email::*;
use crate::v_s_membership::*;
use crate::v_s_permissionstatement::*;
use ini::Ini;
use std::collections::HashSet;
use std::time::*;
use std::{env, thread};
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

    let mut cleaner_modules = HashSet::new();
    let args: Vec<String> = env::args().collect();
    for el in args.iter() {
        if el.starts_with("--modules") {
            let p: Vec<&str> = el.split('=').collect();
            if p.len() == 2 {
                for s in p[1].split(',') {
                    cleaner_modules.insert(s);
                }
            }
        }
    }

    info!("use modules: {:?}", cleaner_modules);

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
            if cleaner_modules.contains("v_s_email") {
                clean_email(&systicket, &mut ch_client, &mut module);
            } else if cleaner_modules.contains("v_s_permissionstatement") {
                clean_invalid_permissionstatement(&systicket, &mut ch_client, &mut module);
            } else if cleaner_modules.contains("v_s_membership") {
                clean_invalid_membership(&systicket, &mut ch_client, &mut module);
            }
            thread::sleep(Duration::from_millis(10000));
        }
    }

    // Ok(())
}
