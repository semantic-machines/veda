mod v_s_email;
mod v_s_membership;
mod v_s_membership1;
mod v_s_permissionstatement;
mod v_wf_process;

#[macro_use]
extern crate log;

use crate::v_s_email::*;
use crate::v_s_membership::*;
use crate::v_s_membership1::*;
use crate::v_s_permissionstatement::*;
use crate::v_wf_process::*;
use ini::Ini;
use std::collections::HashSet;
use std::fs::{File, OpenOptions};
use std::time::*;
use std::{env, thread};
use v_module::module::{init_log, Module};
use v_module::ticket::Ticket;
use v_search::clickhouse_client::*;

pub struct CleanerContext {
    module: Module,
    ch_client: CHClient,
    systicket: Ticket,
    report_type: String,
    report: Option<File>,
}

#[tokio::main]
async fn main() {
    init_log();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");
    let query_search_db = section.get("query_search_db").expect("param [query_search_db_url] not found in veda.properties");

    let mut ctx = CleanerContext {
        module: Module::default(),
        ch_client: CHClient::new(query_search_db.to_owned()),
        systicket: Ticket::default(),
        report_type: "".to_owned(),
        report: None,
    };

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
        } else if el.starts_with("--report") {
            let p: Vec<&str> = el.split('=').collect();
            if p.len() == 2 {
                ctx.report_type = p[1].to_owned();
            }
        }
    }

    if !ctx.report_type.is_empty() {
        match OpenOptions::new().read(true).write(true).create(true).open("report.txt") {
            Ok(ff) => {
                ctx.report = Some(ff);
            }
            Err(e) => error!("error={:?}", e),
        }
    }

    info!("use modules: {:?}", cleaner_modules);

    loop {
        if ctx.ch_client.connect() {
            break;
        }
        thread::sleep(Duration::from_millis(10000));
    }

    info!("cleaner started");

    if let Ok(t) = ctx.module.get_sys_ticket_id() {
        ctx.systicket = ctx.module.get_ticket_from_db(&t);
        {
            if cleaner_modules.contains("email") {
                clean_email(&mut ctx);
            } else if cleaner_modules.contains("permissionstatement") {
                clean_invalid_permissionstatement(&mut ctx);
            } else if cleaner_modules.contains("membership") {
                clean_invalid_membership(&mut ctx);
            } else if cleaner_modules.contains("membership1") {
                remove_membership1(&mut ctx);
            } else if cleaner_modules.contains("process") {
                clean_process(&mut ctx);
            }
            //thread::sleep(Duration::from_millis(1000));
        }
    }
}
