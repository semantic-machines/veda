#[macro_use]
extern crate log;

use ini::Ini;
use std::{process, str};
use v_api::app::ResultCode;
//use v_ft_xapian::index_schema::IndexerSchema;
use v_ft_xapian::init_db_path;
use v_ft_xapian::key2slot::Key2Slot;
//use v_ft_xapian::xerror::Result;
use nng::{Message, Protocol, Socket};
use serde_json::value::Value as JSONValue;
use v_ft_xapian::vql::TTA;
use v_ft_xapian::xapian_reader::XapianReader;
use v_ft_xapian::xapian_vql::OptAuthorize;
use v_ft_xapian::xerror::XError::{Io, Xapian};
use v_module::info::ModuleInfo;
use v_module::module::{init_log, Module};
use v_module::onto::load_onto;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_storage::storage::*;
use xapian_rusty::{get_xapian_err_type, Stem};

const BASE_PATH: &str = "./data";

fn test() {
    init_log();

    let res = TTA::parse_expr("'rdf:type' == '*'");
    if let Some(bt) = res {
        println!("{}", bt.as_ref());
    }

    let module_info = ModuleInfo::new(BASE_PATH, "fulltext_indexer", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        process::exit(101);
    }

    let key2slot = Key2Slot::load();
    if key2slot.is_err() {
        error!("load key2slot, err={:?}", key2slot.err());
        return;
    }

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");

    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let tarantool_addr = if let Some(p) = section.get("tarantool_url") {
        p.to_owned()
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    if !tarantool_addr.is_empty() {
        info!("tarantool addr={}", &tarantool_addr);
    }

    let mut storage: VStorage;
    if !tarantool_addr.is_empty() {
        storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
    } else {
        storage = VStorage::new_lmdb("./data", StorageMode::ReadOnly);
    }

    let mut onto = Onto::default();
    load_onto(&mut storage, &mut onto);

    let mut xr = XapianReader {
        using_dbqp: Default::default(),
        opened_db: Default::default(),
        xapian_stemmer: Stem::new("russian").unwrap(),
        xapian_lang: "".to_string(),
        index_schema: Default::default(),
        //mdif: module_info.unwrap(),
        key2slot: key2slot.unwrap(),
        onto,
        db2path: init_db_path(),
    };

    xr.load_index_schema(&mut storage);
    let mut ctx = vec![];
    fn add_out_element(id: &str, ctx: &mut Vec<String>) {
        ctx.push(id.to_owned());
        info!("id={:?}", id);
    }

    if let Ok(res) = xr.query("cfg:VedaSystem", "'rdf:type' == 'v-s:Document'", "", "", 0, 0, 0, add_out_element, OptAuthorize::NO, &mut ctx) {
        info!("res={:?}", res);
    }
}

fn main() {
    init_log();

    test();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let query_url = "";

    let mut module = Module::default();

    let server = Socket::new(Protocol::Rep0).unwrap();
    if let Err(e) = server.listen(&query_url) {
        error!("fail listen {}, {:?}", query_url, e);
        return;
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let out_msg = req_prepare(&mut module, &recv_msg);
            if let Err(e) = server.send(out_msg) {
                error!("fail send answer, err={:?}", e);
            }
        }
    }
}

const TICKET: usize = 0;
const QUERY: usize = 1;
const TOP: usize = 5;
const LIMIT: usize = 6;
const FROM: usize = 7;

fn req_prepare(module: &mut Module, request: &Message) -> Message {
    if let Ok(s) = str::from_utf8(request.as_slice()) {
        let v: JSONValue = if let Ok(v) = serde_json::from_slice(s.as_bytes()) {
            v
        } else {
            JSONValue::Null
        };

        if let Some(a) = v.as_array() {
            let ticket_id = a.get(TICKET).unwrap().as_str().unwrap_or_default();
            let query = a.get(QUERY).unwrap().as_str().unwrap_or_default();

            let top = a.get(TOP).unwrap().as_i64().unwrap_or_default();
            let limit = a.get(LIMIT).unwrap().as_i64().unwrap_or_default();
            let from = a.get(FROM).unwrap().as_i64().unwrap_or_default();

            let mut user_uri = "cfg:Guest".to_owned();
            if !ticket_id.is_empty() {
                let ticket = module.get_ticket_from_db(&ticket_id);
                if ticket.result == ResultCode::Ok {
                    user_uri = ticket.user_uri;
                }
            }

            //            if let Ok(s) = serde_json::to_string(&res) {
            //                return Message::from(s.as_bytes());
            //            }
        }
    }
    return Message::from("[]".as_bytes());
}
