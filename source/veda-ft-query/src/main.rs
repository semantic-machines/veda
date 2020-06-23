#[macro_use]
extern crate log;

use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde_json::value::Value as JSONValue;
use std::str;
use v_api::app::ResultCode;
use v_ft_xapian::xapian_reader::XapianReader;
use v_ft_xapian::xapian_vql::OptAuthorize;
use v_module::module::{init_log, Module};
use v_module::onto::load_onto;
use v_onto::onto::Onto;
use v_storage::storage::*;

fn main() {
    init_log();

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

    let query_url = section.get("ft_query_service_url").expect("param [search_query_url] not found in veda.properties");

    let mut module = Module::default();

    let mut onto = Onto::default();
    load_onto(&mut storage, &mut onto);

    if let Some(mut xr) = XapianReader::new("russian", &mut storage, onto) {
        let server = Socket::new(Protocol::Rep0).unwrap();
        if let Err(e) = server.listen(&query_url) {
            error!("fail listen {}, {:?}", query_url, e);
            return;
        }

        loop {
            if let Ok(recv_msg) = server.recv() {
                let out_msg = req_prepare(&mut module, &recv_msg, &mut xr);
                if let Err(e) = server.send(out_msg) {
                    error!("fail send answer, err={:?}", e);
                }
            }
        }
    }
}

const TICKET: usize = 0;
const QUERY: usize = 1;
const TOP: usize = 5;
const LIMIT: usize = 6;
const FROM: usize = 7;

fn req_prepare(module: &mut Module, request: &Message, xr: &mut XapianReader) -> Message {
    if let Ok(s) = str::from_utf8(request.as_slice()) {
        let v: JSONValue = if let Ok(v) = serde_json::from_slice(s.as_bytes()) {
            v
        } else {
            JSONValue::Null
        };

        if let Some(a) = v.as_array() {
            let ticket_id = a.get(TICKET).unwrap().as_str().unwrap_or_default();
            let mut query = a.get(QUERY).unwrap().as_str().unwrap_or_default().to_string();

            if !(query.find("==").is_some() || query.find("&&").is_some() || query.find("||").is_some()) {
                query = "'*' == '".to_owned() + &query + "'";
            }

            let top = a.get(TOP).unwrap().as_i64().unwrap_or_default() as i32;
            let limit = a.get(LIMIT).unwrap().as_i64().unwrap_or_default() as i32;
            let from = a.get(FROM).unwrap().as_i64().unwrap_or_default() as i32;

            let mut user_uri = "cfg:Guest".to_owned();
            if !ticket_id.is_empty() {
                let ticket = module.get_ticket_from_db(&ticket_id);
                if ticket.result == ResultCode::Ok {
                    user_uri = ticket.user_uri;
                }
            }

            let mut ctx = vec![];
            fn add_out_element(id: &str, ctx: &mut Vec<String>) {
                ctx.push(id.to_owned());
                debug!("id={:?}", id);
            }

            if let Ok(mut res) = xr.query(&user_uri, &query, "", "", from, top, limit, add_out_element, OptAuthorize::YES, &mut ctx) {
                res.result = ctx;
                debug!("res={:?}", res);
                if let Ok(s) = serde_json::to_string(&res) {
                    return Message::from(s.as_bytes());
                }
            }
        }
    }
    return Message::from("[]".as_bytes());
}
