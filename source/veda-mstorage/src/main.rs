#[macro_use]
extern crate log;

use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde_json::value::Value as JSONValue;
use std::str;
use v_api::ResultCode;
use v_module::module::{init_log, Module};
use v_module::ticket::Ticket;
use v_onto::individual::Individual;
use v_storage::storage::*;

fn main() -> std::io::Result<()> {
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

    let param_name = "main_module_url";
    let main_module_url = Module::get_property(param_name);
    if main_module_url.is_none() {
        error!("not found param {} in properties file", param_name);
        return Ok(());
    }

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&main_module_url.unwrap()) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = req_prepare(&recv_msg, &mut storage);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }
}

fn req_prepare(request: &Message, storage: &mut VStorage) -> Message {
    let v: JSONValue = if let Ok(v) = serde_json::from_slice(request.as_slice()) {
        v
    } else {
        JSONValue::Null
    };

    let mut ticket = Ticket::default();

    if let Some(ticket_id) = v["ticket"].as_str() {
        get_ticket_from_db(ticket_id, &mut ticket, storage);
        if ticket.result != ResultCode::Ok {
            error!("ticket [{}] not found in storage", ticket_id);
            return Message::default();
        }
    } else {
        error!("field [ticket] not found in request");
        return Message::default();
    }

    if !is_ticket_valid(&mut ticket) {
        error!("ticket [{}] not valid", ticket.id);
        return Message::default();
    }

    let assigned_subsystems = v["assigned_subsystems"].as_i64();
    let event_id = v["event_id"].as_str();
    let src = v["src"].as_str();

    match v["function"].as_str().unwrap_or_default() {
        "put" => {}
        "remove" => {}
        "add_to" => {}
        "set_in" => {}
        "remove_from" => {}
        _ => {
            error!("unknown command {:?}", v["function"].as_str());
        }
    }

    Message::default()
}

fn get_ticket_from_db(id: &str, dest: &mut Ticket, storage: &mut VStorage) {
    let mut indv = Individual::default();
    if storage.get_individual_from_db(StorageId::Tickets, id, &mut indv) {
        dest.update_from_individual(&mut indv);
        dest.result = ResultCode::Ok;
    }
}

fn is_ticket_valid(ticket: &mut Ticket) -> bool {
    false
}
