#[macro_use]
extern crate log;

use chrono::Utc;
use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde_json::value::Value as JSONValue;
use std::collections::HashMap;
use std::str;
use v_api::{IndvOp, ResultCode};
use v_module::module::{init_log, Module};
use v_module::ticket::Ticket;
use v_onto::individual::Individual;
use v_onto::json2individual::parse_json_to_individual;
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

    let mut tickets_cache: HashMap<String, Ticket> = HashMap::new();

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = request_prepare(&recv_msg, &mut storage, &mut tickets_cache);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }
}

fn request_prepare(request: &Message, storage: &mut VStorage, tickets_cache: &mut HashMap<String, Ticket>) -> Message {
    let v: JSONValue = if let Ok(v) = serde_json::from_slice(request.as_slice()) {
        v
    } else {
        JSONValue::Null
    };

    let fticket = v["ticket"].as_str();
    if fticket.is_none() {
        error!("field [ticket] not found in request");
        return Message::default();
    }
    let ticket_id = fticket.unwrap();
    let mut ticket = Ticket::default();

    if let Some(cached_ticket) = tickets_cache.get(ticket_id) {
        ticket = cached_ticket.clone();
    } else {
        get_ticket_from_db(ticket_id, &mut ticket, storage);
        if ticket.result != ResultCode::Ok {
            error!("ticket [{}] not found in storage", ticket_id);
            return Message::default();
        }
        tickets_cache.insert(ticket_id.to_string(), ticket.clone());
    }

    if !is_ticket_valid(&mut ticket) {
        error!("ticket [{}] not valid", ticket.id);
        return Message::default();
    }

    let assigned_subsystems = v["assigned_subsystems"].as_i64();
    let event_id = v["event_id"].as_str();
    let src = v["src"].as_str();

    let mut cmd = IndvOp::None;

    match v["function"].as_str().unwrap_or_default() {
        "put" => {
            cmd = IndvOp::Put;
        }
        "remove" => {
            cmd = IndvOp::Remove;
        }
        "add_to" => {
            cmd = IndvOp::AddIn;
        }
        "set_in" => {
            cmd = IndvOp::SetIn;
        }
        "remove_from" => {
            cmd = IndvOp::RemoveFrom;
        }
        _ => {
            error!("unknown command {:?}", v["function"].as_str());
            return Message::default();
        }
    }

    if let Some(jindividuals) = v["individuals"].as_array() {
        for el in jindividuals {
            let mut indv = Individual::default();
            if !parse_json_to_individual(el, &mut indv) {
                error!("fail parse individual fro json");
                return Message::default();
            } else {
                individual_prepare(&ticket, &cmd, &mut indv, storage);
            }
        }
    } else {
        error!("field [individuals] is empty");
        return Message::default();
    }

    Message::default()
}

fn individual_prepare(ticket: &Ticket, cmd: &IndvOp, indv: &mut Individual, storage: &mut VStorage) -> (ResultCode, i64) {
    if indv.get_id().is_empty() || indv.get_id().len() < 2 {
        return (ResultCode::InvalidIdentifier, -1);
    }

    if indv.is_empty() || cmd != &IndvOp::Remove {
        return (ResultCode::NoContent, -1);
    }

    let mut prev_state = Individual::default();
    if storage.get_individual(indv.get_id(), &mut prev_state) {
    } else {
        if prev_state.is_empty() && cmd == &IndvOp::AddIn || cmd == &IndvOp::SetIn || cmd == &IndvOp::RemoveFrom {
            error!("store, cmd={:?}: not read prev_state uri={}", cmd, indv.get_id());
        }
    }

    (ResultCode::FailStore, -1)
}

fn get_ticket_from_db(id: &str, dest: &mut Ticket, storage: &mut VStorage) {
    let mut indv = Individual::default();
    if storage.get_individual_from_db(StorageId::Tickets, id, &mut indv) {
        dest.update_from_individual(&mut indv);
        dest.result = ResultCode::Ok;
    }
}

fn is_ticket_valid(ticket: &mut Ticket) -> bool {
    if ticket.result != ResultCode::Ok {
        return false;
    }

    if Utc::now().timestamp() > ticket.end_time {
        ticket.result = ResultCode::TicketExpired;
        return false;
    }

    if ticket.user_uri.is_empty() {
        ticket.result = ResultCode::NotReady;
        return false;
    }

    true
}
