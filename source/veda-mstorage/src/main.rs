#[macro_use]
extern crate log;

use chrono::Utc;
use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde_json::value::Value as JSONValue;
use std::collections::HashMap;
use std::str;
use v_api::{IndvOp, ResultCode};
use v_authorization::{Access, Trace};
use v_az_lmdb::_authorize;
use v_module::module::{init_log, Module};
use v_module::ticket::Ticket;
use v_onto::individual::{Individual, RawObj};
use v_onto::datatype::Lang;
use v_onto::individual2msgpack::to_msgpack;
use v_onto::json2individual::parse_json_to_individual;
use v_queue::queue::Queue;
use v_queue::record::{Mode, MsgType};
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

    let mut sys_ticket = Ticket::default();
    if let Ok(ticket_id) = Module::get_sys_ticket_id_from_db(&mut storage) {
        get_ticket_from_db(&ticket_id, &mut sys_ticket, &mut storage);
    } else {
        error!("system ticket not found");
        return Ok(());
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

    let mut queue_out = Queue::new("./data", "individuals-flowz", Mode::ReadWrite).expect("!!!!!!!!! FAIL QUEUE");

    let mut tickets_cache: HashMap<String, Ticket> = HashMap::new();

    let mut op_id = 0;

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = request_prepare(&sys_ticket, &mut op_id, &recv_msg, &mut storage, &mut queue_out, &mut tickets_cache);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }
}

fn request_prepare(
    sys_ticket: &Ticket,
    op_id: &mut i64,
    request: &Message,
    storage: &mut VStorage,
    queue_out: &mut Queue,
    tickets_cache: &mut HashMap<String, Ticket>,
) -> Message {
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
                operation_prepare(&cmd, op_id, &ticket, event_id, src, assigned_subsystems, &mut indv, storage, queue_out, sys_ticket);
            }
        }
    } else {
        error!("field [individuals] is empty");
        return Message::default();
    }

    Message::default()
}

fn operation_prepare(
    cmd: &IndvOp,
    op_id: &mut i64,
    ticket: &Ticket,
    event_id: Option<&str>,
    src: Option<&str>,
    assigned_subsystems: Option<i64>,
    indv: &mut Individual,
    storage: &mut VStorage,
    queue_out: &mut Queue,
    sys_ticket: &Ticket,
) -> (ResultCode, i64) {
    let is_need_authorize;

    if sys_ticket.user_uri == ticket.user_uri {
        is_need_authorize = false;
    } else {
        is_need_authorize = true;
    }

    if indv.get_id().is_empty() || indv.get_id().len() < 2 {
        return (ResultCode::InvalidIdentifier, -1);
    }

    if indv.is_empty() || cmd != &IndvOp::Remove {
        return (ResultCode::NoContent, -1);
    }

    let mut new_indv = Individual::default();
    let mut prev_indv = Individual::default();
    let prev_state = storage.get_raw_value(StorageId::Individuals, new_indv.get_id());

    if !prev_state.is_empty() {
        prev_indv = Individual::new_raw(RawObj::new(prev_state.clone()));
        if new_indv.is_empty() || cmd == &IndvOp::Remove {
            new_indv.set_id(prev_indv.get_id());
        }
    }

    if prev_indv.is_empty() && cmd == &IndvOp::AddIn || cmd == &IndvOp::SetIn || cmd == &IndvOp::RemoveFrom {
        error!("fail store, cmd={:?}: not read prev_state uri={}", cmd, new_indv.get_id());
        return (ResultCode::FailStore, -1);
    }

    let mut trace = Trace {
        acl: &mut String::new(),
        is_acl: false,
        group: &mut String::new(),
        is_group: false,
        info: &mut String::new(),
        is_info: false,
        str_num: 0,
    };

    if cmd == &IndvOp::Remove {
        let is_deleted = new_indv.is_exists("v-s:deleted");

        if is_need_authorize && _authorize(new_indv.get_id(), &ticket.user_uri, Access::CanDelete as u8, true, &mut trace).unwrap_or(0) != Access::CanDelete as u8 {
            error!("fail store, Not Authorized, user {} request [can delete] {} ", ticket.user_uri, new_indv.get_id());
            return (ResultCode::NotAuthorized, -1);
        }
    } else {
        if is_need_authorize {
            if _authorize(new_indv.get_id(), &ticket.user_uri, Access::CanUpdate as u8, true, &mut trace).unwrap_or(0) != Access::CanUpdate as u8 {
                error!("fail store, Not Authorized, user {} request [can update] {} ", ticket.user_uri, new_indv.get_id());
                return (ResultCode::NotAuthorized, -1);
            }

            // check access can_create for new types
            let prev_types = prev_indv.get_literals("rdf:type").unwrap_or_default();
            let mut added_types = vec![];
            if let Some(new_types) = new_indv.get_literals("rdf:type") {
                for n_el in prev_types.iter() {
                    let mut found = false;
                    for p_el in new_types.iter() {
                        if p_el == n_el {
                            found = true;
                        }
                    }
                    if !found {
                        added_types.push(n_el);
                    }
                }
            } else {
                error!("fail store, Not Authorized, user {} request [can update] for {} ", ticket.user_uri, new_indv.get_id());
                return (ResultCode::NotAuthorized, -1);
            }

            for type_id in added_types.iter() {
                if _authorize(type_id, &ticket.user_uri, Access::CanCreate as u8, true, &mut trace).unwrap_or(0) != Access::CanCreate as u8 {
                    error!("fail store, Not Authorized, user {} request [can create] for {} ", ticket.user_uri, type_id);
                    return (ResultCode::NotAuthorized, -1);
                }
            }
            // end authorize
        }
    }

    if cmd != &IndvOp::Remove {
        let update_counter = prev_indv.get_first_integer("v-s:updateCounter").unwrap_or(0) + 1;

        if cmd == &IndvOp::AddIn || cmd == &IndvOp::SetIn || cmd == &IndvOp::RemoveFrom {
            indv_apply_cmd(cmd, &mut prev_indv, &mut new_indv);
        }

        new_indv.set_integer("v-s:updateCounter", update_counter);

        // store to main storage
        let mut new_state: Vec<u8> = Vec::new();
        if to_msgpack(&new_indv, &mut new_state).is_ok() && storage.put_kv_raw(StorageId::Individuals, new_indv.get_id(), new_state.clone()) {
            info!("store individual, id={}", new_indv.get_id());
        } else {
            error!("fail store individual, id={}", new_indv.get_id());
            return (ResultCode::FailStore, -1);
        }

        // store to queue

        let mut queue_element = Individual::default();
        queue_element.set_id(&format!("{}", op_id));
        queue_element.set_integer("cmd", cmd.to_i64());
        queue_element.set_uri("uri", new_indv.get_id());

        if !ticket.user_uri.is_empty() {
            queue_element.set_uri("user_uri", &ticket.user_uri);
        }

        if !new_state.is_empty() {
            queue_element.set_binary("new_state", new_state);
        }

        if !prev_state.is_empty() {
            queue_element.set_binary("prev_state", prev_state);
        }

        if let Some (v) = event_id {
            queue_element.set_string("event_id", v, Lang::NONE);
        }

        queue_element.set_integer("tnx_id", *op_id);

        let src = if let Some (v) = src {
            if v.is_empty() {
                "?"
            } else {
                v
            }
        } else {
            "?"
        };
        queue_element.set_string("src", "?", Lang::NONE);

        //queue_element.add_integer("date", date);

        info!("add to queue: uri={}", new_indv.get_id());

        let mut raw1: Vec<u8> = Vec::new();
        if let Err(e) = to_msgpack(&queue_element, &mut raw1) {
            error!("fail serialize, err={:?}", e);
            return (ResultCode::FailStore, -1);
        }
        if let Err(e) = queue_out.push(&raw1, MsgType::Object) {
            error!("fail push into queue, err={:?}", e);
            return (ResultCode::FailStore, -1);
        }

        *op_id += 1;
    }

    (ResultCode::FailStore, -1)
}

fn indv_apply_cmd(cmd: &IndvOp, prev_indv: &mut Individual, indv: &mut Individual) {
    if !prev_indv.is_empty() {
        let list_predicates = indv.get_predicates();

        for predicate in list_predicates {
            if cmd == &IndvOp::AddIn {
                // add value to set or ignore if exists
                prev_indv.apply_predicate_as_add_unique(&predicate, indv);
            } else if cmd == &IndvOp::SetIn {
                // set value to predicate
                prev_indv.apply_predicate_as_set(&predicate, indv);
            } else if cmd == &IndvOp::RemoveFrom {
                // remove predicate or value in set
                prev_indv.apply_predicate_as_remove(&predicate, indv);
            }
        }
    }
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
