#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate uuid;
use num_traits::{FromPrimitive, ToPrimitive};
use std::collections::HashMap;
use std::str::*;
use uuid::*;
use v_api::*;
use v_module::module::Module;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

const TRANSMIT_FAILED: i64 = 32;

#[derive(Primitive, PartialEq, Debug, Clone)]
#[repr(i64)]
pub enum ExImCode {
    Unknown = 0,
    Ok = 1,
    InvalidMessage = 2,
    InvalidCmd = 4,
    InvalidTarget = 8,
    FailUpdate = 16,
    TransmitFailed = TRANSMIT_FAILED,
    SendFailed = 64 | TRANSMIT_FAILED,
    ReceiveFailed = 128 | TRANSMIT_FAILED,
}

impl From<i64> for ExImCode {
    fn from(value: i64) -> Self {
        if let Some(v) = ExImCode::from_i64(value) {
            v
        } else {
            ExImCode::Unknown
        }
    }
}

impl From<ExImCode> for i64 {
    fn from(value: ExImCode) -> Self {
        if let Some(v) = value.to_i64() {
            v
        } else {
            0
        }
    }
}

impl ExImCode {
    pub fn as_string(&self) -> String {
        match self {
            ExImCode::Ok => "ok",
            ExImCode::InvalidMessage => "invalid message",
            ExImCode::InvalidCmd => "invalid cmd",
            ExImCode::InvalidTarget => "invalid target",
            ExImCode::FailUpdate => "fail update",
            ExImCode::TransmitFailed => "fail transmit",
            ExImCode::SendFailed => "fail send",
            ExImCode::ReceiveFailed => "fail receive",
            // ...
            ExImCode::Unknown => "unknown",
        }
        .to_string()
    }
}

pub fn enc_slave_resp(uri: &str, code: ExImCode) -> String {
    let q: i64 = code.into();
    uri.to_owned() + "," + &q.to_string()
}

pub fn dec_slave_resp(msg: &[u8]) -> (&str, ExImCode) {
    let mut iter = msg.split(|ch| *ch == b',');

    if let Some(wuri) = iter.next() {
        if let Ok(uri) = from_utf8(wuri) {
            if let Some(wcode) = iter.next() {
                if let Ok(s) = from_utf8(wcode) {
                    if let Ok(code) = s.parse::<i64>() {
                        return (uri, code.into());
                    }
                }
            }
        }
    }
    ("?", ExImCode::Unknown)
}

pub fn get_linked_nodes(module: &mut Module, node_upd_counter: &mut i64, link_node_addresses: &mut HashMap<String, String>) {
    let mut node = Individual::default();

    if module.storage.set_binobj("cfg:standart_node", &mut node) {
        if let Ok(c) = node.get_first_integer("v-s:updateCounter") {
            if c > *node_upd_counter {
                link_node_addresses.clear();
                if let Ok(v) = node.get_literals("cfg:linked_node") {
                    for el in v {
                        let mut link_node = Individual::default();

                        if module.storage.set_binobj(&el, &mut link_node) && !link_node.is_exists("v-s:delete") {
                            if let Ok(addr) = link_node.get_first_literal("rdf:value") {
                                link_node_addresses.insert(el, addr);
                            }
                        }
                    }
                    info!("linked nodes: {:?}", link_node_addresses);
                }
                *node_upd_counter = c;
            }
        }
    }
}

pub fn get_db_id(module: &mut Module) -> Option<String> {
    let mut indv = Individual::default();
    if module.storage.set_binobj("cfg:system", &mut indv) {
        if let Ok(c) = indv.get_first_literal("cfg:id") {
            return Some(c);
        }
    }
    None
}

pub fn create_db_id(module: &mut Module) -> Option<String> {
    let systicket;
    if let Ok(t) = module.storage.get_sys_ticket_id() {
        systicket = t;
    } else {
        error!("fail get systicket");
        return None;
    }

    let uuid1 = Uuid::new_v4().to_hyphenated().to_string();
    info!("create new db id = {}", uuid1);

    let mut new_indv = Individual::default();
    new_indv.obj.uri = "cfg:system".to_owned();
    new_indv.obj.add_string("cfg:id", &uuid1, Lang::NONE, 0);

    let res = module.api.update(&systicket, IndvOp::Put, &mut new_indv);

    if res.result != ResultCode::Ok {
        error!("fail update, uri={}, result_code={:?}", new_indv.obj.uri, res.result);
    } else {
        return Some(uuid1);
    }

    None
}
