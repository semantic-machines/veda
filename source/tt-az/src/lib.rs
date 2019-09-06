extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate tarantool_rust_api;
#[macro_use]
extern crate log;

use std::io;
use std::vec::Vec;
use tarantool_rust_api::tarantool::api::*;
use v_authorization::*;
use v_onto::individual::*;

static PRIMARY_INDEX: &str = "primary";

pub struct TTIStorage<'a> {
    tarantool: &'a TarantoolContext,
    space: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RowTypeStruct {
    pub name: String,
    pub data: Option<String>,
}

impl<'a> TTIStorage<'a> {
    fn get_impl(&self, key: &str) -> io::Result<Option<RowTypeStruct>> {
        let key = (key,);
        match self.tarantool.index_get(&self.space, PRIMARY_INDEX, &key)? {
            Some(tuple) => Ok(tuple.decode()?),
            None => Ok(None),
        }
    }

    fn get_individual_from_db(&self, uri: &str, iraw: &mut Individual) -> bool {
        let key = (uri,);
        if let Ok(r) = self.tarantool.index_get(&self.space, PRIMARY_INDEX, &key) {
            match r {
                Some(tuple) => match tuple.decode() {
                    Ok(v) => {
                        iraw.raw.data = v;
                        return true;
                    }
                    Err(e) => {
                        error!("get_individual_from_db, err={}", e);
                    }
                },
                None => return false,
            }
        }
        false
    }

    fn put(&self, key: &str, value: &str) -> io::Result<bool> {
        let val = RowTypeStruct {
            name: key.to_string(),
            data: Some(value.to_string()),
        };
        self.tarantool.txn_begin()?;
        self.tarantool.insert(&self.space, &val)?;
        self.tarantool.txn_commit()?;
        Ok(true)
    }
}

impl<'a> Storage for TTIStorage<'a> {
    fn get(&self, key: &str) -> Result<String, i64> {
        match self.get_impl(key) {
            Ok(v) => {
                if let Some(s) = v {
                    if let Some(d) = s.data {
                        return Ok(d);
                    }
                }
                Err(0)
            }
            Err(_e) => Err(-1),
        }
    }

    fn fiber_yield(&self) {
        self.tarantool.fiber_yield();
    }
}

fn authorization_impl(tarantool: &TarantoolContext) -> io::Result<Vec<u8>> {
    let (p_uri, p_ticket, p_request_access): (Option<String>, Option<String>, Option<u8>) = tarantool.decode_input_params()?;

    let mut result: Vec<u8> = Vec::new();

    if p_uri.is_none() || p_ticket.is_none() || p_request_access.is_none() {
        return Ok(result);
    }

    let uri = p_uri.unwrap();
    let ticket = p_ticket.unwrap();
    let request_access = p_request_access.unwrap();
    let mut user_uri = "".to_owned();

    info!("uri={}, user_uri={}, request_access={}\n", uri, user_uri, request_access);
    let filter_value = "";
    let filter_allow_access_to_other = 0;

    let mut trace = Trace {
        acl: &mut String::new(),
        is_acl: false,
        group: &mut String::new(),
        is_group: false,
        info: &mut String::new(),
        is_info: false,
        str_num: 0,
    };

    let tickets_cache_storage = TTIStorage {
        tarantool: tarantool,
        space: "TICKETS_CACHE".to_owned(),
    };

    if let Ok(v) = tickets_cache_storage.get(&ticket) {
        user_uri = v;
    } else {
        let tickets_storage = TTIStorage {
            tarantool: tarantool,
            space: "TICKETS".to_owned(),
        };

        let mut ticket_obj = Individual::default();
        if tickets_storage.get_individual_from_db(&ticket, &mut ticket_obj) {
            if let Ok(s) = ticket_obj.get_first_literal("user_uri") {
                if let Err(e) = tickets_cache_storage.put(&ticket, &s.clone()) {
                    error!("fail update TICKETS_CACHE, err={:?}", e);
                }
                user_uri = s;
            } else {
                error!("[user_uri] not found in ticket: {}", ticket_obj);
            }
        } else {
            error!("ticket not found {}", ticket);
        }
    }

    if user_uri.len() < 2 {
        error!("invalid user_uri {}", user_uri);
        return Ok(result);
    }

    let acl_storage = TTIStorage {
        tarantool: tarantool,
        space: "ACL_INDEX".to_owned(),
    };

    match authorize(&uri, &user_uri, request_access, &filter_value, filter_allow_access_to_other, &acl_storage, &mut trace) {
        Ok(res) => {
            result.push(res);
            return Ok(result);
        }
        Err(_e) => {}
    }

    Ok(result)
}

tarantool_register_stored_procs! {
    authorization => authorization_impl
}
