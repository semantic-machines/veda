extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate tarantool_rust_api;

use std::io;
use std::vec::Vec;
use tarantool_rust_api::tarantool::api::*;
use v_authorization::*;
use v_onto::{individual::*, parser::*};

static PRIMARY_INDEX: &str = "primary";

pub struct TTIStorage<'a> {
    tarantool: &'a TarantoolContext,
    space: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct KVGet {
    pub name: String,
    pub data: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct KVPut {
    pub name: String,
    pub data: String,
}

impl<'a> TTIStorage<'a> {
    fn get_impl(&self, key: &str) -> io::Result<Option<KVGet>> {
        let key = (key,);
        match self.tarantool.index_get(&self.space, PRIMARY_INDEX, &key)? {
            Some(tuple) => Ok(tuple.decode()?),
            None => Ok(None),
        }
    }

    fn get_raw(&self, uri: &str) -> io::Result<Vec<u8>> {
        let key = (uri,);
        match self.tarantool.index_get(&self.space, PRIMARY_INDEX, &key)? {
            Some(tuple) => return Ok(tuple.get_raw_data()),
            None => return Ok(vec![]),
        }
    }

    fn put(&self, key: &str, value: &str) -> io::Result<bool> {
        let val = KVPut {
            name: key.to_string(),
            data: value.to_string(),
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

fn get_individual_impl(tarantool: &TarantoolContext) -> io::Result<Vec<u8>> {
    let (p_ticket, p_uri): (Option<String>, Option<String>) = tarantool.decode_input_params()?;

    let mut result = Vec::new();

    if p_uri.is_none() || p_ticket.is_none() {
        return Ok(result);
    }

    let uri = p_uri.unwrap();
    let ticket = p_ticket.unwrap();
    let request_access = 15;
    let mut user_uri = "".to_owned();

    println!("uri={}, ticket={}, request_access={}", uri, ticket, request_access);
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
        println!("found in cache: user_uri={}", user_uri);
    } else {
        println!("seek user from db TICKETS...");
        let tickets_storage = TTIStorage {
            tarantool: tarantool,
            space: "TICKETS".to_owned(),
        };

        if let Ok(v) = tickets_storage.get_raw(&ticket) {
            let mut ticket_obj = Individual::new_raw(RawObj::new(v));

            if parse_raw(&mut ticket_obj).is_ok() {

                if let Some(s) = ticket_obj.get_first_literal("ticket:accessor") {
                    if let Err(e) = tickets_cache_storage.put(&ticket, &s.clone()) {
                        println!("fail update TICKETS_CACHE, err={:?}", e);
                    }
                    user_uri = s;
                } else {
                    println!("[user_uri] not found in ticket: {}", ticket_obj);
                }
            }
        } else {
            println!("ticket not found {}", ticket);
        }
    }

    if user_uri.len() < 2 {
        println!("invalid user_uri {}", user_uri);
        return Ok(result);
    }

    let acl_storage = TTIStorage {
        tarantool: tarantool,
        space: "ACL_INDEX".to_owned(),
    };
    match authorize(&uri, &user_uri, request_access, &filter_value, filter_allow_access_to_other, &acl_storage, &mut trace) {
        Ok(res) => {
            let individuals_storage = TTIStorage {
                tarantool: tarantool,
                space: "INDIVIDUALS".to_owned(),
            };
            result = individuals_storage.get_raw(&uri)?;
        }
        Err(_e) => {}
    }
    return Ok(result);
}

tarantool_register_stored_procs! {
    get_individual => get_individual_impl
}
