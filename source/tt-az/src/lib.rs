extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate tarantool_rust_api;

use std::io;
use std::vec::Vec;
use tarantool_rust_api::tarantool::api::*;
use v_authorization::*;

static ACL_SPACE: &str = "ACL";
static PRIMARY_INDEX: &str = "primary";

pub struct TTEStorage<'a> {
    tarantool: &'a TarantoolContext,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RowTypeStruct {
    pub name: String,
    pub data: Option<String>,
}

impl<'a> TTEStorage<'a> {
    fn get_impl(&self, key: &str) -> io::Result<Option<RowTypeStruct>> {
        let key = (key,);
        match self.tarantool.index_get(ACL_SPACE, PRIMARY_INDEX, &key)? {
            Some(tuple) => Ok(tuple.decode()?),
            None => Ok(None),
        }
    }
}

impl<'a> Storage for TTEStorage<'a> {
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
    let (p_uri, p_user_uri, p_request_access): (Option<String>, Option<String>, Option<u8>) = tarantool.decode_input_params()?;

    let mut result: Vec<u8> = Vec::new();

    if p_uri.is_none() || p_user_uri.is_none() || p_request_access.is_none() {
        return Ok(result);
    }

    let uri = p_uri.unwrap();
    let user_uri = p_user_uri.unwrap();
    let request_access = p_request_access.unwrap();

    println!("uri={}, user_uri={}, request_access={}\n", uri, user_uri, request_access);
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

    let storage = TTEStorage {
        tarantool: tarantool,
    };

    match authorize(&uri, &user_uri, request_access, &filter_value, filter_allow_access_to_other, &storage, &mut trace) {
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
