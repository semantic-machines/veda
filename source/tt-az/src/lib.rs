extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate tarantool_rust_api;

use serde::ser::{Serialize, SerializeMap, Serializer};
use std::io;
use std::vec::Vec;
use tarantool_rust_api::tarantool::api::*;
use v_authorization::*;

#[derive(Deserialize, Clone, Debug)]
pub struct CountryData {
    pub country_code: u32,
    pub name: String,
    pub region: String,
    pub sub_region: String,
}

static TEST_SPACE: &str = "test_space";
static PRIMARY_INDEX: &str = "primary";

pub struct TTEStorage<'a> {
    tarantool: &'a TarantoolContext,
}

#[derive(Deserialize, Clone, Debug)]
pub struct TestStruct {
    pub a: String,
}

impl Serialize for TestStruct {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("a", &self.a)?;
        map.end()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RowTypeStruct {
    pub id: u32,
    pub name: String,
    pub data: Option<TestStruct>,
}

impl<'a> TTEStorage<'a> {
    fn get_impl(&self, key: &str) -> io::Result<Option<RowTypeStruct>> {
        let key: (&str,) = (key,);
        match self.tarantool.index_get(TEST_SPACE, PRIMARY_INDEX, &key)? {
            Some(tuple) => Ok(tuple.decode()?),
            None => Ok(None),
        }
    }
}

impl<'a> Storage for TTEStorage<'a> {
    fn get(&self, key: &str) -> Result<String, i64> {
        println!("@get key={}", key);
        match self.get_impl(key) {
            Ok(v) => {
                println!("@get, Ok 1 v={:?}", v);
                if let Some(s) = v {
                    if let Some(d) = s.data {
                        return Ok(d.a);
                    }
                }
                println!("@get, Err 0");
                Err(0)
            }
            Err(e) => {
                println!("@get, Err 1, {:?}", e);
                Err(-1)
            }
        }
    }

    fn fiber_yield(&self) {
        self.tarantool.fiber_yield();
    }
}

fn authorization_impl(tarantool: &TarantoolContext) -> io::Result<Vec<u8>> {
    let (p_uri, p_user_uri, p_request_access): (Option<String>, Option<String>, Option<u8>) = tarantool.decode_input_params()?;

    let result: Vec<u8> = Vec::new();

    if p_uri.is_none() || p_user_uri.is_none() || p_request_access.is_none() {
        return Ok(result);
    }

    let uri = p_uri.unwrap();
    let user_uri = p_user_uri.unwrap();
    let request_access = p_request_access.unwrap();

    println!("uri={}, user_uri={}, request_access={}\n", uri, user_uri, request_access);
    let request_access = 0;
    let filter_value = "?";
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
        Ok(_res) => return Ok(result),
        Err(_e) => {}
    }

    Ok(result)
}

tarantool_register_stored_procs! {
    authorization => authorization_impl
}
