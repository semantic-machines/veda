#![allow(non_snake_case)]
#[macro_use]    
extern crate serde_json;

#[macro_use] extern crate juniper;
/// This module represents handler for REST requests in Tarantool

extern crate core;
extern crate rmp_bind;

///Exports tarantool rest-api implementation for veda
mod rest;
///Exports bindings for lua
mod lua;

use std::os::raw::c_char;
use std::io::{ Cursor, Write, stderr };
use rmp_bind::{ decode, encode };
use lua::lua_State;

///Rest-perations code constants
const PUT: u64 = 1;
const GET: u64 = 2;
const GET_TICKET: u64 = 3;
const AUTHORIZE: u64 = 8;
const GET_RIGHTS_ORIGIN: u64 = 9;
const GET_MEMBERSHIP: u64 = 10;
const REMOVE: u64 = 51;

#[repr(C)]
pub struct Response {
    msg: *const c_char,
    size: usize
}


/// Fail function prints errpr messafe and error code to stderr log
pub fn fail(resp_msg: &mut Vec<u8>, code: rest::Codes, err_msg: String) {
    writeln!(&mut stderr(), "{0}", err_msg).unwrap();
    encode::encode_array(resp_msg, 1);
    encode::encode_uint(resp_msg, code as u64);
}


/// According to docs size must be at least for elements, else you get fail
fn unmarshal_request(cursor: &mut Cursor<&[u8]>, arr_size: u64, resp_msg: &mut Vec<u8>) {
    if arr_size < 4 {
        ///Minimal valid array size for request is 4
        fail(resp_msg, rest::Codes::BadRequest, "@INVALID MSGPACK SIZE < 4".to_string());
        return;
    }

    /// First for all requests its operation code
    let op_code: u64;
    match decode::decode_uint(cursor) {
        Err(err) => return fail(resp_msg, rest::Codes::BadRequest, err),
        Ok(op) => (op_code = op)
    }

    /// Second if flag for authorization
    let need_auth: bool;
    match decode::decode_bool(cursor) {
        Err(err) => return fail(resp_msg, rest::Codes::BadRequest, err),
        Ok(v) => (need_auth = v)
    }
    match op_code {
        PUT => rest::put(cursor, arr_size, need_auth, resp_msg),
        GET => rest::get(cursor, arr_size, need_auth, resp_msg),
        /// Auth request don't need need_auth flag
        /// But for the sake of generality request contains it always.AUTHORIZE
        /// Three rest requests need auth function with different params.
        /// AUTHORIZE: auth without data aggreagation.
        /// GET_RIGHTS_ORIGIN: auth with rights aggregation.
        /// GET_MEMBERSHIP: auth with membership aggregation.
        AUTHORIZE => rest::auth(cursor, arr_size, resp_msg, false, false),
        REMOVE => rest::remove(cursor, arr_size, need_auth, resp_msg),
        GET_RIGHTS_ORIGIN => rest::auth(cursor, arr_size, resp_msg, true, false),
        GET_MEMBERSHIP => rest::auth(cursor, arr_size, resp_msg, false, true),
        GET_TICKET => rest::get_ticket(cursor, arr_size, resp_msg),
        _ => fail(resp_msg, rest::Codes::BadRequest, format!("@ERR UNKNOWN REQUEST {0}", op_code))
    }
    // writeln!(stderr(), "@END REQUEST");
}


#[no_mangle]
/// Represents main entry from lua interpretator in Tarantool
/// Parses request array and gets it size
pub extern "C" fn db_handle_request(L: *mut lua_State) -> i32 {
    let mut msg: Vec<u8> = Vec::default();
    ///Getting data from lua stack
    lua::tolstring(L, -1, &mut msg);
    let mut cursor = Cursor::new(&msg[..]);
    let mut resp_msg = Vec::with_capacity(4096);
    
    match decode::decode_array(&mut cursor) {
        Err(err) => fail(&mut resp_msg, rest::Codes::InternalServerError, err),
        Ok(arr_size) => unmarshal_request(&mut cursor, arr_size, &mut resp_msg)
    }

    ///Push response to lua stack, so it'll be returned to tarantool
    lua::pushlstring(L, &resp_msg);
    return 1;
}


#[no_mangle]
pub extern "C" fn luaopen_db_handler(L: *mut lua_State) -> i32 {
    lua::register(L, "db_handle_request", db_handle_request);
    return 0;
}