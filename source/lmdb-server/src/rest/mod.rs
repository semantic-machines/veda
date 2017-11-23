extern crate time;

use lmdb_rs::{DbHandle, Environment, MdbError};
use std;
use std::io::{ Write, stderr, stdout, Cursor };
use rmp_bind::{ encode, decode };
use std::fs::File;
use std::fs::DirBuilder;

mod put_routine;

const MAX_VECTOR_SIZE: usize = 150;

/// REST return codes
pub enum Codes {
    Ok = 200,
    BadRequest = 400,
    TicketExpired = 471,
    NotAuthorized = 472,
    NotFound = 404,
    InternalServerError = 500,
    UnprocessableEntity = 422
}
pub fn get(cursor: &mut Cursor<&[u8]>, arr_size: u64, need_auth:bool, resp_msg: &mut Vec<u8>, 
    env: &Environment ,db_handle: &DbHandle) {
    let mut user_id_buf = Vec::default();
    let user_id: &str;
    match decode::decode_string(cursor, &mut user_id_buf) {
        Err(err) => return super::fail(resp_msg, Codes::InternalServerError, err),
        Ok(_) => user_id = std::str::from_utf8(&user_id_buf).unwrap()
    }

    writeln!(stdout(), "Get request from user [{}]", user_id).unwrap();
    encode::encode_array(resp_msg, ((arr_size - 3) * 2 + 1) as u32);
    encode::encode_uint(resp_msg, Codes::Ok as u64);
    
    for _ in 3 .. arr_size {        
        let mut res_uri_buf = Vec::default();    
        let res_uri: &str;

        match decode::decode_string(cursor, &mut res_uri_buf) {
            Err(err) => { super::fail(resp_msg, Codes::InternalServerError, err); continue; },
            Ok(_) => res_uri = std::str::from_utf8(&res_uri_buf).unwrap()
        }

        writeln!(stdout(), "Try to get individual [{}]", res_uri).unwrap();
        match env.new_transaction() {
            Ok(txn) => {
                let db = txn.bind(db_handle);
                match db.get::<Vec<u8>>(&res_uri.to_string()) {
                    Ok(val) => {
                        encode::encode_uint(resp_msg, Codes::Ok as u64);
                        encode::encode_string_bytes(resp_msg, &val);
                        writeln!(stdout(), "Get OK").unwrap();
                    },
                    Err(e) => {
                        match e {
                            MdbError::NotFound => {
                                encode::encode_uint(resp_msg, Codes::UnprocessableEntity as u64);
                                encode::encode_nil(resp_msg);
                                writeln!(stdout(), "Get NOT FOUND").unwrap();
                            },

                            _ => {
                                writeln!(stderr(), "@ERR ON GET TRANSACTION {:?}, {}", e, res_uri).unwrap();
                                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
                                encode::encode_nil(resp_msg);
                            }
                        }
                    }
                }
            },
            Err(e) => {
                writeln!(stderr(), "@ERR ON CREATING GET TRANSACTION {:?}", e).unwrap();
                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
                encode::encode_nil(resp_msg);
            }

        }      
    }
}

pub fn remove(cursor: &mut Cursor<&[u8]>, arr_size: u64, need_auth:bool, resp_msg: &mut Vec<u8>,
    env: &Environment ,db_handle: &DbHandle) {
    let mut user_id_buf = Vec::default();
    let user_id: &str;
    match decode::decode_string(cursor, &mut user_id_buf) {
        Err(err) => return super::fail(resp_msg, Codes::InternalServerError, err),
        Ok(_) => {user_id = std::str::from_utf8(&user_id_buf).unwrap()}
    }
    writeln!(stdout(), "Remove request from user [{}]", user_id).unwrap();


    encode::encode_array(resp_msg, (arr_size - 3 + 1) as u32);
    encode::encode_uint(resp_msg, Codes::Ok as u64);
    for _ in 3 .. arr_size {
        let mut res_uri_buf = Vec::default();    
        let res_uri: &str;

        /// Decodes resource's uri
        match decode::decode_string(cursor, &mut res_uri_buf) {
            Err(err) => { super::fail(resp_msg, Codes::InternalServerError, err); continue; },
            Ok(_) => res_uri = std::str::from_utf8(&res_uri_buf).unwrap()
        }

        writeln!(stdout(), "Try to remove individual [{}]", res_uri).unwrap();
        match env.new_transaction() {
            Ok(txn) => {
                let db = txn.bind(db_handle);
                match db.del(&res_uri.to_string()) {
                    Ok(_) => {
                        encode::encode_uint(resp_msg, Codes::Ok as u64);
                        writeln!(stdout(), "Remove OK").unwrap();
                    },
                    Err(e) => {
                        match e {
                            MdbError::NotFound => {
                                encode::encode_uint(resp_msg, Codes::UnprocessableEntity as u64);
                                writeln!(stdout(), "Remove NOT FOUND").unwrap();
                            },

                            _ => {
                                writeln!(stderr(), "@ERR ON REMOVE TRANSACTION {:?}, {}", e, res_uri).unwrap();
                                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
                            }
                        }
                    }
                }
            },
            Err(e) => {
                writeln!(stderr(), "@ERR ON CREATING REMOVE TRANSACTION {:?}", e).unwrap();
                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
            }
        }

        encode::encode_uint(resp_msg, Codes::Ok as u64);        
    }
}

pub fn dump_failure(buf: &[u8]) {
    let path = "data/fail/";
    DirBuilder::new().recursive(true).create(path).unwrap();
    
     let current_time = time::get_time();
     let milliseconds = (current_time.sec as i64 * 1000) + 
                       (current_time.nsec as i64 / 1000 / 1000); 
    let file_path = format!("data/fail/{}", milliseconds);
    writeln!(stderr(), "\tdumped to file {0}", file_path);
    let mut f = File::create(file_path).unwrap();
    f.write_all(buf);
}

pub fn put(cursor: &mut Cursor<&[u8]>, arr_size: u64, need_auth:bool, resp_msg: &mut Vec<u8>, 
    env: &Environment ,db_handle: &DbHandle) {
    let mut user_id_buf = Vec::default();
    let user_id: &str;
    ///First put decodes user_id
    match decode::decode_string(cursor, &mut user_id_buf) {
        Err(err) => return super::fail(resp_msg, Codes::InternalServerError, err),
        Ok(_) => {user_id = std::str::from_utf8(&user_id_buf).unwrap()}
    }

    writeln!(stdout(), "Put request from user [{}]", user_id).unwrap();
    
    encode::encode_array(resp_msg, (arr_size - 3 + 1) as u32);
    ///For all responses firs is common operation result
    encode::encode_uint(resp_msg, Codes::Ok as u64);

    ///Cycle handles separate requests from user and encodes results in response
    for _ in 3 .. arr_size {
        let mut individual_msgpack_buf = Vec::default();    
        
        match decode::decode_string(cursor, &mut individual_msgpack_buf) {
            Err(err) => {
                writeln!(stderr(), "@ERR DECODING WRAPPER INDIVIDUAL MSGPACK {:?}", err).unwrap();
                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
                return;
            }
            Ok(_) => {}
        }

        if individual_msgpack_buf[0] != 0xFF {
            encode::encode_uint(resp_msg, Codes::UnprocessableEntity as u64);
            writeln!(stderr(), "@ERR WRAPPER IS NOT MSGPACK, NO SIGNATURE FOUND").unwrap();
            unsafe {
                writeln!(stderr(), "@INDIVIDUAL WRAPPER INVALID BUF [{0}]",
                    std::str::from_utf8_unchecked(&individual_msgpack_buf[..])).unwrap();
                dump_failure(&individual_msgpack_buf[..]);
            }
            return
        }
        
        let mut individual = put_routine::Individual::new();
        ///Decoding msgpack to individual structure
        match put_routine::msgpack_to_individual(&mut Cursor::new(&individual_msgpack_buf[1..]), &mut individual) {
            Ok(_) => {}
            Err(err) => {
                writeln!(stderr(), "@ERR DECODING WRAPPER INDIVIDUAL {:?}", err).unwrap();
                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
                unsafe {
                    writeln!(stderr(), "@INDIVIDUAL WRAPPER INVALID BUF [{0}]",
                        std::str::from_utf8_unchecked(&individual_msgpack_buf[..])).unwrap();
                    dump_failure(&individual_msgpack_buf[..]);
                }
                return;
            }
        }
        writeln!(stdout(), "Decoded wrapper individual").unwrap();

        let new_state_res: &Vec<put_routine::Resource>;
        ///Decoding the state of individual to store in base
        match individual.resources.get(&"new_state".to_string()) {
            Some(res) => new_state_res = res,
            _ => {
                ///New state must be in all individs, 
                ///if new state wasn't found BadRequest is returned to client
                writeln!(stderr(), "@NO NEW_STATE FOUND").unwrap();
                encode::encode_uint(resp_msg, Codes::BadRequest as u64);
                return;
            }
        }

        if new_state_res[0].str_data[0] != 0xFF {
            encode::encode_uint(resp_msg, Codes::UnprocessableEntity as u64);
            writeln!(stderr(), "@ERR NEW_STATE IS NOT MSGPACK, NO SIGNATURE FOUND").unwrap();
            unsafe {
                writeln!(stderr(), "@NEW STATE INVALID BUF [{0}]",
                    std::str::from_utf8_unchecked(&new_state_res[0].str_data[..])).unwrap();
                dump_failure(&new_state_res[0].str_data[..]);
            }
            return
        }

        let mut new_state = put_routine::Individual::new();
        ///Decoding individuals fron new state
        match put_routine::msgpack_to_individual(&mut Cursor::new(&new_state_res[0].str_data[1..]), 
            &mut new_state) {
            Ok(_) => {}
            Err(err) => {
                ///If new state individual can not be decoded InternalServerError code is returned to cleint
                writeln!(stderr(), "@ERR DECODING NEW STATE {:?}", err).unwrap();
                unsafe {
                    writeln!(stderr(), "@NEW STATE INVALID BUF [{0}]",
                        std::str::from_utf8_unchecked(&new_state_res[0].str_data[..])).unwrap();
                    dump_failure(&new_state_res[0].str_data[..]);
                }
                encode::encode_uint(resp_msg, Codes::UnprocessableEntity as u64);
                return;
            }
        }
        writeln!(stdout(), "Decoded new state [{}]", std::str::from_utf8(&new_state.uri[..]).unwrap()).unwrap();
                
        match env.new_transaction() {
            Ok(txn) => {
                let db = txn.bind(db_handle);
                unsafe {
                    let new_sate_data = std::str::from_utf8_unchecked(&new_state_res[0].str_data[..]).to_string();
                    match db.set(&std::str::from_utf8(&new_state.uri[..]).unwrap().to_string(), &new_sate_data) {
                        Ok(_) => {
                            encode::encode_uint(resp_msg, Codes::Ok as u64);
                            writeln!(stdout(), "Put OK").unwrap();
                        },
                        Err(e) => {
                            match e {
                                _ => {
                                    writeln!(stderr(), "@ERR ON PUT TRANSACTION {:?}", e).unwrap();
                                    encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
                                }
                            }
                        }
                    }
                }
            },
            Err(e) => {
                writeln!(stderr(), "@ERR ON CREATING PUT TRANSACTION {:?}", e).unwrap();
                encode::encode_uint(resp_msg, Codes::InternalServerError as u64);
            }

        }   
    }
}