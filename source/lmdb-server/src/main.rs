extern crate lmdb_rs;
extern crate rmp_bind;

use std::io::{Write, Read, stdout, stderr, Cursor};
use std::net::{TcpListener, TcpStream};
use std::thread;
use std::time;
use std::fs::File;
use lmdb_rs::{EnvBuilder, Environment, DbFlags, DbHandle};
use rmp_bind::{ decode, encode };
use std::io::BufReader;
use std::io::BufRead;

const PUT: u64 = 1;
const GET: u64 = 2;
const GET_TICKET: u64 = 3;
const AUTHORIZE: u64 = 8;
const GET_RIGHTS_ORIGIN: u64 = 9;
const GET_MEMBERSHIP: u64 = 10;
const REMOVE: u64 = 51;

mod rest;

pub fn fail(resp_msg: &mut Vec<u8>, code: rest::Codes, err_msg: String) {
    writeln!(&mut stderr(), "{0}", err_msg).unwrap();
    encode::encode_array(resp_msg, 1);
    encode::encode_uint(resp_msg, code as u64);
}

fn unmarshal_request(cursor: &mut Cursor<&[u8]>, arr_size: u64, resp_msg: &mut Vec<u8>, 
    env: &Environment, db_handle: &DbHandle) {
    if arr_size < 4 {
        ///Minimal valid array size for request is 4
        fail(resp_msg, rest::Codes::BadRequest, "@INVALID MSGPACK SIZE < 4".to_string());
        return;
    }

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
        PUT => {
            writeln!(stdout(), "Put request").unwrap();
            rest::put(cursor, arr_size, need_auth, resp_msg, env, db_handle);
        }
        GET => { 
            writeln!(stdout(), "Get request").unwrap();
            rest::get(cursor, arr_size, need_auth, resp_msg, env, db_handle);
        },
        /// Auth request don't need need_auth flag
        /// But for the sake of generality request contains it always.AUTHORIZE
        /// Three rest requests need auth function with different params.
        /// AUTHORIZE: auth without data aggreagation.
        /// GET_RIGHTS_ORIGIN: auth with rights aggregation.
        /// GET_MEMBERSHIP: auth with membership aggregation.
        // AUTHORIZE => rest::auth(cursor, arr_size, resp_msg, false, false),
        REMOVE => {
            writeln!(stdout(), "Remove request").unwrap();
            rest::remove(cursor, arr_size, need_auth, resp_msg, env, db_handle);
        }
 /*       GET_RIGHTS_ORIGIN => rest::auth(cursor, arr_size, resp_msg, true, false),
        GET_MEMBERSHIP => rest::auth(cursor, arr_size, resp_msg, false, true),
        GET_TICKET => rest::get_ticket(cursor, arr_size, resp_msg),*/
        _ => fail(resp_msg, rest::Codes::BadRequest, format!("@ERR UNKNOWN REQUEST {0}", op_code))
    }
    // writeln!(stderr(), "@END REQUEST");*/
}

fn handle_client(stream: &mut TcpStream, env: &Environment, db_handle: &DbHandle) {
    let mut buf = vec![0; 4];
    let mut read = 0;
    while read < 4 {
        match stream.read(&mut buf[read..]) {
            Ok(nbytes) => {
                read += nbytes;
            }
            Err(e) => {
                writeln!(stderr(), "@ERR ON READING REQUEST SIZE: {:?}", e).unwrap();
                return;
            }
        }
    }

    let mut request_size: u32 = 0;
    for i in 0 .. 4 {
        request_size = (request_size << 8) + buf[i] as u32;
    }

    let mut request = vec![0; request_size as usize];
    read = 0;

    while (read as u32) < request_size {
        match stream.read(&mut request[read..]) {
            Ok(nbytes) => {
                read += nbytes;
            }
            Err(e) => {
                writeln!(stderr(), "Err reading request: {:?}", e).unwrap();
                return
            }
        }
    }

    let mut cursor = Cursor::new(&request[..]);
    let mut resp_msg = Vec::with_capacity(4096);
    
    match decode::decode_array(&mut cursor) {
        Err(err) => fail(&mut resp_msg, rest::Codes::InternalServerError, err),
        Ok(arr_size) => unmarshal_request(&mut cursor, arr_size, &mut resp_msg, &env, db_handle)
    }
}

fn main() {
    // tarantool_url = 127.0.0.1:9999
    let f = File::open("veda.properties").unwrap();
    let mut file = BufReader::new(&f);
    let mut addr = "0.0.0.0:9999".to_string();
    for line in file.lines() {
        let mut l = line.unwrap();
        l = l.replace(" ", "");
        let parts: Vec<&str> = l.split("=").collect();
        if parts.len() == 2 {
            if parts[0] == "tarantool_url" {
                // addr = parts[1]
                addr = parts[1].to_string();         
            }
        }
    }        
    let listener = TcpListener::bind(addr).unwrap();

    // accept connections and process them serially
    let env_builder = EnvBuilder::new();
    env_builder.max_dbs(1);
    let env;
    loop {
        match env_builder.open("./data/lmdb-individuals/", 0o777) {
            Ok(env_res) => {
                env = env_res;
                break
            },
            Err(e) => {
                writeln!(stderr(), "Err opening environment: {:?}", e).unwrap();
                thread::sleep(time::Duration::from_secs(3));
                writeln!(stderr(), "Retry").unwrap(); 
            }
        }
    }

    writeln!(stdout(), "Opened environment ./data/lmdb-individuals").unwrap();

    let db_handle;
    loop {
        match env.get_default_db(DbFlags::empty()) {
            Ok(db_handle_res) => {
                db_handle = db_handle_res;
                break;
            },
            Err(e) => {
                writeln!(stderr(), "Err opening db handle: {:?}", e).unwrap();
                thread::sleep(time::Duration::from_secs(3));
                writeln!(stderr(), "Retry").unwrap(); 
            }
        }
    }

    writeln!(stdout(), "Opened default database").unwrap();
    loop {
        writeln!(stdout(), "Accepting").unwrap();
        match listener.accept() {
            Ok((mut stream, addr)) => {
                writeln!(stdout(), "Accepted connection {:?}", addr).unwrap();
                handle_client(&mut stream, &env, &db_handle);
            },
            Err(e) => writeln!(stderr(), "Err accepting connection: {:?}", e).unwrap(),
        }
    }
}