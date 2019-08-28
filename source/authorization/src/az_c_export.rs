use crate::authorization::*;
use lmdb_rs_m::core::{EnvCreateNoLock, EnvCreateNoMetaSync, EnvCreateNoSync, EnvCreateReadOnly};
use lmdb_rs_m::{DbFlags, EnvBuilder, Environment};
use std::cell::RefCell;
use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;
use std::ptr;
use std::sync::Mutex;
use std::thread;
use std::time;
use std::time::SystemTime;

const TRACE_ACL: u8 = 0;
const TRACE_GROUP: u8 = 1;
const TRACE_INFO: u8 = 2;

const DB_PATH: &str = "./data/acl-indexes/";
const MODULE_INFO_PATH: &str = "./data/module-info/acl_preparer_info";

lazy_static! {

#[derive(Debug)]
    static ref LAST_MODIFIED_INFO : Mutex<RefCell<SystemTime>> = Mutex::new(RefCell::new (SystemTime:: now()));

    static ref ENV : Mutex<RefCell<Environment>> = Mutex::new(RefCell::new ({
    let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

    let env1;
    loop {
        match env_builder.open(DB_PATH, 0o644) {
            Ok(env_res) => {
                env1 = env_res;
                break
            },
            Err(e) => {
                eprintln! ("ERR! Authorize: Err opening environment: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                eprintln! ("Retry");
            }
        }
    }
    eprintln! ("LIB_AZ: Opened environment ./data/acl-indexes");
    env1
    }));

}

#[no_mangle]
pub unsafe extern "C" fn get_trace(_uri: *const c_char, _user_uri: *const c_char, _request_access: u8, trace_mode: u8, _is_check_for_reload: bool) -> *const c_char {
    let c_uri: &CStr = CStr::from_ptr(_uri);
    let uri;
    match c_uri.to_str() {
        Ok(value) => uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param uri {:?}", e);
            return ptr::null();
        }
    }

    let c_user_uri: &CStr = CStr::from_ptr(_user_uri);
    let user_uri;
    match c_user_uri.to_str() {
        Ok(value) => user_uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param user_uri {:?}", e);
            return ptr::null();
        }
    }

    let _trace_acl = &mut String::new();
    let is_acl = trace_mode == TRACE_ACL;

    let _trace_group = &mut String::new();
    let is_group = trace_mode == TRACE_GROUP;

    let _trace_info = &mut String::new();
    let is_info = trace_mode == TRACE_INFO;

    let mut trace = Trace {
        acl: _trace_acl,
        is_acl,
        group: _trace_group,
        is_group,
        info: _trace_info,
        is_info,
        str_num: 0,
    };

    if _authorize(&uri, &user_uri, _request_access, _is_check_for_reload, &mut trace).is_ok() {};

    let mut trace_res = &mut String::new();

    if trace_mode == TRACE_ACL {
        trace_res = trace.acl;
    } else if trace_mode == TRACE_GROUP {
        trace_res = trace.group;
    } else if trace_mode == TRACE_INFO {
        trace_res = trace.info;
    }

    //	eprintln! ("trace_res={}", trace_res);

    //	let bytes = trace_res.into_bytes();
    let cres = CString::new(trace_res.clone()).unwrap();

    // http://jakegoulding.com/rust-ffi-omnibus/string_return/
    //cres.into_raw()

    let p = cres.as_ptr();

    std::mem::forget(cres);

    p
}

#[no_mangle]
pub unsafe extern "C" fn authorize_r(_uri: *const c_char, _user_uri: *const c_char, request_access: u8, is_check_for_reload: bool) -> u8 {
    let c_uri: &CStr = CStr::from_ptr(_uri);
    let uri;
    match c_uri.to_str() {
        Ok(value) => uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param uri {:?}", e);
            return 0;
        }
    }

    let c_user_uri: &CStr = CStr::from_ptr(_user_uri);
    let user_uri;
    match c_user_uri.to_str() {
        Ok(value) => user_uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param user_uri {:?}", e);
            return 0;
        }
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

    for attempt in 1..10 {
        if attempt > 1 {
            eprintln!("ERR! AZ: attempt {:?}", attempt)
        }

        match _authorize(uri, user_uri, request_access, is_check_for_reload, &mut trace) {
            Ok(res) => return res,
            Err(_e) => {}
        }
    }
    0
}

fn check_for_reload() -> std::io::Result<bool> {
    use std::fs::File;
    let f = File::open(MODULE_INFO_PATH)?;

    let metadata = f.metadata()?;

    if let Ok(new_time) = metadata.modified() {
        let prev_time = *LAST_MODIFIED_INFO.lock().unwrap().get_mut();

        if new_time != prev_time {
            LAST_MODIFIED_INFO.lock().unwrap().replace(new_time);
            //eprintln!("LAST_MODIFIED_INFO={:?}", new_time);
            return Ok(true);
        }
    }

    Ok(false)
}

pub fn _authorize(uri: &str, user_uri: &str, request_access: u8, _is_check_for_reload: bool, trace: &mut Trace) -> Result<u8, i64> {
    if _is_check_for_reload {
        if let Ok(true) = check_for_reload() {
            //eprintln!("INFO: Authorize: reopen db");

            let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

            match env_builder.open(DB_PATH, 0o644) {
                Ok(env_res) => {
                    ENV.lock().unwrap().replace(env_res);
                }
                Err(e) => {
                    eprintln!("ERR! Authorize: Err opening environment: {:?}", e);
                }
            }
        }
    }

    let env = ENV.lock().unwrap().get_mut().clone();

    let db_handle;
    loop {
        match env.get_default_db(DbFlags::empty()) {
            Ok(db_handle_res) => {
                db_handle = db_handle_res;
                break;
            }
            Err(e) => {
                eprintln!("ERR! Authorize: Err opening db handle: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                eprintln!("Retry");
            }
        }
    }

    let txn;
    match env.get_reader() {
        Ok(txn1) => {
            txn = txn1;
        }
        Err(e) => {
            eprintln!("ERR! Authorize:CREATING TRANSACTION {:?}", e);
            eprintln!("reopen db");

            let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

            match env_builder.open(DB_PATH, 0o644) {
                Ok(env_res) => {
                    ENV.lock().unwrap().replace(env_res);
                }
                Err(e) => {
                    eprintln!("ERR! Authorize: Err opening environment: {:?}", e);
                }
            }

            return _authorize(uri, user_uri, request_access, _is_check_for_reload, trace);
        }
    }

    let db = txn.bind(&db_handle);

    // 0. читаем фильтр прав у object (uri)
    let mut filter_value;
    let mut filter_allow_access_to_other = 0;
    match get_from_db(&(FILTER_PREFIX.to_owned() + uri), &db) {
        Ok(data) => {
            filter_value = data;
            if filter_value.len() < 3 {
                filter_value.clear();
            } else {
                let filters_set: &mut Vec<Right> = &mut Vec::new();
                get_elements_from_index(&filter_value, filters_set);

                if !filters_set.is_empty() {
                    let el = &mut filters_set[0];

                    filter_value = el.id.clone();
                    filter_allow_access_to_other = el.access;
                }
            }
            //eprintln!("Authorize:uri=[{}], filter_value=[{}]", uri, filter_value);
        }
        Err(e) => {
            if e == 0 {
                filter_value = String::new();
            } else {
                eprintln!("ERR! Authorize: _authorize {:?}", uri);
                return Err(e);
            }
        }
    }

    authorize(uri, user_uri, request_access, &filter_value, filter_allow_access_to_other, &db, trace)
}
