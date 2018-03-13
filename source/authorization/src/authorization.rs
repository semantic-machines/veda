extern crate core;
/// This module gives function to check access of user to individual
#[macro_use]
extern crate lazy_static;
extern crate lmdb_rs_m;

//use std::ffi::CString;
//use std::ptr;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::collections::HashMap;
//use std::borrow::BorrowMut;
//use std::convert::AsMut;
//use std::io::{ Cursor, Write, stderr };
//use std::ptr::null_mut;
use std::thread;
use std::time;

//use std::io::{stderr, stdout, Write};
use lmdb_rs_m::{DbFlags, DbHandle, EnvBuilder, Environment, MdbError};
use lmdb_rs_m::core::{Database, EnvCreateNoLock, EnvCreateNoMetaSync, EnvCreateNoSync, EnvCreateReadOnly};

const PERMISSION_PREFIX: &str = "P";
const FILTER_PREFIX: &str = "F";
const MEMBERSHIP_PREFIX: &str = "M";

//enum ACCESS {
//    can_create = 1,
//    can_read = 2,
//    can_update = 4,
//    can_delete = 8,
//}

static ACCESS_LIST: [u8; 4] = [1, 2, 4, 8];

#[derive(Debug)]
pub struct Right {
    id: String,
    access: u8,
    is_deleted: bool,
}

pub struct AzContext<'a> {
    //    uri: &'a str,
    request_access: u8,
    calc_right_res: u8,
    filter_value: &'a str,
    walked_groups_s: &'a mut HashMap<String, u8>,
    walked_groups_o: &'a mut HashMap<String, u8>,
    subject_groups: &'a mut HashMap<String, Right>,
    checked_groups: &'a mut HashMap<String, u8>,
}

lazy_static! {
    pub static ref ENV: Environment = {
    let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

    let env1;
    loop {
        match env_builder.open("./data/acl-indexes/", 0o644) {
            Ok(env_res) => {
                env1 = env_res;
                break
            },
            Err(e) => {
                println! ("ERR! Authorize: Err opening environment: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                println! ("Retry");
            }
        }
    }
    println! ("Opened environment ./data/acl-indexes");
    env1
    };


    pub static ref DB_HANDLE: DbHandle = {
    let db_handle;
    loop {
        match ENV.get_default_db(DbFlags::empty()) {
            Ok(db_handle_res) => {
                db_handle = db_handle_res;
                break;
            },
            Err(e) => {
                println! ("ERR! Authorize: Err opening db handle: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                println! ("Retry");
            }
        }
    }
    println! ("Opened default database");
    db_handle
    };

}

pub fn get_from_db(key: &str, db: &Database) -> String {
    match db.get::<String>(&key) {
        Ok(val) => {
            //writeln!(stdout(), "Authorize:Get OK, val={:?}", val).unwrap();
            return val;
        }
        Err(e) => match e {
            MdbError::NotFound => {
                //println! ("Authorize:Get, [{}] NOT FOUND", key);
                return String::new();
            }

            _ => {
                println!("ERR! Authorize ON GET TRANSACTION {:?}, {}", e, key);
                return String::new();
            }
        },
    }
}

pub fn rights_map_from_string(src: String, results: &mut HashMap<String, Right>) -> bool {
    if src.is_empty() {
        return false;
    }

    let tokens: Vec<&str> = src.split(';').collect();

    let mut idx = 0;
    loop {
        if idx + 1 < tokens.len() {
            let key = tokens[idx];
            let access;

            match tokens[idx + 1].chars().next() {
                Some(c) => match c.to_digit(16) {
                    Some(v) => access = v,
                    None => {
                        println!(
                            "ERR! rights_from_string, fail parse, access is not hex digit {}",
                            src
                        );
                        continue;
                    }
                },
                None => {
                    println!(
                        "ERR! rights_from_string, fail parse, not found access char {}",
                        src
                    );
                    continue;
                }
            }

            let rr = Right {
                id: key.to_string(),
                access: access as u8,
                is_deleted: false,
            };
            results.insert(rr.id.clone(), rr);

        //println!("access= {:?}", access);
        } else {
            break;
        }

        idx += 2;
        if idx >= tokens.len() {
            break;
        }
    }

    return true;
}

pub fn rights_vec_from_string(src: String, results: &mut Vec<Right>) -> bool {
    if src.is_empty() {
        return false;
    }

    let tokens: Vec<&str> = src.split(';').collect();

    let mut idx = 0;
    loop {
        if idx + 1 < tokens.len() {
            let key = tokens[idx];
            let access;

            match tokens[idx + 1].chars().next() {
                Some(c) => match c.to_digit(16) {
                    Some(v) => access = v,
                    None => {
                        println!(
                            "ERR! rights_from_string, fail parse, access is not hex digit {}",
                            src
                        );
                        continue;
                    }
                },
                None => {
                    println!(
                        "ERR! rights_from_string, fail parse, not found access char {}",
                        src
                    );
                    continue;
                }
            }

            let rr = Right {
                id: key.to_string(),
                access: access as u8,
                is_deleted: false,
            };
            results.push(rr);

        //println!("access= {:?}", access);
        } else {
            break;
        }

        idx += 2;
        if idx >= tokens.len() {
            break;
        }
    }

    return true;
}

pub fn authorize_obj_group(azc: &mut AzContext, object_group_id: &str, object_group_access: u8, db: &Database) -> bool {
        	//println!("authorize_r:authorize_obj_group object_group_id={}, object_group_access={}", object_group_id, object_group_access);
    let mut is_authorized = false;
    let mut calc_bits;

    let left_to_check = (azc.calc_right_res ^ azc.request_access) & azc.request_access;

    if left_to_check & object_group_access == 0 {
        return is_authorized;
    }

    match azc.checked_groups.get(object_group_id) {
        Some(v) => {
            if *v == object_group_access {
                return is_authorized;
            }
        }
        None => (),
    }

    azc.checked_groups
        .insert(object_group_id.to_string(), object_group_access);

    let acl_key;
    if azc.filter_value.is_empty() == false {
        acl_key = PERMISSION_PREFIX.to_owned() + azc.filter_value + &object_group_id;
    } else {
        acl_key = PERMISSION_PREFIX.to_owned() + &object_group_id;
    }

    let str = get_from_db(&acl_key, &db);
    if !str.is_empty() {
        let permissions: &mut Vec<Right> = &mut Vec::new();
        	//println!("authorize_r:authorize_obj_group str={}", str);
        rights_vec_from_string(str, permissions);

        for permission in permissions {
        	//println!("authorize_r:authorize_obj_group permission={:?}", permission);
            let subj_id = &permission.id;
            if azc.subject_groups.contains_key(subj_id) {
                let restriction_access = object_group_access;
                let mut permission_access;

                    //if permission !is null
                    {
                        if permission.access > 15 {
        	//println!("authorize_r:authorize_obj_group permission.access={}", permission.access);
                            permission_access = (((permission.access & 0xF0) >> 4) ^ 0x0F) & permission.access;
        	//println!("authorize_r:authorize_obj_group #1 permission_access={}", permission_access);
                        }
                        else
                        {
                            permission_access = permission.access;
				        	//println!("authorize_r:authorize_obj_group #2 permission_access={}", permission_access);
                        }
                    }

                for i_access in ACCESS_LIST.iter() {
        	//println!("authorize_r:authorize_obj_group i_access={}", i_access);
        	//println!("authorize_r:authorize_obj_group azc.request_access({}), access({}, permission_access({}))", azc.request_access, i_access, permission_access);
                    let access = i_access;
                    if (azc.request_access & access & restriction_access) != 0 {
                        calc_bits = access & permission_access;
        	//println!("authorize_r:authorize_obj_group calc_bits({}) = access({}) & permission_access({}))", calc_bits, access, permission_access);

                        if calc_bits > 0 {
                        	
                            azc.calc_right_res = azc.calc_right_res | calc_bits;
        	//println!("authorize_r:authorize_obj_group calc_right_res={}", azc.calc_right_res);

                            if (azc.calc_right_res & azc.request_access) == azc.request_access {
        	//println!("authorize_r:authorize_obj_group is_authorized = true={}", is_authorized = true);
                                is_authorized = true;
                                return is_authorized;
                            }
                        }
                    }
                }
            }
        }
    }

        	//println!("authorize_r:authorize_obj_group calc_right_res={}, request_access={}", azc.calc_right_res, azc.request_access);
    if (azc.calc_right_res & azc.request_access) == azc.request_access {
        is_authorized = true;
        return is_authorized;
    }

    return false;
}

pub fn prepare_obj_group(azc: &mut AzContext, uri: &str, access: u8, level: u8, db: &Database) -> bool {
    if level > 32 {
        return false;
    }

    let groups_str = get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db);
    //println!("authorize_r:prepare_obj_group uri=[{}] groups_str=[{}]", uri, groups_str);

    if groups_str.is_empty() {
        return false;
    }

    let groups_set: &mut Vec<Right> = &mut Vec::new();
    rights_vec_from_string(groups_str, groups_set);

    for idx in 0..groups_set.len() {
        let mut group = &mut groups_set[idx];

        //println!("authorize_r:prepare_obj_group:group ={:?}", group);

        if group.id.is_empty() {
            println!("WARN! WARN! group is null, uri={}, idx={}", uri, idx);
            continue;
        }

        //let orig_access = group.access;
        let new_access = group.access & access;
        group.access = new_access;

        if azc.walked_groups_o.contains_key(&group.id) {
            let preur_access = azc.walked_groups_o[&group.id];
            if preur_access == new_access {
            	 //println!("E1 group={:?} preur_access={}", group, preur_access);
                continue;
            }
        }
        azc.walked_groups_o.insert(group.id.clone(), new_access);

        if uri == group.id {
            	 //println!("E2");
            continue;
        }

        if authorize_obj_group(azc, &group.id, group.access, &db) == true {
            	 //println!("E3");
            return true;
        }

        prepare_obj_group(azc, &group.id, new_access, level + 1, &db);
    }
    return false;
}

pub fn get_resource_groups(azc: &mut AzContext, uri: &str, access: u8, results: &mut HashMap<String, Right>, level: u8, db: &Database) -> bool {
    	//println! ("get_resource_groups uri={}, access={}, level={}", uri, access, level);

    if level > 32 {
        return false;
    }

    let groups_str = get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db);

    if groups_str.is_empty() {
        return false;
    }

    //println!("authorize_r: --- get_resource_groups uri=[{}] groups_str=[{}]", uri, groups_str);

    let groups_set: &mut Vec<Right> = &mut Vec::new();
    rights_vec_from_string(groups_str, groups_set);

    for idx in 0..groups_set.len() {

let mut group = &mut groups_set[idx];

//        let mut group;

//        match groups_set.pop() {
//            Some(v) => group = v,
//            None => break,
//        }

        //println!("authorize_r:get_resource_groups: idx={}, group ={:?}, level={}", idx, group, level);

        if group.id.is_empty() {
            println!("WARN! WARN! group is null, uri={}, idx={}", uri, idx);
            continue;
        }

        //let orig_access = group.access;
        let new_access = group.access & access;
        group.access = new_access;

        //        println!("azc.walked_groups {:?}", azc.walked_groups);
        if azc.walked_groups_s.contains_key(&group.id) {
            //println!("#0 group already prepare {}", &group.id);
            let preur_access = azc.walked_groups_s[&group.id];
            if preur_access == new_access {
                //println!("#1 group already prepare {}", &group.id);
                continue;
            }
        }
        azc.walked_groups_s.insert(group.id.clone(), new_access);

        if uri == group.id {
            continue;
        }

        get_resource_groups(azc, &group.id, 15, results, level + 1, &db);
        results.insert(group.id.clone(),  Right {
                id: group.id.clone(),
                access: group.access,
                is_deleted: group.is_deleted
            });
    }
    //println!("authorize_r: EEE");

    return false;
}

#[no_mangle]
pub extern "C" fn authorize_r(_uri: *const c_char, _user_uri: *const c_char, _request_access: u8, _is_check_for_reload: bool) -> u8 {

    let c_uri: &CStr = unsafe { CStr::from_ptr(_uri) };
    let uri;
    match c_uri.to_str() {
        Ok(value) => uri = value,
        Err(e) => {
            println!("ERR! invalid paramuri {:?}", e);
            return 0;
        }
    }

    let c_user_uri: &CStr = unsafe { CStr::from_ptr(_user_uri) };
    let user_uri;
    match c_user_uri.to_str() {
        Ok(value) => user_uri = value,
        Err(e) => {
            println!("ERR! invalid param user_uri {:?}", e);
            return 0;
        }
    }

    //println!("authorize_r  #1 uri {:?}", uri);
    //println!("authorize_r  #3 user_uri: {:?}", user_uri);

    match ENV.get_reader() {
        Ok(txn) => {
            let db = txn.bind(&DB_HANDLE);

            // 0. читаем фильтр прав у object (uri)
            let filter_value = get_from_db(&(FILTER_PREFIX.to_owned() + uri), &db);
            //println!("Authorize:filter_value=[{}]", filter_value);

            // читаем группы subject (ticket.user_uri)

            let mut walked_groups_s = &mut HashMap::new();
            let mut walked_groups_o = &mut HashMap::new();
            let mut subject_groups = &mut HashMap::new();
            let mut groups = &mut HashMap::new();
            let mut checked_groups = &mut HashMap::new();

            let mut azc = AzContext {
                request_access: _request_access,
                filter_value: &filter_value,
                calc_right_res: 0,
                walked_groups_s: walked_groups_s,
                walked_groups_o: walked_groups_o,
                subject_groups: subject_groups,
                checked_groups: checked_groups,
            };

            get_resource_groups(&mut azc, user_uri, 15, groups, 0, &db);

            azc.subject_groups = groups;

            azc.subject_groups.insert(
                user_uri.to_string(),
                Right {
                    id: user_uri.to_string(),
                    access: 15,
                    is_deleted: false,
                },
            );

            if authorize_obj_group(&mut azc, "v-s:AllResourcesGroup", 15, &db) == true {
                return azc.calc_right_res;
            }

            if authorize_obj_group(&mut azc, uri, 15, &db) == true {
                return azc.calc_right_res;
            }

            if prepare_obj_group(&mut azc, uri, 15, 0, &db) == true {
                return azc.calc_right_res;
            }

            //println!("authorize_r: azc.calc_right_res= {:?}", azc.calc_right_res);
            return azc.calc_right_res;
        }
        Err(e) => {
            println!("ERR! Authorize:ON CREATING GET TRANSACTION {:?}", e);
        }
    }
    return 0;
}
