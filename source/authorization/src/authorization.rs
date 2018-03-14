extern crate core;
/// This module gives function to check access of user to individual

#[macro_use]
extern crate lazy_static;
extern crate lmdb_rs_m;

use core::fmt;
use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;
use std::collections::HashMap;
use std::thread;
use std::time;

use lmdb_rs_m::{DbFlags, DbHandle, EnvBuilder, Environment, MdbError};
use lmdb_rs_m::core::{Database, EnvCreateNoLock, EnvCreateNoMetaSync, EnvCreateNoSync, EnvCreateReadOnly};

const PERMISSION_PREFIX: &str = "P";
const FILTER_PREFIX: &str = "F";
const MEMBERSHIP_PREFIX: &str = "M";

static ACCESS_LIST: [u8; 4] = [1, 2, 4, 8];

pub struct Right {
    id: String,
    access: u8,
    is_deleted: bool,
}

impl fmt::Debug for Right {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.id, access_to_pretty_string(self.access))
    }
}

pub struct AzContext<'a> {
    request_access: u8,
    calc_right_res: u8,
    filter_value: &'a str,
    walked_groups_s: &'a mut HashMap<String, u8>,
    walked_groups_o: &'a mut HashMap<String, u8>,
    subject_groups: &'a mut HashMap<String, Right>,
    checked_groups: &'a mut HashMap<String, u8>,
    trace_acl: Option<extern "C" fn(*const c_char)>,
    is_trace_acl: bool,
    trace_group: Option<extern "C" fn(*const c_char)>,
    is_trace_group: bool,
    trace_info: Option<extern "C" fn(*const c_char)>,
    is_trace_info: bool,
    str_num: u32,
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
            return val;
        }
        Err(e) => match e {
            MdbError::NotFound => {
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
    let mut is_authorized = false;
    let mut calc_bits;

    if azc.is_trace_info == false && azc.is_trace_group == false && azc.is_trace_acl == false {
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
    }

    if azc.is_trace_group {
        print_to_trace_group(azc, format!("{}\n", object_group_id));
    }

    let acl_key;
    if azc.filter_value.is_empty() == false {
        acl_key = PERMISSION_PREFIX.to_owned() + azc.filter_value + &object_group_id;
    } else {
        acl_key = PERMISSION_PREFIX.to_owned() + &object_group_id;
    }

    if azc.is_trace_info {
        print_to_trace_info(azc, format!("look acl_key: [{}]\n", acl_key));
    }

    let str = get_from_db(&acl_key, &db);
    if !str.is_empty() {
        let permissions: &mut Vec<Right> = &mut Vec::new();

        rights_vec_from_string(str, permissions);

        for permission in permissions {
            if azc.is_trace_info {
                let req_acs = azc.request_access;
                print_to_trace_info(
                    azc,
                    format!(
                        "restriction={} {}, permission={:?}, request={}\n",
                        object_group_id,
                        access_to_pretty_string(object_group_access),
                        permission,
                        access_to_pretty_string(req_acs)
                    ),
                );
            }

            let subj_id = &permission.id;
            if azc.subject_groups.contains_key(subj_id) {
                let restriction_access = object_group_access;
                let mut permission_access;

                if permission.access > 15 {
                    permission_access = (((permission.access & 0xF0) >> 4) ^ 0x0F) & permission.access;
                } else {
                    permission_access = permission.access;
                }

                for i_access in ACCESS_LIST.iter() {
                    let access = i_access;
                    if (azc.request_access & access & restriction_access) != 0 {
                        calc_bits = access & permission_access;

                        if calc_bits > 0 {
                            azc.calc_right_res = azc.calc_right_res | calc_bits;

                            if (azc.calc_right_res & azc.request_access) == azc.request_access {
                                if azc.is_trace_info {
                                    let req_acs = azc.request_access;
                                    let crr_acs = azc.calc_right_res;
                                    print_to_trace_info(
                                        azc,
                                        format!(
                                            "EXIT? request_access={}, res={}\n",
                                            access_to_pretty_string(req_acs),
                                            access_to_pretty_string(crr_acs)
                                        ),
                                    );
                                } else if azc.is_trace_group == false {
                                    is_authorized = true;
                                    return is_authorized;
                                }
                            }

                            if azc.is_trace_info {
                                let crr_acs = azc.calc_right_res;
                                print_to_trace_info(
                                    azc,
                                    format!(
                                        "calc_bits={}, res={}\n",
                                        access_to_pretty_string(calc_bits),
                                        access_to_pretty_string(crr_acs)
                                    ),
                                );
                            }

                            if azc.is_trace_acl {
                                print_to_trace_acl(
                                    azc,
                                    format!("{};{};{}\n", object_group_id, subj_id, i_access),
                                );
                            }
                        }
                    }
                }
            }
        }

        if azc.is_trace_info {
            let str = get_from_db(&acl_key, &db);
            let permissions: &mut Vec<Right> = &mut Vec::new();
            rights_vec_from_string(str, permissions);
            print_to_trace_info(azc, format!("for [{}] found {:?}\n", acl_key, permissions));
        }
    }

    if (azc.calc_right_res & azc.request_access) == azc.request_access {
        if azc.is_trace_info {
            let req_acs = azc.request_access;
            let crr_acs = azc.calc_right_res;
            print_to_trace_info(
                azc,
                format!(
                    "EXIT? request_access={}, res={}\n",
                    access_to_pretty_string(req_acs),
                    access_to_pretty_string(crr_acs)
                ),
            );
        }

        if azc.is_trace_info == false && azc.is_trace_group == false && azc.is_trace_acl == false {
            is_authorized = true;
            return is_authorized;
        }
    }

    return false;
}

pub fn prepare_obj_group(azc: &mut AzContext, uri: &str, access: u8, level: u8, db: &Database) -> bool {
    if level > 32 {
        if azc.is_trace_info {
            print_to_trace_info(azc, format!("ERR! level down > 32, uri={}\n", uri));
        }

        return false;
    }

    let groups_str = get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db);

    if groups_str.is_empty() {
        return false;
    }

    let groups_set: &mut Vec<Right> = &mut Vec::new();
    rights_vec_from_string(groups_str, groups_set);

    for idx in 0..groups_set.len() {
        let mut group = &mut groups_set[idx];

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
                continue;
            }
        }
        azc.walked_groups_o.insert(group.id.clone(), new_access);

        if uri == group.id {
            continue;
        }

        if authorize_obj_group(azc, &group.id, group.access, &db) == true {
            return true;
        }

        prepare_obj_group(azc, &group.id, new_access, level + 1, &db);
    }
    return false;
}

pub fn get_resource_groups(azc: &mut AzContext, uri: &str, access: u8, results: &mut HashMap<String, Right>, level: u8, db: &Database) -> bool {
    if level > 32 {
        if azc.is_trace_info {
            print_to_trace_info(azc, format!("ERR! level down > 32, uri={}\n", uri));
        }
        return false;
    }

    let groups_str = get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db);

    if groups_str.is_empty() {
        return false;
    }

    let groups_set: &mut Vec<Right> = &mut Vec::new();
    rights_vec_from_string(groups_str, groups_set);

    for idx in 0..groups_set.len() {
        let mut group = &mut groups_set[idx];

        if group.id.is_empty() {
            println!("WARN! WARN! group is null, uri={}, idx={}", uri, idx);
            continue;
        }

        let new_access = group.access & access;
        let orig_access = group.access;
        group.access = new_access;

        if azc.walked_groups_s.contains_key(&group.id) {
            let preur_access = azc.walked_groups_s[&group.id];
            if preur_access == new_access {
                if azc.is_trace_info {
                    print_to_trace_info(
                        azc,
                        format!(
                            "{:1$} ({})GROUP [{}].access={} SKIP, ALREADY ADDED\n",
                            level * 2,
                            level as usize,
                            group.id,
                            access_to_pretty_string(preur_access)
                        ),
                    );
                }

                continue;
            }
        }
        azc.walked_groups_s.insert(group.id.clone(), new_access);

        if azc.is_trace_info {
            print_to_trace_info(
                azc,
                format!(
                    "{:1$} ({})GROUP [{}] {}-> {}\n",
                    level * 2,
                    level as usize,
                    group.id,
                    access_to_pretty_string(orig_access),
                    access_to_pretty_string(new_access)
                ),
            );
        }

        if uri == group.id {
            if azc.is_trace_info {
                print_to_trace_info(
                    azc,
                    format!(
                        "{:1$} ({})GROUP [{}].access={} SKIP, uri == group_key\n",
                        level * 2,
                        level as usize,
                        group.id,
                        access_to_pretty_string(orig_access)
                    ),
                );
            }
            continue;
        }

        get_resource_groups(azc, &group.id, 15, results, level + 1, &db);
        results.insert(
            group.id.clone(),
            Right {
                id: group.id.clone(),
                access: group.access,
                is_deleted: group.is_deleted,
            },
        );
    }

    return false;
}

fn print_to_trace_acl(azc: &mut AzContext, text: String) {
    if let Some(f) = azc.trace_acl {
        azc.str_num = azc.str_num + 1;
        let c_string = CString::new(azc.str_num.to_string() + " " + &text).unwrap();
        f(c_string.as_ptr());
    }
}

fn print_to_trace_group(azc: &mut AzContext, text: String) {
    if let Some(f) = azc.trace_group {
        azc.str_num = azc.str_num + 1;
        let c_string = CString::new(azc.str_num.to_string() + " " + &text).unwrap();
        f(c_string.as_ptr());
    }
}

fn print_to_trace_info(azc: &mut AzContext, text: String) {
    if let Some(f) = azc.trace_info {
        azc.str_num = azc.str_num + 1;
        let c_string = CString::new(azc.str_num.to_string() + " " + &text).unwrap();
        f(c_string.as_ptr());
    }
}

fn access_to_pretty_string(src: u8) -> String {
    let mut res: String = "".to_owned();

    if src & 1 == 1 {
        res.push_str("C ");
    }

    if src & 2 == 2 {
        res.push_str("R ");
    }

    if src & 4 == 4 {
        res.push_str("U ");
    }

    if src & 8 == 8 {
        res.push_str("D ");
    }

    return res;
}

#[no_mangle]
pub extern "C" fn authorize_r(
    _uri: *const c_char,
    _user_uri: *const c_char,
    _request_access: u8,
    _is_check_for_reload: bool,
    _trace_acl: Option<extern "C" fn(*const c_char)>,
    _trace_group: Option<extern "C" fn(*const c_char)>,
    _trace_info: Option<extern "C" fn(*const c_char)>,
) -> u8 {
    let c_uri: &CStr = unsafe { CStr::from_ptr(_uri) };
    let uri;
    match c_uri.to_str() {
        Ok(value) => uri = value,
        Err(e) => {
            println!("ERR! invalid param uri {:?}", e);
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
            let mut s_groups = &mut HashMap::new();
            let mut checked_groups = &mut HashMap::new();

            let mut is_trace_acl: bool = false;
            if let Some(_f) = _trace_acl {
                is_trace_acl = true
            }
            let mut is_trace_group: bool = false;
            if let Some(_f) = _trace_group {
                is_trace_group = true
            }
            let mut is_trace_info: bool = false;
            if let Some(_f) = _trace_info {
                is_trace_info = true
            }

            let mut azc = AzContext {
                request_access: _request_access,
                filter_value: &filter_value,
                calc_right_res: 0,
                walked_groups_s: walked_groups_s,
                walked_groups_o: walked_groups_o,
                subject_groups: subject_groups,
                checked_groups: checked_groups,
                trace_acl: _trace_acl,
                is_trace_acl: is_trace_acl,
                trace_group: _trace_group,
                is_trace_group: is_trace_group,
                trace_info: _trace_info,
                is_trace_info: is_trace_info,
                str_num: 0,
            };

            if azc.is_trace_info {
                print_to_trace_info(
                    &mut azc,
                    format!(
                        "authorize uri={}, user={}, request_access={}\n",
                        uri,
                        user_uri,
                        access_to_pretty_string(_request_access)
                    ),
                );
            }

            if azc.is_trace_info {
                print_to_trace_info(&mut azc, "READ SUBJECT GROUPS\n".to_string());
            }

            get_resource_groups(&mut azc, user_uri, 15, s_groups, 0, &db);

            azc.subject_groups = s_groups;

            azc.subject_groups.insert(
                user_uri.to_string(),
                Right {
                    id: user_uri.to_string(),
                    access: 15,
                    is_deleted: false,
                },
            );

            if azc.is_trace_info {
                let str = format!("subject_groups={:?}\n", azc.subject_groups);
                print_to_trace_info(&mut azc, str);
            }

            if azc.is_trace_info {
                print_to_trace_info(&mut azc, format!("PREPARE OBJECT GROUPS\n"));
            }

            if is_trace_info == false && is_trace_group == false && is_trace_acl == false {
                if authorize_obj_group(&mut azc, "v-s:AllResourcesGroup", 15, &db) == true {
                    return azc.calc_right_res;
                }

                if authorize_obj_group(&mut azc, uri, 15, &db) == true {
                    return azc.calc_right_res;
                }

                if prepare_obj_group(&mut azc, uri, 15, 0, &db) == true {
                    return azc.calc_right_res;
                }
            } else {
                // IF NEED TRACE

                if authorize_obj_group(&mut azc, "v-s:AllResourcesGroup", 15, &db) == true {
                    if azc.is_trace_info {
                        print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                    }
                }

                if authorize_obj_group(&mut azc, uri, 15, &db) == true {
                    if azc.is_trace_info {
                        print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                    }
                }

                let mut o_groups = &mut HashMap::new();
                if get_resource_groups(&mut azc, uri, 15, o_groups, 0, &db) == true {
                    if azc.is_trace_info {
                        print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                    }
                }

                if azc.is_trace_info {
                    let str = format!("object_groups={:?}\n", o_groups);
                    print_to_trace_info(&mut azc, str);
                }

                for object_group in o_groups.values() {
                    if authorize_obj_group(&mut azc, &object_group.id, object_group.access, &db) == true {
                        if azc.is_trace_info {
                            print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                        }
                    }
                }
            }

            if azc.is_trace_info {
                let calc_right_res = azc.calc_right_res;
                print_to_trace_info(
                    &mut azc,
                    format!(
                        "authorize {}, request={}, answer=[{}]\n\n",
                        uri,
                        access_to_pretty_string(_request_access),
                        access_to_pretty_string(calc_right_res)
                    ),
                );
            }

            return azc.calc_right_res;
        }
        Err(e) => {
            println!("ERR! Authorize:ON CREATING GET TRANSACTION {:?}", e);
        }
    }
    return 0;
}
