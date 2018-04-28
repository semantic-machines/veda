extern crate core;
/// This module gives function to check access of user to individual

#[macro_use]
extern crate lazy_static;
extern crate lmdb_rs_m;

use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;
use std::ptr;
use std::sync::Mutex;
use std::thread;
use std::time;
use std::time::SystemTime;

use lmdb_rs_m::core::{Database, EnvCreateNoLock, EnvCreateNoMetaSync, EnvCreateNoSync, EnvCreateReadOnly};
use lmdb_rs_m::{DbFlags, /*DbHandle,*/ EnvBuilder, Environment, MdbError};

const TRACE_ACL: u8 = 0;
const TRACE_GROUP: u8 = 1;
const TRACE_INFO: u8 = 2;

#[no_mangle]
pub extern "C" fn get_trace(_uri: *const c_char, _user_uri: *const c_char, _request_access: u8, trace_mode: u8, _is_check_for_reload: bool) -> *const c_char {
    let c_uri: &CStr = unsafe { CStr::from_ptr(_uri) };
    let uri;
    match c_uri.to_str() {
        Ok(value) => uri = value,
        Err(e) => {
            println!("ERR! invalid param uri {:?}", e);
            return ptr::null();
        }
    }

    let c_user_uri: &CStr = unsafe { CStr::from_ptr(_user_uri) };
    let user_uri;
    match c_user_uri.to_str() {
        Ok(value) => user_uri = value,
        Err(e) => {
            println!("ERR! invalid param user_uri {:?}", e);
            return ptr::null();
        }
    }

    let trace_acl = &mut String::new();
    let mut is_trace_acl = false;
    if trace_mode == TRACE_ACL {
        is_trace_acl = true;
    }

    let trace_group = &mut String::new();
    let mut is_trace_group = false;
    if trace_mode == TRACE_GROUP {
        is_trace_group = true;
    }

    let trace_info = &mut String::new();
    let mut is_trace_info = false;
    if trace_mode == TRACE_INFO {
        is_trace_info = true;
    }

    _authorize(
        &uri,
        &user_uri,
        _request_access,
        _is_check_for_reload,
        is_trace_acl,
        trace_acl,
        is_trace_group,
        trace_group,
        is_trace_info,
        trace_info,
    );

    let mut trace_res = &mut String::new();

    if trace_mode == TRACE_ACL {
        trace_res = trace_acl;
    } else if trace_mode == TRACE_GROUP {
        trace_res = trace_group;
    } else if trace_mode == TRACE_INFO {
        trace_res = trace_info;
    }

    //	println! ("trace_res={}", trace_res);

    //	let bytes = trace_res.into_bytes();
    let cres = CString::new(trace_res.clone()).unwrap();

    // http://jakegoulding.com/rust-ffi-omnibus/string_return/
    //cres.into_raw()

    let p = cres.as_ptr();

    std::mem::forget(cres);

    return p;
}

#[no_mangle]
pub extern "C" fn authorize_r(_uri: *const c_char, _user_uri: *const c_char, request_access: u8, is_check_for_reload: bool) -> u8 {
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

    let trace_acl = &mut String::new();
    let trace_group = &mut String::new();
    let trace_info = &mut String::new();

    return _authorize(uri, user_uri, request_access, is_check_for_reload, false, trace_acl, false, trace_group, false, trace_info);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const MODULE_INFO_PATH: &str = "./data/module-info/acl_preparer_info";
const DB_PATH: &str = "./data/acl-indexes/";

const PERMISSION_PREFIX: &str = "P";
const FILTER_PREFIX: &str = "F";
const MEMBERSHIP_PREFIX: &str = "M";

const ROLE_SUBJECT: u8 = 0;
const ROLE_OBJECT: u8 = 1;

static ACCESS_LIST: [u8; 4] = [1, 2, 4, 8];
static ACCESS_LIST_PREDICATES: [&str; 9] = ["", "v-s:canCreate", "v-s:canRead", "", "v-s:canUpdate", "", "", "", "v-s:canDelete"];

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
    calc_right_res: u8,
    walked_groups_s: &'a mut HashMap<String, u8>,
    walked_groups_o: &'a mut HashMap<String, u8>,
    subject_groups: &'a mut HashMap<String, Right>,
    checked_groups: &'a mut HashMap<String, u8>,
    //
    trace_acl: &'a mut String,
    is_trace_acl: bool,

    trace_group: &'a mut String,
    is_trace_group: bool,

    trace_info: &'a mut String,
    is_trace_info: bool,
    //
    str_num: u32,
}

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
                println! ("ERR! Authorize: Err opening environment: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                println! ("Retry");
            }
        }
    }
    println! ("LIB_AZ: Opened environment ./data/acl-indexes");
    env1
    }));

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

pub fn rights_vec_from_str(src: &str, results: &mut Vec<Right>) -> bool {
    if src.is_empty() {
        return false;
    }

    let tokens: Vec<&str> = src.split(';').collect();

    let mut idx = 0;
    loop {
        if idx + 1 < tokens.len() {
            let key = tokens[idx];
            let mut access = 0;
            let mut shift = 0;

            let mut element = tokens[idx + 1].chars();

            while let Some(c) = element.next() {
                match c.to_digit(16) {
                    Some(v) => access = access | (v << shift),
                    None => {
                        println!("ERR! rights_from_string, fail parse, access is not hex digit {}", src);
                        continue;
                    }
                }
                shift = shift + 4;
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

pub fn authorize_obj_group(azc: &mut AzContext, request_access: u8, object_group_id: &str, object_group_access: u8, filter_value: &str, db: &Database) -> bool {
    let mut is_authorized = false;
    let mut calc_bits;

    if azc.is_trace_info == false && azc.is_trace_group == false && azc.is_trace_acl == false {
        let left_to_check = (azc.calc_right_res ^ request_access) & request_access;

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

        azc.checked_groups.insert(object_group_id.to_string(), object_group_access);
    }

    if azc.is_trace_group == true {
        print_to_trace_group(azc, format!("{}\n", object_group_id));
    }

    let acl_key;
    if filter_value.is_empty() == false {
        acl_key = PERMISSION_PREFIX.to_owned() + filter_value + &object_group_id;
    } else {
        acl_key = PERMISSION_PREFIX.to_owned() + &object_group_id;
    }

    if azc.is_trace_info {
        print_to_trace_info(azc, format!("look acl_key: [{}]\n", acl_key));
    }

    let str = get_from_db(&acl_key, &db);
    if !str.is_empty() {
        let permissions: &mut Vec<Right> = &mut Vec::new();

        rights_vec_from_str(&str, permissions);

        for permission in permissions {
            if azc.is_trace_info {
                let req_acs = request_access;
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
                    if (request_access & access & restriction_access) != 0 {
                        calc_bits = access & permission_access;

                        if calc_bits > 0 {
                            azc.calc_right_res = azc.calc_right_res | calc_bits;

                            if (azc.calc_right_res & request_access) == request_access {
                                if azc.is_trace_info {
                                    let req_acs = request_access;
                                    let crr_acs = azc.calc_right_res;
                                    print_to_trace_info(
                                        azc,
                                        format!("EXIT? request_access={}, res={}\n", access_to_pretty_string(req_acs), access_to_pretty_string(crr_acs)),
                                    );
                                } else if azc.is_trace_group == false && azc.is_trace_acl == false {
                                    is_authorized = true;
                                    return is_authorized;
                                }
                            }

                            if azc.is_trace_info {
                                let crr_acs = azc.calc_right_res;
                                print_to_trace_info(azc, format!("calc_bits={}, res={}\n", access_to_pretty_string(calc_bits), access_to_pretty_string(crr_acs)));
                            }

                            if azc.is_trace_acl {
                                print_to_trace_acl(azc, format!("{};{};{}\n", object_group_id, subj_id, ACCESS_LIST_PREDICATES[*i_access as usize]));
                            }
                        }
                    }
                }
            }
        }

        if azc.is_trace_info {
            let str = get_from_db(&acl_key, &db);
            let permissions: &mut Vec<Right> = &mut Vec::new();
            rights_vec_from_str(&str, permissions);
            print_to_trace_info(azc, format!("for [{}] found {:?}\n", acl_key, permissions));
        }
    }

    if (azc.calc_right_res & request_access) == request_access {
        if azc.is_trace_info {
            let req_acs = request_access;
            let crr_acs = azc.calc_right_res;
            print_to_trace_info(
                azc,
                format!("EXIT? request_access={}, res={}\n", access_to_pretty_string(req_acs), access_to_pretty_string(crr_acs)),
            );
        }

        if azc.is_trace_info == false && azc.is_trace_group == false && azc.is_trace_acl == false {
            is_authorized = true;
            return is_authorized;
        }
    }

    return false;
}

pub fn prepare_obj_group(azc: &mut AzContext, request_access: u8, uri: &str, access: u8, filter_value: &str, level: u8, db: &Database) -> bool {
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
    rights_vec_from_str(&groups_str, groups_set);

    for idx in 0..groups_set.len() {
        let mut group = &mut groups_set[idx];

        if group.id.is_empty() {
            println!("WARN! WARN! group is null, uri={}, idx={}", uri, idx);
            continue;
        }

        //let orig_access = group.access;
        let new_access = group.access & access;
        group.access = new_access;

        let mut key = group.id.clone();
        let mut preur_access = 0;

        if azc.walked_groups_o.contains_key(&key) {
            preur_access = azc.walked_groups_o[&key];
            if (preur_access & new_access) == new_access {
                continue;
            }
        }
        azc.walked_groups_o.insert(key, new_access | preur_access);

        if uri == group.id {
            continue;
        }

        if authorize_obj_group(azc, request_access, &group.id, group.access, filter_value, &db) == true {
            return true;
        }

        prepare_obj_group(azc, request_access, &group.id, new_access, filter_value, level + 1, &db);
    }
    return false;
}

pub fn get_resource_groups(p_role: u8, azc: &mut AzContext, uri: &str, access: u8, results: &mut HashMap<String, Right>, filter_value: &str, level: u8, db: &Database) {
    if level > 32 {
        if azc.is_trace_info {
            print_to_trace_info(azc, format!("ERR! level down > 32, uri={}\n", uri));
        }
        return;
    }

    let groups_str = get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db);

    if groups_str.is_empty() {
        return;
    }

    let groups_set: &mut Vec<Right> = &mut Vec::new();
    rights_vec_from_str(&groups_str, groups_set);

    for idx in 0..groups_set.len() {
        let mut group = &mut groups_set[idx];

        if group.id.is_empty() {
            println!("WARN! WARN! group is null, uri={}, idx={}", uri, idx);
            continue;
        }

        let new_access = group.access & access;
        let orig_access = group.access;
        group.access = new_access;

        if p_role == ROLE_SUBJECT {
            let mut preur_access = 0;
            if azc.walked_groups_s.contains_key(&group.id) {
                preur_access = azc.walked_groups_s[&group.id];
                if (preur_access & new_access) == new_access {
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
            azc.walked_groups_s.insert(group.id.clone(), new_access | preur_access);
        } else {
            let mut preur_access = 0;
            if azc.walked_groups_o.contains_key(&group.id) {
                preur_access = azc.walked_groups_o[&group.id];
                if (preur_access & new_access) == new_access {
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
            azc.walked_groups_o.insert(group.id.clone(), new_access | preur_access);
        }

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

        get_resource_groups(p_role, azc, &group.id, 15, results, filter_value, level + 1, &db);
        results.insert(
            group.id.clone(),
            Right {
                id: group.id.clone(),
                access: group.access,
                is_deleted: group.is_deleted,
            },
        );
    }
}

fn print_to_trace_acl(azc: &mut AzContext, text: String) {
    azc.trace_acl.push_str(&text);
}

fn print_to_trace_group(azc: &mut AzContext, text: String) {
    azc.trace_group.push_str(&text);
}

fn print_to_trace_info(azc: &mut AzContext, text: String) {
    azc.str_num = azc.str_num + 1;
    azc.trace_info.push_str(&(azc.str_num.to_string() + " " + &text));
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

    if src & 16 == 16 {
        res.push_str("!C ");
    }

    if src & 32 == 32 {
        res.push_str("!R ");
    }

    if src & 64 == 64 {
        res.push_str("!U ");
    }

    if src & 128 == 128 {
        res.push_str("!D ");
    }

    return res;
}

pub fn _authorize(
    uri: &str, user_uri: &str, request_access: u8, _is_check_for_reload: bool, is_trace_acl: bool, _trace_acl: &mut String, is_trace_group: bool, _trace_group: &mut String,
    is_trace_info: bool, _trace_info: &mut String,
) -> u8 {
    fn check_for_reload() -> std::io::Result<bool> {
        use std::fs::File;
        let f = File::open(MODULE_INFO_PATH)?;

        let metadata = f.metadata()?;

        if let Ok(new_time) = metadata.modified() {
            let prev_time = LAST_MODIFIED_INFO.lock().unwrap().get_mut().clone();

            if new_time != prev_time {
                LAST_MODIFIED_INFO.lock().unwrap().replace(new_time);
                //println!("LAST_MODIFIED_INFO={:?}", new_time);
                return Ok(true);
            }
        }

        Ok(false)
    }

    if _is_check_for_reload == true {
        if let Ok(true) = check_for_reload() {
            //println!("INFO: Authorize: reopen db");

            let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

            match env_builder.open(DB_PATH, 0o644) {
                Ok(env_res) => {
                    ENV.lock().unwrap().replace(env_res);
                }
                Err(e) => {
                    println!("ERR! Authorize: Err opening environment: {:?}", e);
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
                println!("ERR! Authorize: Err opening db handle: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                println!("Retry");
            }
        }
    }

    let res;

    match env.get_reader() {
        Ok(txn) => {
            let mut walked_groups_s = &mut HashMap::new();
            let mut walked_groups_o = &mut HashMap::new();
            let mut subject_groups = &mut HashMap::new();
            let mut s_groups = &mut HashMap::new();
            let mut checked_groups = &mut HashMap::new();

            let mut azc = AzContext {
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

            let db = txn.bind(&db_handle);

            // 0. читаем фильтр прав у object (uri)
            let mut filter_value = get_from_db(&(FILTER_PREFIX.to_owned() + uri), &db);
            //println!("Authorize:filter_value=[{}]", filter_value);
            let mut filter_allow_access_to_other = 0;

            if filter_value.is_empty() == false {
                if filter_value.len() < 3 {
                    filter_value.clear();
                } else {
                    let filters_set: &mut Vec<Right> = &mut Vec::new();
                    rights_vec_from_str(&filter_value, filters_set);

                    if filters_set.len() > 0 {
                        let mut el = &mut filters_set[0];

                        filter_value = el.id.clone();
                        filter_allow_access_to_other = el.access;
                    }
                }
            }

            // читаем группы subject (ticket.user_uri)
            if azc.is_trace_info {
                print_to_trace_info(
                    &mut azc,
                    format!("authorize uri={}, user={}, request_access={}\n", uri, user_uri, access_to_pretty_string(request_access)),
                );
            }

            if azc.is_trace_info {
                print_to_trace_info(&mut azc, "READ SUBJECT GROUPS\n".to_string());
            }

            get_resource_groups(ROLE_SUBJECT, &mut azc, user_uri, 15, s_groups, &filter_value, 0, &db);

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

            let mut request_access_t = request_access;
            let empty_filter_value = String::new();

            if filter_value.is_empty() == false {
                request_access_t = request_access & filter_allow_access_to_other;
            }

            if is_trace_info == false && is_trace_group == false && is_trace_acl == false {
                if authorize_obj_group(&mut azc, request_access_t, "v-s:AllResourcesGroup", 15, &empty_filter_value, &db) == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        return azc.calc_right_res;
                    }
                }

                if authorize_obj_group(&mut azc, request_access_t, uri, 15, &empty_filter_value, &db) == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        return azc.calc_right_res;
                    }
                }

                if prepare_obj_group(&mut azc, request_access_t, uri, 15, &empty_filter_value, 0, &db) == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        return azc.calc_right_res;
                    }
                }

                if filter_value.is_empty() == false {
                    if authorize_obj_group(&mut azc, request_access, "v-s:AllResourcesGroup", 15, &filter_value, &db) == true {
                        return azc.calc_right_res;
                    }

                    if authorize_obj_group(&mut azc, request_access, uri, 15, &filter_value, &db) == true {
                        return azc.calc_right_res;
                    }

                    if prepare_obj_group(&mut azc, request_access, uri, 15, &filter_value, 0, &db) == true {
                        return azc.calc_right_res;
                    }
                }
            } else {
                // IF NEED TRACE

                if authorize_obj_group(&mut azc, request_access_t, "v-s:AllResourcesGroup", 15, &empty_filter_value, &db) == true {
                    if azc.is_trace_info {
                        print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                    }
                }

                if authorize_obj_group(&mut azc, request_access_t, uri, 15, &empty_filter_value, &db) == true {
                    if azc.is_trace_info {
                        print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                    }
                }

                let mut o_groups = &mut HashMap::new();
                get_resource_groups(ROLE_OBJECT, &mut azc, uri, 15, o_groups, &empty_filter_value, 0, &db);

                if azc.is_trace_info {
                    let str = format!("object_groups={:?}\n", o_groups);
                    print_to_trace_info(&mut azc, str);
                }

                for object_group in o_groups.values() {
                    if authorize_obj_group(&mut azc, request_access_t, &object_group.id, object_group.access, &empty_filter_value, &db) == true {
                        if azc.is_trace_info {
                            print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                        }
                    }
                }

                if filter_value.is_empty() == false {
                    if authorize_obj_group(&mut azc, request_access, "v-s:AllResourcesGroup", 15, &filter_value, &db) == true {
                        if azc.is_trace_info {
                            print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                        }
                    }

                    if authorize_obj_group(&mut azc, request_access, uri, 15, &filter_value, &db) == true {
                        if azc.is_trace_info {
                            print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                        }
                    }

                    let mut o_groups = &mut HashMap::new();
                    get_resource_groups(ROLE_OBJECT, &mut azc, uri, 15, o_groups, &filter_value, 0, &db);

                    if azc.is_trace_info {
                        let str = format!("object_groups={:?}\n", o_groups);
                        print_to_trace_info(&mut azc, str);
                    }

                    for object_group in o_groups.values() {
                        if authorize_obj_group(&mut azc, request_access, &object_group.id, object_group.access, &filter_value, &db) == true {
                            if azc.is_trace_info {
                                print_to_trace_info(&mut azc, format!("RETURN MY BE ASAP\n"));
                            }
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
                        access_to_pretty_string(request_access),
                        access_to_pretty_string(calc_right_res)
                    ),
                );
            }

            res = azc.calc_right_res;
        }
        Err(e) => {
            println!("ERR! Authorize:ON CREATING GET TRANSACTION {:?}", e);
            println!("reopen db");

            let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

            match env_builder.open(DB_PATH, 0o644) {
                Ok(env_res) => {
                    ENV.lock().unwrap().replace(env_res);
                }
                Err(e) => {
                    println!("ERR! Authorize: Err opening environment: {:?}", e);
                }
            }

            return _authorize(
                uri,
                user_uri,
                request_access,
                _is_check_for_reload,
                is_trace_acl,
                _trace_acl,
                is_trace_group,
                _trace_group,
                is_trace_info,
                _trace_info,
            );
        }
    }
    return res;
}
