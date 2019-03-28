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
use lmdb_rs_m::{DbFlags, /* DbHandle, */ EnvBuilder, Environment, MdbError};

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
            eprintln!("ERR! invalid param uri {:?}", e);
            return ptr::null();
        },
    }

    let c_user_uri: &CStr = unsafe { CStr::from_ptr(_user_uri) };
    let user_uri;
    match c_user_uri.to_str() {
        Ok(value) => user_uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param user_uri {:?}", e);
            return ptr::null();
        },
    }

    let _trace_acl = &mut String::new();
    let mut is_acl = false;
    if trace_mode == TRACE_ACL {
        is_acl = true;
    }

    let _trace_group = &mut String::new();
    let mut is_group = false;
    if trace_mode == TRACE_GROUP {
        is_group = true;
    }

    let _trace_info = &mut String::new();
    let mut is_info = false;
    if trace_mode == TRACE_INFO {
        is_info = true;
    }

    let mut trace = Trace {
        acl: _trace_acl,
        is_acl: is_acl,
        group: _trace_group,
        is_group: is_group,
        info: _trace_info,
        is_info: is_info,
        str_num: 0,
    };

    match _authorize(&uri, &user_uri, _request_access, _is_check_for_reload, &mut trace) {
        Ok(_) => {},
        Err(_) => {},
    }

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

    return p;
}

#[no_mangle]
pub extern "C" fn authorize_r(_uri: *const c_char, _user_uri: *const c_char, request_access: u8, is_check_for_reload: bool) -> u8 {
    let c_uri: &CStr = unsafe { CStr::from_ptr(_uri) };
    let uri;
    match c_uri.to_str() {
        Ok(value) => uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param uri {:?}", e);
            return 0;
        },
    }

    let c_user_uri: &CStr = unsafe { CStr::from_ptr(_user_uri) };
    let user_uri;
    match c_user_uri.to_str() {
        Ok(value) => user_uri = value,
        Err(e) => {
            eprintln!("ERR! invalid param user_uri {:?}", e);
            return 0;
        },
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
            Err(_e) => {},
        }
    }
    return 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const MODULE_INFO_PATH: &str = "./data/module-info/acl_preparer_info";
const DB_PATH: &str = "./data/acl-indexes/";

const PERMISSION_PREFIX: &str = "P";
const FILTER_PREFIX: &str = "F";
const MEMBERSHIP_PREFIX: &str = "M";

const M_IS_EXCLUSIVE: char = 'X';
const M_IGNORE_EXCLUSIVE: char = 'N';

static ACCESS_LIST: [u8; 4] = [1, 2, 4, 8];
static ACCESS_LIST_PREDICATES: [&str; 9] = ["", "v-s:canCreate", "v-s:canRead", "", "v-s:canUpdate", "", "", "", "v-s:canDelete"];

pub struct Right {
    id: String,
    access: u8,
    marker: char,
    is_deleted: bool,
    level: u8,
}

impl fmt::Debug for Right {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "({}, {}, {}, {})", self.id, access_to_pretty_string(self.access), self.marker, self.level) }
}

pub struct Trace<'a> {
    acl: &'a mut String,
    is_acl: bool,

    group: &'a mut String,
    is_group: bool,

    info: &'a mut String,
    is_info: bool,

    str_num: u32,
}

pub struct AzContext<'a> {
    uri: &'a str,
    user_uri: &'a str,
    request_access: u8,
    calc_right_res: u8,
    is_need_exclusive_az: bool,
    is_found_exclusive_az: bool,
    walked_groups_s: &'a mut HashMap<String, (u8, char)>,
    tree_groups_s: &'a mut HashMap<String, String>,
    walked_groups_o: &'a mut HashMap<String, u8>,
    tree_groups_o: &'a mut HashMap<String, String>,
    subject_groups: &'a mut HashMap<String, Right>,
    checked_groups: &'a mut HashMap<String, u8>,
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

fn get_from_db(key: &str, db: &Database) -> Result<String, i64> {
    match db.get::<String>(&key) {
        Ok(val) => {
            return Ok(val);
        },
        Err(e) => match e {
            MdbError::NotFound => {
                return Err(0);
            },
            _ => {
                eprintln!("ERR! Authorize: db.get {:?}, {}", e, key);
                return Err(-1);
            },
        },
    }
}

fn rights_vec_from_str(src: &str, results: &mut Vec<Right>) -> bool {
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
            let mut marker = 0 as char;

            let mut element = tokens[idx + 1].chars();

            let mut pos = 0;

            while let Some(c) = element.next() {
                if c == M_IS_EXCLUSIVE {
                    marker = c;
                } else {
                    pos = pos + 1;
                    match c.to_digit(16) {
                        Some(v) => access = access | (v << shift),
                        None => {
                            eprintln!("ERR! rights_from_string, fail parse, access is not hex digit {}", src);
                            continue;
                        },
                    }
                    shift = shift + 4;
                }
            }

            let rr = Right {
                id: key.to_string(),
                access: access as u8,
                marker: marker,
                is_deleted: false,
                level: 0,
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

fn authorize_obj_group(
    azc: &mut AzContext,
    trace: &mut Trace,
    request_access: u8,
    object_group_id: &str,
    object_group_access: u8,
    filter_value: &str,
    db: &Database,
) -> Result<bool, i64> {
    let mut is_authorized = false;
    let mut calc_bits;

    if trace.is_info == false && trace.is_group == false && trace.is_acl == false {
        let left_to_check = (azc.calc_right_res ^ request_access) & request_access;

        if left_to_check & object_group_access == 0 {
            return Ok(is_authorized);
        }

        match azc.checked_groups.get(object_group_id) {
            Some(v) => {
                if *v == object_group_access {
                    return Ok(is_authorized);
                }
            },
            None => (),
        }

        azc.checked_groups.insert(object_group_id.to_string(), object_group_access);
    }

    if trace.is_group == true {
        print_to_trace_group(trace, format!("{}\n", object_group_id));
    }

    let acl_key;
    if filter_value.is_empty() == false {
        acl_key = PERMISSION_PREFIX.to_owned() + filter_value + &object_group_id;
    } else {
        acl_key = PERMISSION_PREFIX.to_owned() + &object_group_id;
    }

    //        if trace.is_info {
    //            print_to_trace_info(trace, format!("look acl_key: [{}]\n",
    // acl_key));        }

    match get_from_db(&acl_key, &db) {
        Ok(str) => {
            let permissions: &mut Vec<Right> = &mut Vec::new();

            rights_vec_from_str(&str, permissions);

            for permission in permissions {
                /*
                                                if trace.is_info {
                                                    let req_acs = request_access;
                                                    print_to_trace_info(
                                                        trace,
                                                        format!(
                                                            "restriction={} {}, permission={:?}, request={}\n",
                                                            object_group_id,
                                                            access_to_pretty_string(object_group_access),
                                                            permission,
                                                            access_to_pretty_string(req_acs)
                                                        ),
                                                    );
                                                }
                */

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
                                let prev_res = azc.calc_right_res;

                                azc.calc_right_res = azc.calc_right_res | calc_bits;

                                if (azc.calc_right_res & request_access) == request_access {
                                    if trace.is_info {
                                    } else if trace.is_group == false && trace.is_acl == false {
                                        is_authorized = true;
                                        return Ok(is_authorized);
                                    }
                                }

                                if trace.is_info {
                                    if prev_res != azc.calc_right_res {
                                        let mut f_log_str = "".to_owned();
                                        if filter_value.is_empty() == false {
                                            f_log_str = ", with filter ".to_owned() + filter_value;
                                        }

                                        print_to_trace_info(
                                            trace,
                                            format!(
                                                "found permission S:[{}], O:[{}], access={} {}\n",
                                                &subj_id,
                                                &object_group_id,
                                                access_to_pretty_string(permission_access),
                                                f_log_str
                                            ),
                                        );

                                        print_to_trace_info(
                                            trace,
                                            format!(
                                                "access: request={}, calc={}, total={}\n",
                                                access_to_pretty_string(request_access),
                                                access_to_pretty_string(calc_bits),
                                                access_to_pretty_string(azc.calc_right_res)
                                            ),
                                        );
                                        print_to_trace_info(trace, "O:PATH".to_owned() + &get_path(azc.tree_groups_o, object_group_id.to_string()) + "\n");
                                        print_to_trace_info(trace, "S:PATH".to_owned() + &get_path(azc.tree_groups_s, subj_id.to_string()) + "\n");
                                    }
                                }

                                if trace.is_acl {
                                    print_to_trace_acl(trace, format!("{};{};{}\n", object_group_id, subj_id, ACCESS_LIST_PREDICATES[*i_access as usize]));
                                }
                            }
                        }
                    }
                }
            }
        },
        Err(e) => {
            if e < 0 {
                eprintln!("ERR! Authorize: authorize_obj_group:main, object_group_id={:?}", object_group_id);
                return Err(e);
            }
        },
    }

    /*    if trace.is_info {
        match get_from_db(&acl_key, &db) {
            Ok(str) => {
                let permissions: &mut Vec<Right> = &mut Vec::new();
                rights_vec_from_str(&str, permissions);
                print_to_trace_info(trace, format!("for [{}] found {:?}\n", acl_key, permissions));
            },
            Err(e) => if e < 0 {
                eprintln!("ERR! Authorize: authorize_obj_group:trace, object_group_id={:?}", object_group_id);
                return Err(e);
            },
        }
    }*/

    if (azc.calc_right_res & request_access) == request_access {
        if trace.is_info {
            /*            let req_acs = request_access;
                        let crr_acs = azc.calc_right_res;
                        print_to_trace_info(
                            trace,
                            format!("EXIT? request_access={}, res={}\n", access_to_pretty_string(req_acs), access_to_pretty_string(crr_acs)),
                        );
            */

        }

        if trace.is_info == false && trace.is_group == false && trace.is_acl == false {
            is_authorized = true;
            return Ok(is_authorized);
        }
    }

    return Ok(false);
}

fn prepare_obj_group(azc: &mut AzContext, trace: &mut Trace, request_access: u8, uri: &str, access: u8, filter_value: &str, level: u8, db: &Database) -> Result<bool, i64> {
    if level > 32 {
        //        if trace.is_info {
        //            print_to_trace_info(trace, format!("ERR! level down > 32,
        // uri={}\n", uri));        }

        return Ok(false);
    }

    let mut is_contain_suffix_group = false;
    let groups_set_len;

    match get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db) {
        Ok(groups_str) => {
            let groups_set: &mut Vec<Right> = &mut Vec::new();
            rights_vec_from_str(&groups_str, groups_set);

            groups_set_len = groups_set.len();

            for idx in 0..groups_set_len {
                let mut group = &mut groups_set[idx];

                if group.id.is_empty() {
                    eprintln!("WARN! skip, group is null, uri={}, group.id={}", uri, group.id);
                    continue;
                }

                let new_access = group.access & access;
                group.access = new_access;

                let mut key = group.id.clone();

                if azc.is_need_exclusive_az == true && azc.is_found_exclusive_az == false {
                    if level == 0 {
                        if group.id.contains("_group") {
                            is_contain_suffix_group = true;
                        }

                        if idx == groups_set_len - 1 && is_contain_suffix_group == false {
                            azc.is_found_exclusive_az = true;
                        }

                        if group.id.contains("cfg:TTLResourcesGroup") {
                            azc.is_found_exclusive_az = true;
                        }
                    }

                    if azc.is_found_exclusive_az == false && (level == 0 || uri.contains("_group")) {
                        if azc.subject_groups.contains_key(&key) {
                            match azc.subject_groups.get(&key) {
                                Some(s_val) => {
                                    if s_val.marker == M_IS_EXCLUSIVE {
                                        azc.is_found_exclusive_az = true;
                                    }
                                },
                                None => (),
                            }
                        }
                    }
                }

                let mut preur_access = 0;

                if azc.walked_groups_o.contains_key(&key) {
                    preur_access = azc.walked_groups_o[&key];
                    if (preur_access & new_access) == new_access {
                        continue;
                    }
                }

                if trace.is_info {
                    azc.walked_groups_o.insert(key.clone(), new_access | preur_access);
                    azc.tree_groups_o.insert(key.clone(), uri.to_string());
                } else {
                    azc.walked_groups_o.insert(key.clone(), new_access | preur_access);
                }

                if uri == group.id {
                    continue;
                }

                //if trace.is_info {
                //	print_to_trace_info(trace, format!("({})prepare group: [{}]\n", level,
                // uri));
                //}

                match authorize_obj_group(azc, trace, request_access, &group.id, group.access, filter_value, &db) {
                    Ok(res) => {
                        if res == true {
                            if azc.is_need_exclusive_az == false {
                                return Ok(true);
                            }

                            if azc.is_need_exclusive_az == true && azc.is_found_exclusive_az == true {
                                return Ok(true);
                            }
                        }
                    },
                    Err(e) => {
                        if e < 0 {
                            return Err(e);
                        }
                    },
                }

                try!(prepare_obj_group(azc, trace, request_access, &group.id, new_access, filter_value, level + 1, &db));
            }

            if groups_set_len == 0 {
                azc.is_found_exclusive_az = true;
            }

            return Ok(false);
        },
        Err(e) => {
            if e < 0 {
                eprintln!("ERR! Authorize: prepare_obj_group {:?}", uri);
                return Err(e);
            } else {
                if level == 0 {
                    azc.is_found_exclusive_az = true;
                }
                return Ok(false);
            }
        },
    }
}

fn get_resource_groups(
    walked_groups: &mut HashMap<String, (u8, char)>,
    tree_groups: &mut HashMap<String, String>,
    trace: &mut Trace,
    uri: &str,
    access: u8,
    results: &mut HashMap<String, Right>,
    filter_value: &str,
    level: u8,
    db: &Database,
    out_f_is_exclusive: &mut bool,
    ignore_exclusive: bool,
) -> Result<bool, i64> {
    if level > 32 {
        //        if trace.is_info {
        //            print_to_trace_info(trace, format!("ERR! level down > 32,
        // uri={}\n", uri));        }
        return Ok(true);
    }

    match get_from_db(&(MEMBERSHIP_PREFIX.to_owned() + uri), &db) {
        Ok(groups_str) => {
            let groups_set: &mut Vec<Right> = &mut Vec::new();
            rights_vec_from_str(&groups_str, groups_set);

            for idx in 0..groups_set.len() {
                let mut group = &mut groups_set[idx];

                if group.id.is_empty() {
                    eprintln!("WARN! WARN! group is null, uri={}, idx={}", uri, idx);
                    continue;
                }

                let new_access = group.access & access;
                group.access = new_access;

                let mut preur_access = 0;
                if walked_groups.contains_key(&group.id) {
                    preur_access = walked_groups[&group.id].0;
                    if (preur_access & new_access) == new_access && group.marker == walked_groups[&group.id].1 {
                        /*                           if trace.is_info {
                                                        print_to_trace_info(
                                                            trace,
                                                            format!(
                                                                "{:1$} ({})GROUP [{}].access={} SKIP, ALREADY ADDED\n",
                                                                level * 2,
                                                                level as usize,
                                                                group.id,
                                                                access_to_pretty_string(preur_access)
                                                            ),
                                                        );
                                                    }
                        */

                        continue;
                    }
                }

                walked_groups.insert(group.id.clone(), ((new_access | preur_access), group.marker));

                if trace.is_info {
                    tree_groups.insert(group.id.clone(), uri.to_string());
                }

                /*
                                if trace.is_info {
                                    print_to_trace_info(
                                        trace,
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
                */
                if uri == group.id {
                    /*                    if trace.is_info {
                        print_to_trace_info(
                            trace,
                            format!(
                                "{:1$} ({})GROUP [{}].access={} SKIP, uri == group_key\n",
                                level * 2,
                                level as usize,
                                group.id,
                                access_to_pretty_string(orig_access)
                            ),
                        );
                    } */
                    continue;
                }

                let mut t_ignore_exclusive = ignore_exclusive;

                if ignore_exclusive == false && group.marker == M_IGNORE_EXCLUSIVE {
                    t_ignore_exclusive = true;
                }

                match get_resource_groups(walked_groups, tree_groups, trace, &group.id, 15, results, filter_value, level + 1, &db, out_f_is_exclusive, t_ignore_exclusive) {
                    Ok(_res) => {},
                    Err(e) => {
                        if e < 0 {
                            return Err(e);
                        }
                    },
                }

                if ignore_exclusive == false && group.marker == M_IS_EXCLUSIVE {
                    if trace.is_info {
                        print_to_trace_info(trace, format!("FOUND EXCLUSIVE RESTRICTIONS, PATH={} \n", &get_path(tree_groups, group.id.clone())));
                    }
                    *out_f_is_exclusive = true;
                }

                let new_group_marker;

                match results.get(&group.id) {
                    Some(val) => {
                        if val.marker == 0 as char {
                            new_group_marker = group.marker;
                        } else {
                            new_group_marker = val.marker;
                        }
                    },
                    None => {
                        new_group_marker = group.marker;
                    },
                }

                results.insert(
                    group.id.clone(),
                    Right {
                        id: group.id.clone(),
                        access: group.access,
                        marker: new_group_marker,
                        is_deleted: group.is_deleted,
                        level: level,
                    },
                );
            }
        },
        Err(e) => {
            if e < 0 {
                eprintln!("ERR! Authorize: get_resource_groups {:?}", uri);
                return Err(e);
            } else {
                return Ok(false);
            }
        },
    }

    return Ok(false);
}

fn print_to_trace_acl(trace: &mut Trace, text: String) { trace.acl.push_str(&text); }

fn print_to_trace_group(trace: &mut Trace, text: String) { trace.group.push_str(&text); }

fn print_to_trace_info(trace: &mut Trace, text: String) {
    trace.str_num = trace.str_num + 1;
    trace.info.push_str(&(trace.str_num.to_string() + " " + &text));
}

fn get_path(mopc: &mut HashMap<String, String>, el: String) -> String {
    if mopc.contains_key(&el) {
        let parent = mopc[&el].clone();
        mopc.remove(&el);
        let prev = get_path(mopc, parent.to_string());

        return prev + "->" + &el;
    } else {
        return "".to_owned();
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

fn _authorize(uri: &str, user_uri: &str, request_access: u8, _is_check_for_reload: bool, trace: &mut Trace) -> Result<u8, i64> {
    let walked_groups_s = &mut HashMap::new();
    let tree_groups_s = &mut HashMap::new();
    let walked_groups_o = &mut HashMap::new();
    let tree_groups_o = &mut HashMap::new();
    let subject_groups = &mut HashMap::new();
    let s_groups = &mut HashMap::new();
    let checked_groups = &mut HashMap::new();

    let mut azc = AzContext {
        uri: uri,
        user_uri: user_uri,
        request_access: request_access,
        calc_right_res: 0,
        is_need_exclusive_az: false,
        is_found_exclusive_az: false,
        walked_groups_s: walked_groups_s,
        tree_groups_s: tree_groups_s,
        walked_groups_o: walked_groups_o,
        tree_groups_o: tree_groups_o,
        subject_groups: subject_groups,
        checked_groups: checked_groups,
    };

    fn check_for_reload() -> std::io::Result<bool> {
        use std::fs::File;
        let f = File::open(MODULE_INFO_PATH)?;

        let metadata = f.metadata()?;

        if let Ok(new_time) = metadata.modified() {
            let prev_time = LAST_MODIFIED_INFO.lock().unwrap().get_mut().clone();

            if new_time != prev_time {
                LAST_MODIFIED_INFO.lock().unwrap().replace(new_time);
                //eprintln!("LAST_MODIFIED_INFO={:?}", new_time);
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn final_check(azc: &mut AzContext, trace: &mut Trace) -> bool {
        let mut res = false;

        if azc.is_need_exclusive_az == false {
            res = true;
        }

        if azc.is_need_exclusive_az == true && azc.is_found_exclusive_az == true {
            res = true;
        }

        if trace.is_info == true && res == true {
            print_to_trace_info(
                trace,
                format!(
                    "result: uri={}, user={}, request={}, answer={}\n\n",
                    azc.uri,
                    azc.user_uri,
                    access_to_pretty_string(azc.request_access),
                    access_to_pretty_string(azc.calc_right_res)
                ),
            );
        }

        return res;
    }

    if _is_check_for_reload == true {
        if let Ok(true) = check_for_reload() {
            //eprintln!("INFO: Authorize: reopen db");

            let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

            match env_builder.open(DB_PATH, 0o644) {
                Ok(env_res) => {
                    ENV.lock().unwrap().replace(env_res);
                },
                Err(e) => {
                    eprintln!("ERR! Authorize: Err opening environment: {:?}", e);
                },
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
            },
            Err(e) => {
                eprintln!("ERR! Authorize: Err opening db handle: {:?}", e);
                thread::sleep(time::Duration::from_secs(3));
                eprintln!("Retry");
            },
        }
    }

    let txn;
    match env.get_reader() {
        Ok(txn1) => {
            txn = txn1;
        },
        Err(e) => {
            eprintln!("ERR! Authorize:CREATING TRANSACTION {:?}", e);
            eprintln!("reopen db");

            let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

            match env_builder.open(DB_PATH, 0o644) {
                Ok(env_res) => {
                    ENV.lock().unwrap().replace(env_res);
                },
                Err(e) => {
                    eprintln!("ERR! Authorize: Err opening environment: {:?}", e);
                },
            }

            return _authorize(uri, user_uri, request_access, _is_check_for_reload, trace);
        },
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
                rights_vec_from_str(&filter_value, filters_set);

                if filters_set.len() > 0 {
                    let mut el = &mut filters_set[0];

                    filter_value = el.id.clone();
                    filter_allow_access_to_other = el.access;
                }
            }
            //eprintln!("Authorize:uri=[{}], filter_value=[{}]", uri, filter_value);
        },
        Err(e) => {
            if e == 0 {
                filter_value = String::new();
            } else {
                eprintln!("ERR! Authorize: _authorize {:?}", uri);
                return Err(e);
            }
        },
    }

    // читаем группы subject (ticket.user_uri)
    if trace.is_info {
        print_to_trace_info(trace, format!("authorize uri={}, user={}, request_access={}\n", uri, user_uri, access_to_pretty_string(request_access)));
    }

    //   if trace.is_info {
    //       print_to_trace_info(trace, "READ SUBJECT GROUPS\n".to_string());
    //   }

    match get_resource_groups(azc.walked_groups_s, azc.tree_groups_s, trace, user_uri, 15, s_groups, &filter_value, 0, &db, &mut azc.is_need_exclusive_az, false) {
        Ok(_res) => {},
        Err(e) => return Err(e),
    }

    azc.subject_groups = s_groups;

    azc.subject_groups.insert(
        user_uri.to_string(),
        Right {
            id: user_uri.to_string(),
            access: 15,
            marker: 0 as char,
            is_deleted: false,
            level: 0,
        },
    );

    //    if trace.is_info {
    //        let str = format!("subject_groups={:?}\n", azc.subject_groups);
    //        print_to_trace_info(trace, str);
    //    }

    let mut request_access_t = request_access;
    let empty_filter_value = String::new();

    if filter_value.is_empty() == false {
        request_access_t = request_access & filter_allow_access_to_other;
    }

    if trace.is_info == false && trace.is_group == false && trace.is_acl == false {
        match authorize_obj_group(&mut azc, trace, request_access_t, "v-s:AllResourcesGroup", 15, &empty_filter_value, &db) {
            Ok(res) => {
                if res == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                }
            },
            Err(e) => return Err(e),
        }

        match authorize_obj_group(&mut azc, trace, request_access_t, uri, 15, &empty_filter_value, &db) {
            Ok(res) => {
                if res == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                }
            },
            Err(e) => return Err(e),
        }

        match prepare_obj_group(&mut azc, trace, request_access_t, uri, 15, &empty_filter_value, 0, &db) {
            Ok(res) => {
                if res == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                }
            },
            Err(e) => return Err(e),
        }

        if filter_value.is_empty() == false {
            azc.checked_groups.clear();
            azc.walked_groups_o.clear();

            match authorize_obj_group(&mut azc, trace, request_access, "v-s:AllResourcesGroup", 15, &filter_value, &db) {
                Ok(res) => {
                    if res == true {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                },
                Err(e) => return Err(e),
            }

            match authorize_obj_group(&mut azc, trace, request_access, uri, 15, &filter_value, &db) {
                Ok(res) => {
                    if res == true {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                },
                Err(e) => return Err(e),
            }

            match prepare_obj_group(&mut azc, trace, request_access, uri, 15, &filter_value, 0, &db) {
                Ok(res) => {
                    if res == true {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                },
                Err(e) => return Err(e),
            }
        }
    } else {
        // IF NEED TRACE

        match authorize_obj_group(&mut azc, trace, request_access_t, "v-s:AllResourcesGroup", 15, &empty_filter_value, &db) {
            Ok(res) => {
                if res == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        //                    if trace.is_info {
                        //                        print_to_trace_info(trace, format!("RETURN MY BE
                        // ASAP\n"));                    }
                    } else {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                }
            },
            Err(e) => return Err(e),
        }

        match authorize_obj_group(&mut azc, trace, request_access_t, uri, 15, &empty_filter_value, &db) {
            Ok(res) => {
                if res == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        //                    if trace.is_info {
                        //                        print_to_trace_info(trace, format!("RETURN MY BE
                        // ASAP\n"));                    }
                    } else {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                }
            },
            Err(e) => return Err(e),
        }

        match prepare_obj_group(&mut azc, trace, request_access_t, uri, 15, &empty_filter_value, 0, &db) {
            Ok(res) => {
                if res == true {
                    if filter_value.is_empty() || (filter_value.is_empty() == false && request_access == azc.calc_right_res) {
                        //                    if trace.is_info {
                        //                        print_to_trace_info(trace, format!("RETURN MY BE
                        // ASAP\n"));                    }
                    }
                } else {
                    if final_check(&mut azc, trace) {
                        return Ok(azc.calc_right_res);
                    }
                }
            },
            Err(e) => return Err(e),
        }

        if filter_value.is_empty() == false {
            if trace.is_info {
                print_to_trace_info(trace, format!("USE FILTER: [{}]\n", filter_value));
            }

            azc.checked_groups.clear();
            azc.walked_groups_o.clear();

            match authorize_obj_group(&mut azc, trace, request_access, "v-s:AllResourcesGroup", 15, &filter_value, &db) {
                Ok(res) => {
                    if res == true {
                        //                    if trace.is_info {
                        //                        print_to_trace_info(trace, format!("RETURN MY BE
                        // ASAP\n"));                    }
                    } else {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                },
                Err(e) => return Err(e),
            }

            match authorize_obj_group(&mut azc, trace, request_access, uri, 15, &filter_value, &db) {
                Ok(res) => {
                    if res == true {
                        //                    if trace.is_info {
                        //                        print_to_trace_info(trace, format!("RETURN MY BE
                        // ASAP\n"));                    }
                    } else {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                },
                Err(e) => return Err(e),
            }

            match prepare_obj_group(&mut azc, trace, request_access, uri, 15, &filter_value, 0, &db) {
                Ok(res) => {
                    if res == true {
                        //                    if trace.is_info {
                        //                        print_to_trace_info(trace, format!("RETURN MY BE
                        // ASAP\n"));                    }
                    } else {
                        if final_check(&mut azc, trace) {
                            return Ok(azc.calc_right_res);
                        }
                    }
                },
                Err(e) => return Err(e),
            }
        }
    }

    if final_check(&mut azc, trace) {
        return Ok(azc.calc_right_res);
    } else {
        if trace.is_acl {
            trace.acl.clear();
        }

        if trace.is_info {
            print_to_trace_info(
                trace,
                format!("result: uri={}, user={}, request={}, answer={}\n\n", azc.uri, azc.user_uri, access_to_pretty_string(azc.request_access), access_to_pretty_string(0)),
            );
        }

        return Ok(0);
    }
}
