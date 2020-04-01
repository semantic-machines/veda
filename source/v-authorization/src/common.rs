use crate::{AzContext, Right};
use core::fmt;
use std::collections::HashMap;

pub(crate) const PERMISSION_PREFIX: &str = "P";
pub const FILTER_PREFIX: &str = "F";
pub(crate) const MEMBERSHIP_PREFIX: &str = "M";
pub(crate) const M_IS_EXCLUSIVE: char = 'X';
const M_IGNORE_EXCLUSIVE: char = 'N';
pub(crate) static ACCESS_LIST: [u8; 4] = [1, 2, 4, 8];
pub(crate) static ACCESS_LIST_PREDICATES: [&str; 9] = ["", "v-s:canCreate", "v-s:canRead", "", "v-s:canUpdate", "", "", "", "v-s:canDelete"];

/// Битовые поля для прав
#[repr(u8)]
pub enum Access {
    /// Создание
    CanCreate = 1u8,

    /// Чтение
    CanRead = 2u8,

    /// Изменеие
    CanUpdate = 4u8,

    /// Удаление
    CanDelete = 8u8,

    /// Запрет создания
    CantCreate = 16u8,

    /// Запрет чтения
    CantRead = 32u8,

    /// Запрет обновления
    CantUpdate = 64u8,

    /// Запрет удаления
    CantDelete = 128u8,
}

pub trait Storage {
    fn get(&self, key: &str) -> Result<String, i64>;
    fn fiber_yield(&self);
}

impl fmt::Debug for Right {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}, {}, {})", self.id, access_to_pretty_string(self.access), self.marker, self.level)
    }
}

pub struct Trace<'a> {
    pub acl: &'a mut String,
    pub is_acl: bool,

    pub group: &'a mut String,
    pub is_group: bool,

    pub info: &'a mut String,
    pub is_info: bool,

    pub str_num: u32,
}

pub fn get_elements_from_index(src: &str, results: &mut Vec<Right>) -> bool {
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

            for c in tokens[idx + 1].chars() {
                if c == M_IS_EXCLUSIVE || c == M_IGNORE_EXCLUSIVE {
                    marker = c;
                } else {
                    match c.to_digit(16) {
                        Some(v) => access |= v << shift,
                        None => {
                            eprintln!("ERR! rights_from_string, fail parse, access is not hex digit {}", src);
                            continue;
                        }
                    }
                    shift += 4;
                }
            }

            let rr = Right {
                id: key.to_string(),
                access: access as u8,
                marker,
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

    true
}

pub(crate) fn get_resource_groups(
    walked_groups: &mut HashMap<String, (u8, char)>,
    tree_groups: &mut HashMap<String, String>,
    trace: &mut Trace,
    uri: &str,
    access: u8,
    results: &mut HashMap<String, Right>,
    filter_value: &str,
    level: u8,
    db: &dyn Storage,
    out_f_is_exclusive: &mut bool,
    ignore_exclusive: bool,
) -> Result<bool, i64> {
    if level > 32 {
        //        if trace.is_info {
        //            print_to_trace_info(trace, format!("ERR! level down > 32,
        // uri={}\n", uri));        }
        return Ok(true);
    }

    match db.get(&(MEMBERSHIP_PREFIX.to_owned() + uri)) {
        Ok(groups_str) => {
            let groups_set: &mut Vec<Right> = &mut Vec::new();
            get_elements_from_index(&groups_str, groups_set);

            for (idx, group) in groups_set.iter_mut().enumerate() {
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

                let t_ignore_exclusive = if !ignore_exclusive && group.marker == M_IGNORE_EXCLUSIVE {
                    true
                } else {
                    ignore_exclusive
                };

                db.fiber_yield();

                match get_resource_groups(walked_groups, tree_groups, trace, &group.id, 15, results, filter_value, level + 1, db, out_f_is_exclusive, t_ignore_exclusive)
                {
                    Ok(_res) => {}
                    Err(e) => {
                        if e < 0 {
                            return Err(e);
                        }
                    }
                }

                if !ignore_exclusive && group.marker == M_IS_EXCLUSIVE {
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
                    }
                    None => {
                        new_group_marker = group.marker;
                    }
                }

                results.insert(
                    group.id.clone(),
                    Right {
                        id: group.id.clone(),
                        access: group.access,
                        marker: new_group_marker,
                        is_deleted: group.is_deleted,
                        level,
                    },
                );
            }
        }
        Err(e) => {
            if e < 0 {
                eprintln!("ERR! Authorize: get_resource_groups {:?}", uri);
                return Err(e);
            } else {
                return Ok(false);
            }
        }
    }

    Ok(false)
}

pub(crate) fn print_to_trace_acl(trace: &mut Trace, text: String) {
    trace.acl.push_str(&text);
}

pub(crate) fn print_to_trace_group(trace: &mut Trace, text: String) {
    trace.group.push_str(&text);
}

pub fn print_to_trace_info(trace: &mut Trace, text: String) {
    trace.str_num += 1;
    trace.info.push_str(&(trace.str_num.to_string() + " " + &text));
}

pub(crate) fn get_path(mopc: &mut HashMap<String, String>, el: String) -> String {
    if mopc.contains_key(&el) {
        let parent = mopc[&el].clone();
        mopc.remove(&el);
        let prev = get_path(mopc, parent);

        prev + "->" + &el
    } else {
        "".to_owned()
    }
}

pub fn access_to_pretty_string(src: u8) -> String {
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

    res
}

pub(crate) fn final_check(azc: &mut AzContext, trace: &mut Trace) -> bool {
    let res = if azc.is_need_exclusive_az && azc.is_found_exclusive_az {
        true
    } else {
        !azc.is_need_exclusive_az
    };

    if trace.is_info && res {
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

    res
}
