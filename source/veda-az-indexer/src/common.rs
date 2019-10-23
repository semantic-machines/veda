use std::collections::HashMap;
use v_onto::individual::Individual;
use v_storage::storage::{StorageId, VStorage};

pub struct Context {
    pub id: u32,
    pub storage: VStorage,
}

pub const PERMISSION_PREFIX: &str = "P";
pub const MEMBERSHIP_PREFIX: &str = "M";
pub const FILTER_PREFIX: &str = "F";
pub const M_IS_EXCLUSIVE: u8 = b'X';
pub const M_IGNORE_EXCLUSIVE: u8 = b'N';

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

pub struct Right {
    pub id: String,
    pub access: u8,
    pub marker: u8,
    pub is_deleted: bool,
}

pub type RightSet = HashMap<String, Right>;

pub fn prepare_right_set(prev_state: &mut Individual, new_state: &mut Individual, p_resource: &str, p_in_set: &str, prefix: &str, default_access: u8, ctx: &mut Context) {
    let mut access = 0u8;

    let is_deleted = new_state.get_first_bool("v-s:deleted").unwrap_or_default();

    if let Some(v) = new_state.get_first_bool("v-s:canCreate") {
        if v {
            access = access | Access::CanCreate as u8;
        } else {
            access = access | Access::CantCreate as u8;
        }
    }

    if let Some(v) = new_state.get_first_bool("v-s:canRead") {
        if v {
            access = access | Access::CanRead as u8;
        } else {
            access = access | Access::CantRead as u8;
        }
    }

    if let Some(v) = new_state.get_first_bool("v-s:canUpdate") {
        if v {
            access = access | Access::CanUpdate as u8;
        } else {
            access = access | Access::CantUpdate as u8;
        }
    }

    if let Some(v) = new_state.get_first_bool("v-s:canDelete") {
        if v {
            access = access | Access::CanDelete as u8;
        } else {
            access = access | Access::CantDelete as u8;
        }
    }

    if access == 0 {
        access = default_access;
    }

    let use_filter = new_state.get_first_literal("v-s:useFilter").unwrap_or_default();

    let resource = new_state.get_literals(p_resource).unwrap_or_default();
    let in_set = new_state.get_literals(p_in_set).unwrap_or_default();

    let prev_resource = prev_state.get_literals(p_resource).unwrap_or_default();
    let prev_in_set = prev_state.get_literals(p_in_set).unwrap_or_default();

    let removed_resource = get_disappeared(&prev_resource, &resource);
    let removed_in_set = get_disappeared(&prev_in_set, &in_set);

    let ignore_exclusive = new_state.get_first_bool("v-s:ignoreExclusive").unwrap_or_default();
    let is_exclusive = new_state.get_first_bool("v-s:isExclusive").unwrap_or_default();

    let marker = if is_exclusive == true {
        M_IS_EXCLUSIVE
    } else if ignore_exclusive == true {
        M_IGNORE_EXCLUSIVE
    } else {
        0
    };

    update_right_set(&resource, &in_set, marker, is_deleted, &use_filter, prefix, access, ctx);

    if removed_resource.len() > 0 {
        update_right_set(&removed_resource, &in_set, marker, true, &use_filter, prefix, access, ctx);
    }

    if removed_in_set.len() > 0 {
        update_right_set(&resource, &removed_in_set, marker, true, &use_filter, prefix, access, ctx);
    }
}

pub fn update_right_set(resources: &Vec<String>, in_set: &Vec<String>, marker: u8, is_deleted: bool, filter: &String, prefix: &str, access: u8, ctx: &mut Context) {
    for rs in resources.iter() {
        let key = prefix.to_owned() + &filter + rs;

        if let Some(prev_data_str) = ctx.storage.get_value(StorageId::Az, &key) {
            let mut new_right_set = RightSet::new();
            rights_from_string(&prev_data_str, &mut new_right_set);

            for mb in in_set.iter() {
                if let Some(rr) = new_right_set.get_mut(mb) {
                    rr.is_deleted = is_deleted;
                    rr.access = rr.access | access;
                    rr.marker = marker;
                } else {
                    new_right_set.insert(
                        mb.to_string(),
                        Right {
                            id: mb.to_string(),
                            access,
                            marker,
                            is_deleted,
                        },
                    );
                }
            }

            let mut new_record = rights_as_string(new_right_set);

            if new_record.len() == 0 {
                new_record = "X".to_string();
            }

            ctx.storage.put_kv(StorageId::Az, &key, &new_record);
        }
    }
}

pub fn get_disappeared(a: &Vec<String>, b: &Vec<String>) -> Vec<String> {
    let delta = Vec::new();

    for r_a in a.iter() {
        let mut delta = Vec::new();
        let mut is_found = false;
        for r_b in b.iter() {
            if r_a == r_b {
                is_found = true;
                break;
            }
        }

        if is_found == false {
            delta.push(r_a);
        }
    }

    return delta;
}

pub fn rights_from_string(src: &str, new_rights: &mut RightSet) -> bool {
    let tokens: Vec<&str> = src.split(";").collect();

    if tokens.len() <= 2 {
        return false;
    }

    let mut idx = 0;
    while idx < tokens.len() {
        if let Some(key) = tokens.get(idx) {
            if let Some(tmk) = tokens.get(idx + 1) {
                let mut marker = 0;
                if tmk.len() > 1 {
                    marker = tmk.as_bytes()[0];
                }

                if let Some(s_access) = tokens.get(idx + 1) {
                    if let Ok(access) = u8::from_str_radix(&s_access, 16) {
                        new_rights.insert(
                            key.to_string(),
                            Right {
                                id: key.to_string(),
                                access,
                                marker,
                                is_deleted: false,
                            },
                        );
                    }
                }
            }
        }
        idx += 2;
    }

    true
}

fn rights_as_string(new_rights: RightSet) -> String {
    let mut outbuff = String::new();

    for key in new_rights.keys() {
        if let Some(right) = new_rights.get(key) {
            if right.is_deleted == false {
                outbuff.push_str(&right.id);
                outbuff.push(';');
                outbuff.push_str(&format!("{:X}", right.access));

                if right.marker > 0 {
                    outbuff.push(right.marker as char);
                }
                outbuff.push(';');
            }
        }
    }
    outbuff
}
