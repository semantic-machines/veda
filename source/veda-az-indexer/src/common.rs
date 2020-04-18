use v_authorization::common::{Access};
use v_authorization::formats::{decode_rec_to_rightset, encode_rightset, M_IGNORE_EXCLUSIVE, M_IS_EXCLUSIVE, update_counters};
use v_authorization::{Right, RightSet};
use v_onto::individual::Individual;
use v_storage::storage::{StorageId, VStorage};
use std::collections::HashMap;

pub struct Context {
    pub permission_statement_counter: u32,
    pub membership_counter: u32,
    pub storage: VStorage,
}

fn get_access_from_individual (state: &mut Individual, default_access: u8) -> u8 {
    let mut access= 0;

    if let Some(v) = state.get_first_bool("v-s:canCreate") {
        if v {
            access |= Access::CanCreate as u8;
        } else {
            access |= Access::CantCreate as u8;
        }
    }

    if let Some(v) = state.get_first_bool("v-s:canRead") {
        if v {
            access |= Access::CanRead as u8;
        } else {
            access |= Access::CantRead as u8;
        }
    }

    if let Some(v) = state.get_first_bool("v-s:canUpdate") {
        if v {
            access |= Access::CanUpdate as u8;
        } else {
            access |= Access::CantUpdate as u8;
        }
    }

    if let Some(v) = state.get_first_bool("v-s:canDelete") {
        if v {
            access |= Access::CanDelete as u8;
        } else {
            access |= Access::CantDelete as u8;
        }
    }

    if access == 0 {
        access = default_access;
    }

    access
}

pub fn prepare_right_set(prev_state: &mut Individual, new_state: &mut Individual, p_resource: &str, p_in_set: &str, prefix: &str, default_access: u8, ctx: &mut Context) {
    let new_access = get_access_from_individual (new_state, default_access);
    let prev_access = get_access_from_individual (prev_state, default_access);

    let is_deleted = new_state.get_first_bool("v-s:deleted").unwrap_or_default();

    let use_filter = new_state.get_first_literal("v-s:useFilter").unwrap_or_default();

    let resource = new_state.get_literals(p_resource).unwrap_or_default();
    let in_set = new_state.get_literals(p_in_set).unwrap_or_default();

    let prev_resource = prev_state.get_literals(p_resource).unwrap_or_default();
    let prev_in_set = prev_state.get_literals(p_in_set).unwrap_or_default();

    let removed_resource = get_disappeared(&prev_resource, &resource);
    let removed_in_set = get_disappeared(&prev_in_set, &in_set);

    let ignore_exclusive = new_state.get_first_bool("v-s:ignoreExclusive").unwrap_or_default();
    let is_exclusive = new_state.get_first_bool("v-s:isExclusive").unwrap_or_default();

    let marker = if is_exclusive {
        M_IS_EXCLUSIVE
    } else if ignore_exclusive {
        M_IGNORE_EXCLUSIVE
    } else {
        0 as char
    };

    if is_deleted && resource.is_empty() && in_set.is_empty(){
        update_right_set(new_state.get_id(), &prev_resource, &prev_in_set, marker, is_deleted, &use_filter, prefix, prev_access, new_access, ctx);
    } else {
        update_right_set(new_state.get_id(), &resource, &in_set, marker, is_deleted, &use_filter, prefix, prev_access, new_access, ctx);
    }

    if !removed_resource.is_empty() {
        update_right_set(new_state.get_id(), &removed_resource, &in_set, marker, true, &use_filter, prefix, prev_access, new_access, ctx);
    }

    if !removed_in_set.is_empty() {
        update_right_set(new_state.get_id(), &resource, &removed_in_set, marker, true, &use_filter, prefix, prev_access, new_access, ctx);
    }
}

pub fn update_right_set(
    source_id: &str,
    resources: &[String],
    in_set: &[String],
    marker: char,
    is_deleted: bool,
    filter: &str,
    prefix: &str,
    prev_access: u8,
    new_access: u8,
    ctx: &mut Context,
) {
    for rs in resources.iter() {
        let key = prefix.to_owned() + filter + rs;

        let mut new_right_set = RightSet::new();
        if let Some(prev_data_str) = ctx.storage.get_value(StorageId::Az, &key) {
            decode_rec_to_rightset(&prev_data_str, &mut new_right_set);
        }

        for in_set_id in in_set.iter() {
            if let Some(rr) = new_right_set.get_mut(in_set_id) {
                rr.is_deleted = is_deleted;
                rr.marker = marker;
                if is_deleted {
                    rr.access = update_counters(&mut rr.counters, prev_access, rr.access, is_deleted);
                    if rr.access != 0 {
                        rr.is_deleted = false;
                    }
                } else {
                    rr.access = update_counters(&mut rr.counters, prev_access, new_access, is_deleted);
                }
            } else {
                new_right_set.insert(
                    in_set_id.to_string(),
                    Right {
                        id: in_set_id.to_string(),
                        access: new_access,
                        marker,
                        is_deleted,
                        level: 0,
                        counters: HashMap::default()
                    },
                );
            }
        }

        let mut new_record = encode_rightset(new_right_set);

        if new_record.is_empty() {
            new_record = "X".to_string();
        }

        debug!("{} {} {:?}", source_id, rs, new_record);

        ctx.storage.put_kv(StorageId::Az, &key, &new_record);
    }
}

pub fn get_disappeared(a: &[String], b: &[String]) -> Vec<String> {
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

        if !is_found {
            delta.push(r_a);
        }
    }

    delta
}
