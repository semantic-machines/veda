use crate::common::*;
use crate::{Right, RightSet};
use std::collections::HashMap;

pub const M_IS_EXCLUSIVE: char = 'X';
pub const M_IGNORE_EXCLUSIVE: char = 'N';
static ACCESS_C_FULL_LIST: [char; 8] = ['M', 'R', 'U', 'P', 'm', 'r', 'u', 'p'];

fn access_from_char(c: char) -> Option<Access> {
    match c {
        'M' => Some(Access::CanCreate),
        'R' => Some(Access::CanRead),
        'U' => Some(Access::CanUpdate),
        'P' => Some(Access::CanDelete),
        'm' => Some(Access::CantCreate),
        'r' => Some(Access::CantRead),
        'u' => Some(Access::CantUpdate),
        'p' => Some(Access::CantDelete),
        _ => None,
    }
}

fn access8_from_char(c: char) -> Option<u8> {
    match c {
        'M' => Some(1),
        'R' => Some(2),
        'U' => Some(4),
        'P' => Some(8),
        'm' => Some(16),
        'r' => Some(32),
        'u' => Some(64),
        'p' => Some(128),
        _ => None,
    }
}

fn access8_to_char(a: u8) -> Option<char> {
    match a {
        1 => Some('M'),
        2 => Some('R'),
        4 => Some('U'),
        8 => Some('P'),
        16 => Some('m'),
        32 => Some('r'),
        64 => Some('u'),
        128 => Some('p'),
        _ => None,
    }
}

pub fn update_counters(counters: &mut HashMap<char, u16>, prev_access: u8, cur_access: u8, is_deleted: bool) -> u8 {
    let mut out_access = cur_access;

    for access_c in ACCESS_C_FULL_LIST.iter() {
        if let Some(check_bit) = access8_from_char(*access_c) {
            if let Some(cc) = counters.get_mut(access_c) {
                if out_access & check_bit > 0 {
                    if is_deleted {
                        if prev_access & check_bit > 0 {
                            *cc -= 1;
                            if *cc == 0 {
                                out_access &= !check_bit;
                            }
                        }
                    } else {
                        *cc += 1;
                        out_access |= check_bit;
                    }
                }
            } else {
                if !is_deleted && (out_access & check_bit > 0) {
                    out_access |= check_bit;
                    counters.insert(*access_c, 1);
                }
            }
        }
    }

    out_access
}

fn decode_value_v2(value: &str, rr: &mut Right, with_count: bool) {
    let mut access = 0;

    let mut tag: Option<char> = None;
    let mut val = String::new();
    for c in value.chars() {
        if c == 'M' || c == 'R' || c == 'U' || c == 'P' || c == 'm' || c == 'r' || c == 'u' || c == 'p' || c == M_IS_EXCLUSIVE || c == M_IGNORE_EXCLUSIVE {
            if c == M_IS_EXCLUSIVE || c == M_IGNORE_EXCLUSIVE {
                rr.marker = c;
            } else {
                if let Some(a) = access_from_char(c) {
                    access |= a as u8;
                }
            }

            if with_count {
                if let Some(t) = tag {
                    rr.counters.insert(t, val.parse::<u16>().unwrap_or(1));
                }
            }

            tag = Some(c);
            val = String::new();
        } else {
            val.push(c);
        }
    }

    if with_count {
        if let Some(t) = tag {
            rr.counters.insert(t, val.parse::<u16>().unwrap_or(1));
        }
    }

    rr.access = access as u8;
}

fn decode_value_v1(value: &str, rr: &mut Right, with_count: bool) {
    let mut access = 0;
    let mut marker = 0 as char;

    // format value, ver 1
    let mut shift = 0;
    for c in value.chars() {
        if c == M_IS_EXCLUSIVE || c == M_IGNORE_EXCLUSIVE {
            marker = c;
        } else {
            match c.to_digit(16) {
                Some(v) => access |= v << shift,
                None => {
                    eprintln!("ERR! decode_value_v1, fail parse, access is not hex digit {}", value);
                    continue;
                }
            }
            shift += 4;
        }
    }

    rr.access = access as u8;
    rr.marker = marker;

    if with_count {
        for a in ACCESS_8_FULL_LIST.iter() {
            if a & rr.access > 0 {
                if let Some(ac) = access8_to_char(*a) {
                    rr.counters.insert(ac, 1);
                }
            }
        }
    }
}

pub fn decode_index_record<F>(src: &str, with_counter: bool, mut drain: F) -> bool
where
    F: FnMut(&str, Right),
{
    if src.is_empty() {
        return false;
    }

    let tokens: Vec<&str> = src.split(';').collect();

    let mut idx = 0;
    loop {
        if idx + 1 < tokens.len() {
            let key = tokens[idx];
            let value = tokens[idx + 1];

            if !value.is_empty() {
                let mut rr = Right::new(key);

                if access_from_char(value.chars().next().unwrap()).is_none() {
                    decode_value_v1(value, &mut rr, with_counter);
                } else {
                    decode_value_v2(value, &mut rr, with_counter);
                }

                //println!("{} -> {}", value, access_to_pretty_string(rr.access));

                drain(key, rr);
            }
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

pub fn decode_rec_to_rights(src: &str, result: &mut Vec<Right>) -> bool {
    decode_index_record(src, false, |_key, right| {
        result.push(right);
    })
}

pub fn decode_rec_to_rightset(src: &str, new_rights: &mut RightSet) -> bool {
    decode_index_record(src, true, |key, right| {
        new_rights.insert(key.to_owned(), right);
    })
}

fn encode_value_v1(right: &Right, outbuff: &mut String) {
    outbuff.push_str(&format!("{:X}", right.access));

    if right.marker == M_IS_EXCLUSIVE || right.marker == M_IGNORE_EXCLUSIVE {
        outbuff.push(right.marker);
    }
}

fn encode_value_v2(right: &Right, outbuff: &mut String) {
    let mut set_access = 0;
    for (tag, count) in right.counters.iter() {
        if let Some(c) = access_from_char(*tag) {
            if *count > 0 {
                set_access |= c as u8;
                outbuff.push(*tag);
                if *count > 1 {
                    outbuff.push_str(&count.to_string());
                }
            }
        }
    }

    for a in ACCESS_8_FULL_LIST.iter() {
        if a & right.access & set_access == 0 {
            if let Some(c) = access8_to_char(a & right.access) {
                outbuff.push(c);
            }
        }
    }

    if right.marker == M_IS_EXCLUSIVE || right.marker == M_IGNORE_EXCLUSIVE {
        outbuff.push(right.marker);
    }

    //println!("{} -> {}", access_to_pretty_string(right.access), outbuff);
}

pub fn encode_rightset(new_rights: RightSet, version_of_index_format: u8) -> String {
    let mut outbuff = String::new();

    for key in new_rights.keys() {
        if let Some(right) = new_rights.get(key) {
            if !right.is_deleted {
                outbuff.push_str(&right.id);
                outbuff.push(';');

                let summ_counters: u16 = right.counters.values().sum();

                if summ_counters < 2 || version_of_index_format == 1 {
                    encode_value_v1(right, &mut outbuff);
                } else {
                    encode_value_v2(right, &mut outbuff);
                }

                outbuff.push(';');
            }
        }
    }
    outbuff
}
