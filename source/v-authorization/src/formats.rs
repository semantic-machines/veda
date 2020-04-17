use crate::common::*;
use crate::{Right, RightSet, TagCount};

pub const M_IS_EXCLUSIVE: char = 'X';
pub const M_IGNORE_EXCLUSIVE: char = 'N';

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

fn decode_value_v2(value: &str, rr: &mut Right, with_count: bool) {
    let mut access = 0;

    let mut tags_counters = vec![];
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
                    rr.counters.push(TagCount::new(t, &val));
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
            tags_counters.push(TagCount::new(t, &val));
        }

        println!("{:?}", tags_counters);
    }

    rr.access = access as u8;
}

fn decode_value_v1(value: &str, rr: &mut Right) {
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
}

pub fn decode_index_record<F>(src: &str, mut drain: F) -> bool
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
                    decode_value_v1(value, &mut rr);
                } else {
                    decode_value_v2(value, &mut rr, false);
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
    decode_index_record(src, |_key, right| {
        result.push(right);
    })
}

pub fn decode_rec_to_rightset(src: &str, new_rights: &mut RightSet) -> bool {
    decode_index_record(src, |key, right| {
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
    for t in right.counters.iter() {
        if let Some(c) = access_from_char(t.tag) {
            set_access = set_access | c as u8;
        }

        outbuff.push(t.tag);
        if t.count > 0 {
            outbuff.push_str(&t.count.to_string());
        }
    }

    for a in ACCESS_FULL_LIST.iter() {
        if a & right.access & set_access == 0 {
            if let Some(c) = access8_to_char(a & right.access) {
                outbuff.push(c);
            }
        }
    }

    if right.marker == M_IS_EXCLUSIVE || right.marker == M_IGNORE_EXCLUSIVE {
        outbuff.push(right.marker);
    }

    //println!("{} -> {}", access_to_pretty_string(right.access), outbuff)
}

pub fn encode_rightset(new_rights: RightSet) -> String {
    let mut outbuff = String::new();

    for key in new_rights.keys() {
        if let Some(right) = new_rights.get(key) {
            if !right.is_deleted {
                outbuff.push_str(&right.id);
                outbuff.push(';');

                if right.access == 0 {
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
