use crate::{Right, RightSet};
pub const M_IS_EXCLUSIVE: char = 'X';
pub const M_IGNORE_EXCLUSIVE: char = 'N';

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
                    eprintln!("ERR! rights_from_string, fail parse, access is not hex digit {}", value);
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

            let mut rr = Right::new(key);

            if value.len() < 3 {
                decode_value_v1(value, &mut rr);
            } else {
                // format value, ver 2
            }

            drain(key, rr);
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

pub fn encode_rightset(new_rights: RightSet) -> String {
    let mut outbuff = String::new();

    for key in new_rights.keys() {
        if let Some(right) = new_rights.get(key) {
            if !right.is_deleted {
                outbuff.push_str(&right.id);
                outbuff.push(';');

                encode_value_v1(right, &mut outbuff);

                outbuff.push(';');
            }
        }
    }
    outbuff
}
