use crate::cbor2individual::*;
use crate::individual::*;
use crate::msgpack2individual::*;

#[derive(PartialEq, Debug)]
pub enum RawType {
    CBOR,
    JSON,
    MSGPACK,
    UNKNOWN,
}

pub fn parse_to_predicate(expect_predicate: &str, iraw: &mut Individual) -> bool {
    if iraw.raw.raw_type == RawType::MSGPACK {
        if let Err(e) = parse_msgpack_to_predicate(expect_predicate, iraw) {
            if !e.is_empty() {
                error!("parse for [{}], err={}", expect_predicate, e);
            }
            return false;
        }
        return true;
    } else if iraw.raw.raw_type == RawType::CBOR {
        return parse_cbor_to_predicate(expect_predicate, iraw);
    }

    false
}

const MSGPACK_MAGIC_HEADER: u8 = 146;

pub fn parse_raw(iraw: &mut Individual) -> Result<(), i8> {
    if iraw.raw.data.is_empty() {
        return Err(-1);
    }

    let traw: &[u8] = iraw.raw.data.as_slice();

    if traw[0] == MSGPACK_MAGIC_HEADER {
        iraw.raw.raw_type = RawType::MSGPACK;
    } else {
        iraw.raw.raw_type = RawType::CBOR;
    }

    let res = if iraw.raw.raw_type == RawType::MSGPACK {
        parse_msgpack(&mut iraw.raw)
    } else if iraw.raw.raw_type == RawType::CBOR {
        parse_cbor(&mut iraw.raw)
    } else {
        Err(-1)
    };

    if let Ok(uri) = res {
        iraw.obj.uri = uri;
        return Ok(());
    }

    Err(-1)
}
