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
        return parse_msgpack_to_predicate(expect_predicate, iraw);
    } else if iraw.raw.raw_type == RawType::CBOR {
        return parse_cbor_to_predicate(expect_predicate, iraw);
    }

    false
}

const MSGPACK_MAGIC_HEADER: u8 = 146;

pub fn parse_raw(iraw: &mut Individual) -> Result<String, i8> {
    if iraw.raw.data.is_empty() {
        return Err(-1);
    }

    let traw: &[u8] = iraw.raw.data.as_slice();

    if traw[0] == MSGPACK_MAGIC_HEADER {
        iraw.raw.raw_type = RawType::MSGPACK;
    } else {
        iraw.raw.raw_type = RawType::CBOR;
    }

    if iraw.raw.raw_type == RawType::MSGPACK {
        return parse_msgpack(&mut iraw.raw);
    } else if iraw.raw.raw_type == RawType::CBOR {
        return parse_cbor(&mut iraw.raw);
    }

    Err(-1)
}
