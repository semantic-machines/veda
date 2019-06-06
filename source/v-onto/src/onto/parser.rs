use crate::cbor8individual::*;
use crate::individual::Individual;
use crate::msgpack8individual::*;

#[derive(PartialEq, Debug)]
pub enum RawType {
    CBOR,
    JSON,
    MSGPACK,
    UNKNOWN,
}

pub fn parse_to_predicate(expect_predicate: &str, indv: &mut Individual) -> bool {
    if indv.raw_type == RawType::MSGPACK {
        return parse_msgpack_to_predicate(expect_predicate, indv);
    } else if indv.raw_type == RawType::CBOR {
        return parse_cbor_to_predicate(expect_predicate, indv);
    }

    return false;
}

const MSGPACK_MAGIC_HEADER: u8 = 146;

pub fn raw2individual(indv: &mut Individual) -> bool {
    let raw: &[u8] = indv.raw.as_slice();

    if raw[0] == MSGPACK_MAGIC_HEADER {
        indv.raw_type = RawType::MSGPACK;
    } else {
        indv.raw_type = RawType::CBOR;
    }

    if indv.raw_type == RawType::MSGPACK {
        return msgpack2individual(indv);
    } else if indv.raw_type == RawType::CBOR {
        return cbor2individual(indv);
    }

    return false;
}
