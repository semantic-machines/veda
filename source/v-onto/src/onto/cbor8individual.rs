use std::io::Cursor;
//use cbor::types::Type;
//use cbor::value::{self, Key};
use cbor::{Config, GenericDecoder};
//use crate::datatype::{DataType, Lang};
//use crate::resource::{Resource, Value};
use crate::individual::Individual;
use crate::parser::*;

pub fn parse_cbor_to_predicate(_expect_predicate: &str, _indv: &mut Individual) -> bool {
    return false;
}

pub fn cbor2individual(indv: &mut Individual) -> bool {
    if indv.raw.len() == 0 {
        return false;
    }

    if indv.raw_type != RawType::CBOR {
        return false;
    }

    info!("@1");
    let input = Cursor::new(indv.raw.as_slice());
    info!("@2");
    let mut d = GenericDecoder::new(Config::default(), input);
    info!("@3");

    if let Ok(aa) = d.value() {
        if let cbor::value::Value::Map(n) = aa {
            for el in n {
                info!("{:?}{:?}", el.0, el.1);
            }
        }
    }
    info!("@4");

    return false;
}
