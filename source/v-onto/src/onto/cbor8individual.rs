use crate::individual::{Individual, RawObj};
use crate::parser::*;
use cbor::types::Type;
use cbor::{Config, Decoder};
use std::io::Cursor;

pub fn parse_cbor_to_predicate(_expect_predicate: &str, raw: &mut RawObj, _indv: &mut Individual) -> bool {
    return false;
}

pub fn cbor2individual(raw: &mut RawObj, indv: &mut Individual) -> bool {
    if raw.data.len() == 0 {
        return false;
    }

    if raw.raw_type != RawType::CBOR {
        return false;
    }

    let input = Cursor::new(raw.data.as_slice());
    let mut d = Decoder::new(Config::default(), input);

    if let Ok(len) = d.object() {
        for _x in 0..len {
            if let Ok(type_info) = d.typeinfo() {
                if let Ok(predicate) = d._text(&type_info) {
                    info!("@K {:?}", &predicate);

                    if predicate == "@" {
                        indv.uri = predicate;
                    } else {
                        if let Ok(type_info) = d.typeinfo() {
                            match type_info.0 {
                                Type::Text => {
                                    if let Ok(t) = d._text(&type_info) {
                                        info!("@V {:?}", t);
                                    }
                                }
                                Type::UInt8 => {
                                    if let Ok(i) = d._u8(&type_info) {
                                        info!("@V {:?}", d._u8(&type_info));
                                        //indv.add_integer(&predicate, i as i64, 0);
                                    }
                                }
                                Type::UInt16 => info!("@V {:?}", d._u16(&type_info)),
                                Type::UInt32 => info!("@V {:?}", d._u32(&type_info)),
                                Type::Array => {
                                    let len = d._array(&type_info);
                                    info!("@V ARRAY {:?}", len);
                                }
                                _ => error!("unknown type {:?}", type_info.0),
                            }
                        }
                    }
                }
            }
        }
    }

    info!("@4");

    return false;
}
