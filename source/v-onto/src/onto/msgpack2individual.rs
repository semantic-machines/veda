extern crate rmp as msgpack;
use crate::datatype::*;
use crate::individual::*;
use crate::parser::*;
use crate::resource::*;
use msgpack::decode::*;
use msgpack::Marker;
use std::io::Cursor;

pub fn parse_msgpack(raw: &mut RawObj) -> Result<String, i8> {
    if raw.data.is_empty() || raw.raw_type != RawType::MSGPACK {
        return Err(-1);
    }

    let mut cur = Cursor::new(raw.data.as_slice());

    if let Ok(v) = read_marker(&mut cur) {
        if let Marker::FixArray(size) = v {
            if size != 2 {
                return Err(-1);
            }
        } else {
            return Err(-1);
        }
    } else {
        return Err(-1);
    }

    // read individual URI
    let uri = match read_string_from_msgpack(&mut cur) {
        Ok(p) => p,
        Err(_) => return Err(-1),
    };

    if let Ok(size) = read_map_len(&mut cur) {
        raw.len_predicates = size as u32;
        raw.cur = cur.position();
        Ok(uri)
    } else {
        Err(-1)
    }
}

pub fn parse_msgpack_to_predicate(expect_predicate: &str, iraw: &mut Individual) -> Result<(), String> {
    if iraw.raw.cur >= iraw.raw.data.len() as u64 {
        return Err("fail position of cursor".to_owned());
    }

    let mut is_found = false;
    let mut cur = Cursor::new(iraw.raw.data.as_slice());
    cur.set_position(iraw.raw.cur);

    for i in iraw.raw.cur_predicates..iraw.raw.len_predicates {
        let predicate = match read_string_from_msgpack(&mut cur) {
            Ok(p) => p,
            Err(e) => {
                iraw.raw.cur = cur.position();
                if e == -1 {
                    return Err("fail read_string_from_msgpack".to_owned());
                } else {
                    return Err(String::default());
                }
            }
        };

        iraw.raw.cur_predicates = i;

        if predicate == expect_predicate {
            is_found = true;
        }

        match read_array_len(&mut cur) {
            Ok(size) => {
                for _i_values in 0..size {
                    match read_marker(&mut cur) {
                        Ok(v) => match v {
                            Marker::FixArray(size) => {
                                if size != 2 && size != 3 {
                                    return Err(format!("parsing values, unexpected array size, len={:?}", size));
                                }

                                let v_type: u8;
                                if let Ok(t) = read_int(&mut cur) {
                                    v_type = t;
                                } else {
                                    return Err("parsing type".to_owned());
                                }

                                if size == 2 {
                                    if v_type == DataType::Boolean as u8 {
                                        match read_bool(&mut cur) {
                                            Ok(res) => iraw.obj.add_bool(&predicate, res),
                                            Err(e) => return Err(format!("value: expected {}, err={:?}", v_type, e)),
                                        }
                                    } else if v_type == DataType::Datetime as u8 {
                                        match read_int(&mut cur) {
                                            Ok(res) => iraw.obj.add_datetime(&predicate, res),
                                            Err(e) => return Err(format!("value: expected {}, err={:?}", v_type, e)),
                                        }
                                    } else if v_type == DataType::Integer as u8 {
                                        match read_int(&mut cur) {
                                            Ok(res) => iraw.obj.add_integer(&predicate, res),
                                            Err(e) => return Err(format!("value: expected {}, err={:?}", v_type, e)),
                                        }
                                    } else if v_type == DataType::Uri as u8 {
                                        match read_string_from_msgpack(&mut cur) {
                                            Ok(res) => iraw.obj.add_uri(&predicate, &res),
                                            Err(e) => return Err(format!("value: expected {}, err={:?}", v_type, e)),
                                        }
                                    } else if v_type == DataType::Binary as u8 {
                                        let values = iraw.obj.resources.entry(predicate.to_owned()).or_default();
                                        if !read_raw_into_resources(&mut cur, values) {
                                            return Err("value: fail read raw".to_owned());
                                        }
                                    } else if v_type == DataType::String as u8 {
                                        match read_string_from_msgpack(&mut cur) {
                                            Ok(res) => iraw.obj.add_string(&predicate, &res, Lang::NONE),
                                            Err(e) => return Err(format!("value: expected {}, err={:?}", v_type, e)),
                                        };
                                    } else {
                                        return Err(format!("msgpack:unknown type {}", v_type));
                                    }
                                } else if size == 3 {
                                    if v_type == DataType::Decimal as u8 {
                                        match read_int(&mut cur) {
                                            Ok(mantissa) => match read_int(&mut cur) {
                                                Ok(exponent) => iraw.obj.add_decimal_d(&predicate, mantissa, exponent),
                                                Err(e) => {
                                                    return Err(format!("value: fail read exponent, err={:?}", e));
                                                }
                                            },
                                            Err(e) => return Err(format!("value: fail read mantissa, err={:?}", e)),
                                        }
                                    } else if v_type == DataType::String as u8 {
                                        match read_string_from_msgpack(&mut cur) {
                                            Ok(res) => {
                                                let lang;
                                                match read_int(&mut cur) {
                                                    Ok(res) => lang = Lang::new_from_i64(res),
                                                    Err(e) => {
                                                        return Err(format!("value: fail read lang, err={:?}", e));
                                                    }
                                                }

                                                iraw.obj.add_string(&predicate, &res, lang);
                                            }
                                            Err(e) => return Err(format!("value: expected {}, err={:?}", v_type, e)),
                                        }
                                    }
                                }
                            }
                            marker => return Err(format!("parsing values: unexpected marker={:?}", marker)),
                        },
                        Err(e) => return Err(format!("parsing values: err={:?}", e)),
                    }
                }
            }
            Err(e) => return Err(format!("parsing {:?}", e)),
        }

        if is_found {
            iraw.raw.cur = cur.position();
            return Ok(());
        }
    }

    iraw.raw.cur = cur.position();
    Ok(())
}

fn read_raw_into_resources(cur: &mut Cursor<&[u8]>, values: &mut Vec<Resource>) -> bool {
    let m_pos = cur.position();
    let size: u32;

    if let Ok(v) = read_marker(cur) {
        match v {
            Marker::FixStr(s) => {
                size = u32::from(s);
                cur.set_position(m_pos);
            }
            Marker::Str8 | Marker::Str16 | Marker::Str32 => {
                cur.set_position(m_pos);

                match read_str_len(cur) {
                    Ok(s) => size = s,
                    Err(e) => {
                        error!("fail read str len , err={:?}", e);
                        return false;
                    }
                }
                cur.set_position(m_pos);
            }
            _marker => return false,
        }
    } else {
        return false;
    }

    let mut out = vec![0u8; size as usize];

    match read_str(cur, &mut out) {
        Ok(v) => {
            values.push(Resource {
                rtype: DataType::Binary,
                order: 0,
                value: Value::Binary(v.as_bytes().to_vec()),
            });
            true
        }
        Err(e) => {
            if let DecodeStringError::InvalidUtf8(buf, _err) = e {
                values.push(Resource {
                    rtype: DataType::Binary,
                    order: 0,
                    value: Value::Binary(buf.to_vec()),
                });
                return true;
            }
            false
        }
    }
}

fn read_string_from_msgpack(cur: &mut Cursor<&[u8]>) -> Result<String, i64> {
    let m_pos = cur.position();
    let size: u32;

    if let Ok(v) = read_marker(cur) {
        match v {
            Marker::FixStr(s) => {
                size = u32::from(s);
                cur.set_position(m_pos);
            }
            Marker::Str8 | Marker::Str16 | Marker::Str32 => {
                cur.set_position(m_pos);
                match read_str_len(cur) {
                    Ok(s) => size = s,
                    Err(e) => {
                        error!("fail read str len , err={:?}", e);
                        return Err(-1);
                    }
                }
                cur.set_position(m_pos);
            }
            Marker::Null => {
                //cur.set_position(m_pos);
                return Ok("".to_string());
            }
            marker => {
                error!("marker={:?}", marker);
                return Err(-1);
            }
        }
    } else {
        return Err(-2);
    }

    let mut out = vec![0u8; size as usize];
    match read_str(cur, &mut out) {
        Ok(v) => Ok(v.to_string()),
        Err(e) => {
            if let DecodeStringError::InvalidUtf8(buf, _err) = e {
                let res = String::from_utf8_lossy(buf);
                return Ok(res.to_string());
            }
            error!("fail read str, err={:?}", e);
            Err(-1)
        }
    }
}
