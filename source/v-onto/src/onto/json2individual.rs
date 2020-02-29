use crate::datatype::{DataType, Lang};
use crate::individual::Individual;
use serde_json::value::Value as JSONValue;
use serde_json::Map;

pub fn parse_json_to_individual(src: &JSONValue, dest: &mut Individual) -> bool {
    let mut res = true;

    if let Some(props) = src.as_object() {
        for (key, value) in props.iter() {
            if key == "@" {
                if let Some(id) = value.as_str() {
                    dest.set_id(id);
                } else {
                    error!("json->individual: fail get id");
                    res = false;
                }
            } else if let Some(values) = value.as_array() {
                if !json_to_predicate(key, values, dest) {
                    res = false;
                }
            } else {
                error!("json->individual: predicate [{}] must contain an array of values", key);
                res = false;
            }
        }
    }

    res
}

fn get_datatype_from_json(val: Option<&JSONValue>) -> Result<DataType, String> {
    if val.is_none() {
        return Err("not content field type".to_owned());
    }

    let tp = val.unwrap();
    if tp.is_string() {
        if let Some(v) = tp.as_str() {
            if let Some(d) = DataType::new_from_str(v) {
                return Ok(d);
            }
        }
    } else if tp.is_u64() {
        if let Some(v) = tp.as_u64() {
            if let Some(d) = DataType::new_from_u64(v) {
                return Ok(d);
            }
        }
    } else {
        return Err("expected string or integer for value of field [type]".to_owned());
    }

    Err("invalid value of field [type]".to_owned())
}

fn add_string(v: &Map<String, JSONValue>, vdata: &JSONValue, predicate: &str, dest: &mut Individual) {
    if let Some(s) = vdata.as_str() {
        let lang = if let Some(v) = v.get("lang") {
            if v.is_string() {
                Lang::new_from_str(v.as_str().unwrap_or_default().to_lowercase().as_str())
            } else if v.is_number() {
                Lang::new_from_i64(v.as_i64().unwrap_or_default())
            } else {
                Lang::NONE
            }
        } else {
            Lang::NONE
        };

        dest.add_string(predicate, s, lang);
    }
}

fn json_to_predicate(predicate: &str, values: &[JSONValue], dest: &mut Individual) -> bool {
    let mut res = true;
    for val in values {
        if let Some(v) = val.as_object() {
            let vdata = v.get("data");
            if vdata.is_none() {
                error!("json->individual: predicate [{}], value must contain [data]", predicate);
                res = false;
                continue;
            }
            let ptype = get_datatype_from_json(v.get("type"));
            if ptype.is_err() {
                error!("json->individual: predicate [{}], invalid value", predicate);
                res = false;
                continue;
            }
            let vdata = vdata.unwrap();

            match ptype.unwrap() {
                DataType::Uri => {
                    if let Some(v) = vdata.as_str() {
                        dest.add_uri(predicate, v);
                    }
                }
                DataType::String => add_string(v, vdata, predicate, dest),

                DataType::Integer => {
                    if let Some(v) = vdata.as_i64() {
                        dest.add_integer(predicate, v);
                    }
                }
                DataType::Datetime => {
                    if vdata.is_number() {
                        if let Some(v) = vdata.as_i64() {
                            dest.add_datetime(predicate, v);
                        }
                    } else if vdata.is_string() {
                        if let Some(v) = vdata.as_str() {
                            dest.add_datetime_from_str(predicate, v);
                        }
                    }
                }
                DataType::Decimal => {
                    if vdata.is_f64() {
                        if let Some(v) = vdata.as_f64() {
                            dest.add_decimal_from_f64(predicate, v);
                        }
                    } else if vdata.is_number() {
                        if let Some(v) = vdata.as_i64() {
                            dest.add_decimal_from_i64(predicate, v);
                        }
                    } else if vdata.is_string() {
                        if let Some(v) = vdata.as_str() {
                            dest.add_decimal_from_str(predicate, v);
                        }
                    }
                }
                DataType::Boolean => {
                    if let Some(v) = vdata.as_bool() {
                        dest.add_bool(predicate, v);
                    }
                }
                DataType::Binary => {
                    if let Some(v) = vdata.as_str() {
                        dest.add_binary(predicate, v.as_bytes().to_vec());
                    }
                }
            }
        } else {
            error!("json->individual: value for predicate [{}] must contain map", predicate);
            res = false;
        }
    }

    res
}
