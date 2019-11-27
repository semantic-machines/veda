use crate::datatype::Lang;
use crate::individual::Individual;
use serde_json::value::Value as JSONValue;

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
            } else {
                if let Some(values) = value.as_array() {
                    if !json_to_predicate(key, values, dest) {
                        res = false;
                    }
                } else {
                    error!("json->individual: predicate [{}] must contain an array of values", key);
                    res = false;
                }
            }
        }
    }

    res
}

fn json_to_predicate(predicate: &str, values: &Vec<JSONValue>, dest: &mut Individual) -> bool {
    let mut res = true;
    for val in values {
        if let Some(v) = val.as_object() {
            let vdata = v.get("data");
            if vdata.is_none() {
                error!("json->individual: predicate [{}], value must contain [data]", predicate);
                res = false;
                continue;
            }

            let vtype = v.get("type");
            if vtype.is_none() {
                error!("json->individual: predicate [{}], value must contain [type]", predicate);
                res = false;
                continue;
            }

            if let Some(vtype) = vtype.unwrap().as_str() {
                match vtype {
                    "Uri" => {
                        if let Some(v) = vdata.unwrap().as_str() {
                            dest.add_uri(predicate, v);
                        }
                    }
                    "String" => {
                        if let Some(s) = vdata.unwrap().as_str() {
                            let lang = if let Some(v) = v.get("lang") {
                                if v.is_string() {
                                    match v.as_str().unwrap().to_uppercase().as_str() {
                                        "RU" => Lang::RU,
                                        "EN" => Lang::EN,
                                        _ => Lang::NONE,
                                    }
                                } else if v.is_number() {
                                    match v.as_i64().unwrap() {
                                        0 => Lang::RU,
                                        1 => Lang::EN,
                                        _ => Lang::NONE,
                                    }
                                } else {
                                    Lang::NONE
                                }
                            } else {
                                Lang::NONE
                            };

                            dest.add_string(predicate, s, lang);
                        }
                    }
                    "Integer" => {
                        if let Some(v) = vdata.unwrap().as_i64() {
                            dest.add_integer(predicate, v);
                        }
                    }
                    "Datetime" => {
                        let vdata = vdata.unwrap();

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
                    "Decimal" => {
                        let vdata = vdata.unwrap();

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
                    "Boolean" => {
                        if let Some(v) = vdata.unwrap().as_bool() {
                            dest.add_bool(predicate, v);
                        }
                    }
                    "Binary" => {
                        if let Some(v) = vdata.unwrap().as_str() {
                            dest.add_binary(predicate, v.as_bytes().to_vec());
                        }
                    }
                    _ => {
                        error!("json->individual: predicate [{}], unknown type {}", predicate, vtype);
                        res = false;
                    }
                }
            } else {
                error!("json->individual: predicate [{}] expect type as string", predicate);
                res = false;
            }
        } else {
            error!("json->individual: value for predicate [{}] must contain map", predicate);
            res = false;
        }
    }

    res
}
