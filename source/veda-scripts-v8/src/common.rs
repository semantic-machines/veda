use chrono::{TimeZone, Utc};
use rust_decimal::prelude::ToPrimitive;
use rust_decimal::Decimal;
use rusty_v8_m as v8;
use std::collections::HashSet;
use std::fs::DirEntry;
use std::path::Path;
use std::sync::Mutex;
use std::{fs, io};
use v_onto::datatype::Lang;
use v_onto::individual::Individual;
use v_onto::resource::Value;
use v_search::common::QueryResult;

pub struct HashVec<String> {
    pub hash: HashSet<String>,
    pub vec: Vec<String>,
}

impl Default for HashVec<String> {
    fn default() -> Self {
        Self {
            hash: Default::default(),
            vec: vec![],
        }
    }
}

impl HashVec<String> {
    pub(crate) fn new(src: Vec<String>) -> Self {
        Self {
            hash: src.iter().cloned().collect(),
            vec: src,
        }
    }
}

lazy_static! {
    pub static ref SYS_TICKET: Mutex<String> = Mutex::new("?".to_owned());
}

pub fn str_2_v8<'sc>(scope: &mut impl v8::ToLocal<'sc>, s: &str) -> v8::Local<'sc, v8::String> {
    v8::String::new(scope, s).unwrap()
}

pub fn v8_2_str<'sc>(scope: &mut impl v8::ToLocal<'sc>, s: &v8::Local<'sc, v8::Value>) -> String {
    s.to_string(scope).unwrap().to_rust_string_lossy(scope)
}

pub fn v8obj2individual<'a>(scope: &mut impl v8::ToLocal<'a>, context: &v8::Local<v8::Context>, v8_obj: v8::Local<'a, v8::Object>) -> Individual {
    let mut res = Individual::default();

    let data_key = str_2_v8(scope, "data");
    let type_key = str_2_v8(scope, "type");
    let lang_key = str_2_v8(scope, "lang");

    let j_predicates = v8_obj.get_property_names(scope, *context);

    for idx in 0..j_predicates.length() {
        let j_idx = v8::Integer::new(scope, idx as i32);
        let key = j_predicates.get(scope, *context, j_idx.into()).unwrap();
        let predicate = v8_2_str(scope, &key);
        let val = v8_obj.get(scope, *context, key).unwrap();

        if predicate == "@" {
            res.set_id(&v8_2_str(scope, &val));
        } else {
            if let Some(resources) = val.to_object(scope) {
                if !resources.is_array() {
                    add_v8_value_obj_to_individual(scope, context, &predicate, resources, &mut res, data_key, type_key, lang_key);
                } else {
                    let key_list = resources.get_property_names(scope, *context);

                    for resources_idx in 0..key_list.length() {
                        let j_resources_idx = v8::Integer::new(scope, resources_idx as i32);
                        if let Some(v) = resources.get(scope, *context, j_resources_idx.into()) {
                            if let Some(resource) = v.to_object(scope) {
                                if resource.is_array() {
                                    let idx_0 = v8::Integer::new(scope, 0);
                                    if let Some(v) = resource.get(scope, *context, idx_0.into()) {
                                        if let Some(resource) = v.to_object(scope) {
                                            add_v8_value_obj_to_individual(scope, context, &predicate, resource, &mut res, data_key, type_key, lang_key);
                                        }
                                    }
                                } else {
                                    add_v8_value_obj_to_individual(scope, context, &predicate, resource, &mut res, data_key, type_key, lang_key);
                                }
                            } else {
                                error!("v8obj2individual: invalid value predicate[{}], idx={}", predicate, resources_idx);
                            }
                        }
                    }
                }
            }
        }

        debug!("res={}", res.to_string());
    }

    res
}

fn add_v8_value_obj_to_individual<'a>(
    scope: &mut impl v8::ToLocal<'a>,
    context: &v8::Local<v8::Context>,
    predicate: &str,
    resource: v8::Local<v8::Object>,
    res: &mut Individual,
    data_key: v8::Local<v8::String>,
    type_key: v8::Local<v8::String>,
    lang_key: v8::Local<v8::String>,
) {
    let vdata = resource.get(scope, *context, data_key.into()).unwrap();
    let vtype = resource.get(scope, *context, type_key.into()).unwrap();
    if !vtype.is_string() {
        return;
    }

    if let Some(stype) = vtype.to_string(scope) {
        let stype = stype.to_rust_string_lossy(scope);
        match stype.as_str() {
            "Decimal" => {
                if vdata.is_number() && !vdata.is_int32() && !vdata.is_big_int() {
                    if let Some(v) = vdata.to_number(scope) {
                        res.add_decimal_from_f64(&predicate, v.value());
                    }
                } else if vdata.is_int32() || vdata.is_big_int() {
                    if let Some(v) = vdata.to_integer(scope) {
                        res.add_decimal_from_i64(&predicate, v.value());
                    }
                } else if vdata.is_string() {
                    if let Some(v) = vdata.to_string(scope) {
                        res.add_decimal_from_str(&predicate, &v.to_rust_string_lossy(scope));
                    }
                }
            }
            "Integer" => {
                if vdata.is_number() {
                    res.add_integer(&predicate, vdata.integer_value(scope).unwrap());
                }
            }
            "Datetime" => {
                if vdata.is_number() {
                    if let Some(v) = vdata.to_integer(scope) {
                        res.add_datetime(&predicate, v.value());
                    }
                } else if vdata.is_string() {
                    res.add_datetime_from_str(&predicate, &v8_2_str(scope, &vdata));
                } else if vdata.is_date() {
                    if let Some(v) = vdata.to_integer(scope) {
                        res.add_datetime(&predicate, v.value() / 1000);
                    }
                }
            }
            "Boolean" => {
                if vdata.is_boolean() {
                    res.add_bool(&predicate, vdata.to_integer(scope).unwrap().value() != 0);
                }
            }
            "String" => {
                let lang = if let Some(vlang) = resource.get(scope, *context, lang_key.into()) {
                    Lang::new_from_str(&v8_2_str(scope, &vlang).to_lowercase())
                } else {
                    Lang::NONE
                };

                let sdata = v8_2_str(scope, &vdata);
                res.add_string(&predicate, &sdata, lang);
            }
            "Uri" => {
                let sdata = v8_2_str(scope, &vdata);
                res.add_uri(&predicate, &sdata);
            }
            _ => {
                error!("v8obj2individual: unknown type = {}, predicate[{}]", stype, predicate);
            }
        }
    } else {
        error!("v8obj2individual: type is not string");
    }
}

pub fn query_result2v8obj<'a>(scope: &mut impl v8::ToLocal<'a>, src: &QueryResult) -> v8::Local<'a, v8::Object> {
    let context = scope.get_current_context().unwrap();
    let v8_obj = v8::Object::new(scope);

    v8_obj.set(context, str_2_v8(scope, "count").into(), v8::Integer::new(scope, src.count as i32).into());

    let js_resources = v8::Array::new(scope, src.result.len() as i32);
    let mut idx = 0;
    for el in src.result.iter() {
        js_resources.set(context, v8::Integer::new(scope, idx).into(), str_2_v8(scope, el).into());
        idx += 1;
    }

    v8_obj.set(context, str_2_v8(scope, "result").into(), js_resources.into());
    v8_obj.set(context, str_2_v8(scope, "estimated").into(), v8::Integer::new(scope, src.estimated as i32).into());
    v8_obj.set(context, str_2_v8(scope, "processed").into(), v8::Integer::new(scope, src.processed as i32).into());
    v8_obj.set(context, str_2_v8(scope, "cursor").into(), v8::Integer::new(scope, src.cursor as i32).into());
    v8_obj.set(context, str_2_v8(scope, "total_time").into(), v8::Integer::new(scope, src.total_time as i32).into());
    v8_obj.set(context, str_2_v8(scope, "query_time").into(), v8::Integer::new(scope, src.query_time as i32).into());
    v8_obj.set(context, str_2_v8(scope, "authorize_time").into(), v8::Integer::new(scope, src.authorize_time as i32).into());
    v8_obj.set(context, str_2_v8(scope, "result_code").into(), v8::Integer::new(scope, src.result_code as i32).into());
    v8_obj
}

pub fn individual2v8obj<'a>(scope: &mut impl v8::ToLocal<'a>, src: &mut Individual) -> v8::Local<'a, v8::Object> {
    let context = scope.get_current_context().unwrap();
    let v8_obj = v8::Object::new(scope);

    v8_obj.set(context, str_2_v8(scope, "@").into(), str_2_v8(scope, src.get_id()).into());

    let map_resources = src.get_obj().get_resources();
    for (predicate, resources) in map_resources {
        let js_resources = v8::Array::new(scope, resources.len() as i32);

        let mut idx = 0;
        for resource in resources {
            let v8_value = v8::Object::new(scope);

            js_resources.set(context, v8::Integer::new(scope, idx).into(), v8_value.into());
            idx += 1;

            match &resource.value {
                Value::Num(m, e) => {
                    let scale = if *e < 0 {
                        (*e * -1) as u32
                    } else {
                        0
                    };

                    let num = if *e > 0 {
                        *m * 10_i64.pow(*e as u32)
                    } else {
                        *m
                    };

                    let d = Decimal::new(num, scale);

                    v8_value.set(context, str_2_v8(scope, "data").into(), v8::Number::new(scope, d.to_f64().unwrap_or_default()).into());
                    v8_value.set(context, str_2_v8(scope, "type").into(), str_2_v8(scope, "Decimal").into());
                }
                Value::Int(i) => {
                    if *i < i32::max as i64 {
                        v8_value.set(context, str_2_v8(scope, "data").into(), v8::Integer::new(scope, *i as i32).into());
                    } else {
                        v8_value.set(context, str_2_v8(scope, "data").into(), v8::Number::new(scope, *i as f64).into());
                        //                        error!("individual2v8obj: predicate{}, {} > i32.max", predicate, i);
                    }
                    v8_value.set(context, str_2_v8(scope, "type").into(), str_2_v8(scope, "Integer").into());
                }
                Value::Datetime(i) => {
                    let dt = *i;
                    v8_value.set(context, str_2_v8(scope, "type").into(), str_2_v8(scope, "Datetime").into());
                    v8_value.set(context, str_2_v8(scope, "data").into(), str_2_v8(scope, &format!("{:?}", &Utc.timestamp(dt, 0))).into());
                }
                Value::Bool(b) => {
                    v8_value.set(context, str_2_v8(scope, "data").into(), v8::Boolean::new(scope, *b).into());
                    v8_value.set(context, str_2_v8(scope, "type").into(), str_2_v8(scope, "Boolean").into());
                }
                Value::Str(s, l) => {
                    if *l != Lang::NONE {
                        v8_value.set(context, str_2_v8(scope, "lang").into(), str_2_v8(scope, &l.to_string().to_uppercase()).into());

                        v8_value.set(context, str_2_v8(scope, "data").into(), str_2_v8(scope, s).into());
                        v8_value.set(context, str_2_v8(scope, "type").into(), str_2_v8(scope, "String").into());
                    }
                }
                Value::Uri(s) => {
                    v8_value.set(context, str_2_v8(scope, "type").into(), str_2_v8(scope, "Uri").into());
                    v8_value.set(context, str_2_v8(scope, "data").into(), str_2_v8(scope, s).into());
                }
                _ => {}
            }
        }

        v8_obj.set(context, str_2_v8(scope, predicate).into(), js_resources.into());
    }

    v8_obj
}

fn visit_dirs<T>(in_path: &Path, res: &mut Vec<T>, cb: &dyn Fn(&DirEntry, &mut Vec<T>)) -> io::Result<()> {
    if in_path.is_dir() {
        for entry in fs::read_dir(in_path)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, res, cb)?;
            } else {
                cb(&entry, res);
            }
        }
    }
    Ok(())
}

pub(crate) fn collect_module_dirs(in_path: &str, res: &mut Vec<String>) {
    fn prepare_dir(d: &DirEntry, res: &mut Vec<String>) {
        let path = d.path().as_path().to_owned();
        if let Some(path_str) = path.as_os_str().to_str() {
            if path_str.contains("/server/") || path_str.contains("/common/") {
                res.push(path.to_str().unwrap().to_owned());
            }
        } else {
            return;
        }
    }
    visit_dirs(Path::new(&in_path), res, &prepare_dir).unwrap_or_default();
}

pub(crate) fn collect_js_files(in_path: &str, res: &mut Vec<String>) {
    fn prepare_dir(d: &DirEntry, res: &mut Vec<String>) {
        let path = d.path().as_path().to_owned();
        if let Some(ext) = path.extension() {
            if let Some(ext) = ext.to_str() {
                if ext != "js" {
                    return;
                }
            }
        } else {
            return;
        }
        res.push(path.to_str().unwrap().to_owned());
    }

    let path = Path::new(&in_path);

    if !path.is_dir() {
        if path.exists() {
            res.push(path.to_str().unwrap().to_owned());
        }
    } else {
        visit_dirs(Path::new(&in_path), res, &prepare_dir).unwrap_or_default();
    }
}
