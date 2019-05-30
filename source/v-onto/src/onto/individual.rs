use std::collections::HashMap;
use std::fmt;

use crate::msgpack8individual::parse_to_predicate;
use crate::resource::{Resource, Value};

pub struct Individual {
    pub binobj: Vec<u8>,
    pub uri: String,
    pub resources: HashMap<String, Vec<Resource>>,

    //
    pub cur: u64,
    pub len_predicates: u32,
    pub cur_predicates: u32,
}

impl fmt::Display for Individual {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "uri={}, \n {:#?}", self.uri, self.resources)
    }
}

impl Individual {
    pub fn new(buff: Vec<u8>) -> Individual {
        Individual {
            uri: "".to_string(),
            resources: HashMap::new(),
            binobj: buff,
            cur: 0,
            len_predicates: 0,
            cur_predicates: 0,
        }
    }

    pub fn new_empty() -> Individual {
        Individual {
            uri: "".to_string(),
            resources: HashMap::new(),
            binobj: Vec::new(),
            cur: 0,
            len_predicates: 0,
            cur_predicates: 0,
        }
    }

    pub fn get_predicates(&self) -> Vec<String> {
        let kk: Vec<String> = self.resources.iter().map(|(key, _)| key.clone()).collect();
        return kk;
    }

    pub fn get_first_literal(&mut self, predicate: &str) -> Result<String, i64> {
        match self.resources.get(predicate) {
            Some(v) => match &v[0].value {
                Value::Str(s, _l) => {
                    return Ok(s.to_string());
                }
                _ => return Err(-2),
            },
            None => {
                if self.cur < self.binobj.len() as u64 {
                    // next parse
                    if parse_to_predicate(predicate, self) == true {
                        return self.get_first_literal(predicate);
                    } else {
                        println!("@ERR -3");
                        return Err(-3);
                    }
                }
            }
        }

        return Ok("".to_string());
    }

    pub fn get_first_binobj(&mut self, predicate: &str) -> Vec<u8> {
        match self.resources.get(predicate) {
            Some(v) => match &v[0].value {
                Value::Binary(s) => {
                    return s.clone();
                }
                _ => {}
            },
            None => {
                if self.cur < self.binobj.len() as u64 {
                    // next parse
                    if parse_to_predicate(predicate, self) == true {
                        return self.get_first_binobj(predicate);
                    }
                }
            }
        }

        return "".as_bytes().to_vec();
    }

    fn _get_first_integer(&mut self, predicate: &str) -> Option<i64> {
        match self.resources.get(predicate) {
            Some(v) => match &v[0].value {
                Value::Int(i) => {
                    return Some(*i);
                }
                _ => {}
            },
            _ => {}
        }

        return None;
    }

    pub fn get_first_integer(&mut self, predicate: &str) -> i64 {
        if let Some(v) = self._get_first_integer(predicate) {
            return v;
        } else {
            if self.cur < self.binobj.len() as u64 {
                // next parse
                if parse_to_predicate(predicate, self) == true {
                    if let Some(v) = self._get_first_integer(predicate) {
                        return v;
                    }
                }
            }
        }

        return -1;
    }

    pub fn any_exists(&mut self, predicate: &str, values: Vec<String>) -> bool {
        match self.resources.get(predicate) {
            Some(v) => {
                for el in v {
                    match &el.value {
                        Value::Str(s, _l) => {
                            for ve in &values {
                                if *ve == *s {
                                    return true;
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            None => {
                if self.cur < self.binobj.len() as u64 {
                    // next parse
                    if parse_to_predicate(predicate, self) == true {
                        return self.any_exists(predicate, values);
                    } else {
                        return false;
                    }
                }
            }
        }
        return false;
    }
}

/*
    pub fn get_first_value(indv: &mut Individual, predicate: &str) -> Value {
        match indv.resources.get(predicate) {
            Some(v) => return v[0].value.clone (),
            None => {
                if indv.cur < indv.binobj.len () as u64 {
                    // next parse
                    if parse_to_predicate(predicate, indv) == true {
                        return get_first_value(indv, predicate);
                    }
                }
            },
        }

        return Value::Bool(false);
    }
*/
//}
