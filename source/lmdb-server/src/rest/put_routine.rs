extern crate cbor;
extern crate rustc_serialize;

use std;
use std::collections::HashMap;
use std::ffi::{ CStr };
use std::io::{ Write, stderr, Cursor };
use std::os::raw::c_char;
use std::ptr::null_mut;
use rmp_bind:: { decode, encode };

const MAX_VECTOR_SIZE: usize = 150;

#[derive(PartialEq, Eq, Copy)]
enum ResourceType {
    Uri = 1,
    Str = 2,
    Integer = 4,
    Datetime = 8,
    Decimal = 32,
    Boolean = 64
}

#[derive(PartialEq, Eq, Copy)]
enum Lang {
    LangNone = 0,
    LangRu  = 1,
    LangEn   = 2
}

#[derive(PartialEq, Eq)]
pub struct Resource {
    res_type: ResourceType,
    lang: Lang,
    pub str_data: Vec<u8>,
    bool_data: bool,
    pub long_data: i64,
    decimal_mantissa_data: i64,
    decimal_exponent_data: i64,
}

impl Clone for ResourceType {
    fn clone(&self) -> ResourceType {
        *self
    }
}

impl Clone for Lang {
    fn clone(&self) -> Lang {
        *self
    }
}

///Individual struct representation
pub struct Individual {
    ///Individual uri in veda
    pub uri: Vec<u8>,
    ///Individual resources
    pub resources: HashMap<String, Vec<Resource>>    
}

impl Resource {
    pub fn new() -> Resource {
        return Resource { res_type: ResourceType::Uri, lang: Lang::LangNone, str_data: Vec::default(),
            bool_data: false, long_data: 0, decimal_mantissa_data: 0, decimal_exponent_data: 0};
    }

    fn clone(&self) -> Resource {
        return Resource { res_type: self.res_type, lang: self.lang, str_data: self.str_data.clone(),
            bool_data: self.bool_data, long_data: self.long_data, 
            decimal_mantissa_data: self.decimal_mantissa_data, decimal_exponent_data: 
            self.decimal_exponent_data};
    }
}

impl Individual {
    pub fn new() -> Individual {
        return Individual { uri: Vec::default(), resources: HashMap::new() };
    }
}

impl Lang {
    fn from_u64(val: u64) -> Lang {
        match val {
            1 => Lang::LangRu,
            2 => Lang::LangEn,
            _ => Lang::LangNone
        }
    } 
}


pub fn msgpack_to_individual(cursor: &mut Cursor<&[u8]>, individual: &mut Individual) -> Result<(), String> {
    let arr_size: u64;
    /// Decodes main array
    match decode::decode_array(cursor) {
        Err(err) => return Err(format!("@ERR DECODING INDIVIDUAL MSGPACK ARRAY {:?}", err)),
        Ok(size) => arr_size = size
    }

    if arr_size != 2 {
        /// Array at least must have len 2, uri and map     
        return Err("@ERR INVALID INDIVIDUAL MSGPACK SIZE".to_string());    
    }

    /// Decodes individual uri
    match decode::decode_string(cursor, &mut individual.uri) {
        Err(err) => return Err(format!("@ERR DECODING INDIVIDUAL URI {:?}", err)),
        Ok(_) => {}
    }

    let map_size: u64;
    /// Decodes map with resources
    match decode::decode_map(cursor) {
        Err(err) => return Err(format!("@ERR DECODING INDIVIDUAL MAP {:?}", err)),
        Ok(size) => map_size = size
    }

    /// For each pair in map performs convertion to resource
    for _ in 0..map_size {
        let mut key: Vec<u8> = Vec::default();
        let mut resources: Vec<Resource> = Vec::with_capacity(MAX_VECTOR_SIZE);

        /// Map key is resource
        match decode::decode_string(cursor, &mut key) {
            Err(err) => return Err(format!("@ERR DECODING RESOURCE URI {:?}", err)),
            Ok(_) => {}
        }
        
        let res_size: u64;
        /// Decodes resource's array
        match decode::decode_array(cursor) {
            Ok(rs) => res_size = rs,
            Err(err) => return Err(format!("@ERR DECODING RESOURCES ARRAY {:?}", err))
        }

        /// For each element in resource array checks it type
        for _ in 0.. res_size {
            let objtype: decode::Type;
            match decode::decode_type(cursor) {
                Ok(t) => objtype = t,
                Err(err) => return Err(format!("@ERR DECODING RESOURCE TYPE {:?}", err))
            }

            match objtype {

                /// Arrays can have len 2 or 3
                decode::Type::ArrayObj => {
                    let res_arr_size = decode::decode_array(cursor).unwrap();
                    let res_type: u64;
                    /// Frist element of oall array is resource tyoe
                    match decode::decode_uint(cursor) {
                        Ok(rt) => res_type = rt,
                        Err(err) => return Err(format!("@ERR DECODING RESOURCE TYPE {:?}", err))
                    }
                    if res_arr_size == 2 {
                        if res_type == ResourceType::Datetime as u64 {
                            /// Arrays with len 2 can be datetime, datetime can be int or uint in msgpack                            
                            let mut datetime: i64 = 0;
                            let decode_type: decode::Type;                   
                            match decode::decode_type(cursor) {
                                Ok(dt) => decode_type = dt,
                                Err(err) => return Err(format!("@ERR DECODING STRING RES TYPE {:?}", err))
                            }

                            match decode_type {
                                decode::Type::UintObj => {
                                    match decode::decode_uint(cursor) {
                                        Ok(dt) => datetime = dt as i64,
                                        Err(err) => return Err(format!("@ERR DECODING DATETIME {:?}", err))
                                    }
                                }

                                decode::Type::IntObj => {
                                    match decode::decode_int(cursor) {
                                        Ok(dt) => datetime = dt,
                                        Err(err) => return Err(format!("@ERR DECODING DATETIME {:?}", err))
                                    }
                                }

                                _ => {}
                            }
                            let mut resource = Resource::new();
                            resource.res_type = ResourceType::Datetime;
                            resource.long_data = datetime;
                            resources.push(resource);
                        } else if res_type == ResourceType::Str as u64 {
                            /// Arrays with len 2 can be str without language
                            let mut resource = Resource::new();
                            
                            let decode_type: decode::Type;
                            match decode::decode_type(cursor) {
                                Ok(dt) => decode_type = dt,
                                Err(err) => return Err(format!("@ERR DECODING STRING RES TYPE {:?}", err))
                            }

                            match decode_type {
                                decode::Type::StrObj => 
                                    decode::decode_string(cursor, &mut resource.str_data).unwrap(),
                                decode::Type::NilObj => decode::decode_nil(cursor).unwrap(),
                                _ => return Err("@UNKNOWN TYPE IN STRING RESOURCE".to_string())
                            }
                            resource.lang = Lang::LangNone;
                            resources.push(resource);
                        } else {
                            return Err("@UNKNOWN RESOURCE TYPE".to_string());
                        }
                    } else if res_arr_size == 3 {
                        if res_type == ResourceType::Decimal as u64 {
                            /// Arrays with len 3 can be decimal
                            /// Decimal contains two elements of int or uint type
                            /// Mantissa and exponent
                            let mut resource = Resource::new();
                            
                            let mut decode_type: decode::Type;  
                            match decode::decode_type(cursor) {
                                Ok(dt) => decode_type = dt,
                                Err(err) => return Err(format!("@ERR DECODEING MANTISSA TYPE {:?}", err))
                            }

                            match decode_type {
                                decode::Type::UintObj => {
                                    resource.decimal_mantissa_data = decode::decode_uint(cursor).unwrap() as i64;
                                },
                                decode::Type::IntObj => {
                                    resource.decimal_mantissa_data = decode::decode_int(cursor).unwrap();
                                },
                                _ => return Err("@ERR UNSUPPORTED MANTISSA TYPE".to_string())
                            }
  
                            match decode::decode_type(cursor) {
                                Ok(dt) => decode_type = dt,
                                Err(err) => return Err(format!("@ERR DECODEING MANTISSA TYPE {:?}", err))
                            }

                            match decode_type {
                                decode::Type::UintObj => {
                                    resource.decimal_exponent_data = decode::decode_uint(cursor).unwrap() as i64;
                                },
                                decode::Type::IntObj => {
                                    resource.decimal_exponent_data = decode::decode_int(cursor).unwrap();
                                },
                                _ => return Err("@ERR UNSUPPORTED EXPONENT TYPE".to_string())
                            }

                            resource.res_type = ResourceType::Decimal;
                            resources.push(resource);
                        } else if res_type == ResourceType::Str as u64 {
                            /// Arrays with lan 3 can be str with languate
                            let mut resource = Resource::new();

                            let decode_type: decode::Type;
                            match decode::decode_type(cursor) {
                                Ok(dt) => decode_type = dt,
                                Err(err) => return Err(format!("@ERR DECODING STRING RES TYPE {:?}", err))
                            }

                            match decode_type {
                                decode::Type::StrObj => 
                                    decode::decode_string(cursor, &mut resource.str_data).unwrap(),
                                decode::Type::NilObj => decode::decode_nil(cursor).unwrap(),
                                _ => return Err("@UNKNOWN TYPE IN STRING RESOURCE".to_string())
                            }

                            match decode::decode_uint(cursor) {
                                Ok(l) => resource.lang = Lang::from_u64(l),
                                Err(err) => return Err(format!("@ERR DECODING LEN {:?}", err))
                            }
                            resource.res_type = ResourceType::Str;
                            resources.push(resource);
                        }                     
                    }
                }

                decode::Type::StrObj => {
                    let mut resource = Resource::new();
                    decode::decode_string(cursor, &mut resource.str_data).unwrap();
                    resource.res_type = ResourceType::Uri;
                    resources.push(resource);
                }
                decode::Type::UintObj => {
                    let mut resource = Resource::new();
                    resource.long_data = decode::decode_uint(cursor).unwrap() as i64;
                    resource.res_type = ResourceType::Integer;
                    resources.push(resource);
                }
                decode::Type::IntObj => {
                    let mut resource = Resource::new();
                    resource.long_data = decode::decode_int(cursor).unwrap();
                    resource.res_type = ResourceType::Integer;
                    resources.push(resource);
                }
                decode::Type::BoolObj => {
                    let mut resource = Resource::new();
                    resource.bool_data = decode::decode_bool(cursor).unwrap();
                    resource.res_type = ResourceType::Boolean;
                    resources.push(resource);
                }
               _ => return Err(format!("@UNSUPPORTED RESOURCE TYPE {0} :{1}", objtype as u64, 
                std::str::from_utf8(&key[..]).unwrap()))
            }
        }

        individual.resources.insert(std::str::from_utf8(key.as_ref()).unwrap().to_string(), resources);
    }
    return Ok(());
}