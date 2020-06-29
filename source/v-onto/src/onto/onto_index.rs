use bincode::{deserialize_from, serialize_into, ErrorKind};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::fs::rename;
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter};
use std::path::Path;
use std::time::SystemTime;

const LOCAL_STORAGE_FILE_NAME: &str = "data/onto-index";

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct OntoIndex {
    pub data: HashMap<String, i64>,
}

impl OntoIndex {
    pub fn get_modified() -> Option<SystemTime> {
        if let Ok(m) = fs::metadata(LOCAL_STORAGE_FILE_NAME) {
            if let Ok(t) = m.modified() {
                return Some(t);
            }
        }
        None
    }

    pub fn load() -> Self {
        if let Ok(f) = File::open(LOCAL_STORAGE_FILE_NAME) {
            let mut r = BufReader::new(f);
            let res: Result<OntoIndex, _> = deserialize_from(&mut r);
            match res {
                Ok(d) => {
                    info!("load {} onto elements", d.data.keys().len());
                    return d;
                }
                Err(e) => error!("fail load local storage, err={}", e),
            }
        }
        OntoIndex {
            data: HashMap::new(),
        }
    }

    pub fn exists(&self) -> bool {
        Path::new(LOCAL_STORAGE_FILE_NAME).exists()
    }

    pub fn dump(&self) -> Result<(), Box<ErrorKind>> {
        let tmp_file_name = LOCAL_STORAGE_FILE_NAME.to_owned() + ".tmp";
        let file = OpenOptions::new().write(true).create(true).truncate(false).open(tmp_file_name.clone())?;
        serialize_into(&mut BufWriter::new(file), &self.data)?;
        rename(tmp_file_name, LOCAL_STORAGE_FILE_NAME)?;
        Ok(())
    }

    pub fn len(&self) -> usize {
        self.data.keys().len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.keys().len() > 0
    }

    pub fn remove(&mut self, key: &str) -> Result<(), Box<ErrorKind>> {
        self.data.remove(key);
        self.dump()
    }

    pub fn set(&mut self, key: &str, val: &i64) -> Result<(), Box<ErrorKind>> {
        self.data.insert(key.to_owned(), val.to_owned());
        self.dump()
    }
}
