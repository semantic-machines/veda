use crc32fast::Hasher;
use std::collections::HashMap;
use std::fs;
use std::fs::OpenOptions;
use std::io::{BufRead, BufReader, Seek, SeekFrom, Write};
use std::time::SystemTime;
use xapian_rusty::XError;

pub const XAPIAN_INFO_PATH: &str = "./data/xapian-info";

pub struct Key2Slot {
    data: HashMap<String, u32>,
    last_size_key2slot: usize,
    modified: SystemTime,
}

impl Default for Key2Slot {
    fn default() -> Self {
        Key2Slot {
            data: Default::default(),
            last_size_key2slot: 0,
            modified: SystemTime::now(),
        }
    }
}

impl Key2Slot {
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn get_slot(&self, key: &str) -> Option<u32> {
        if key.is_empty() {
            return None;
        }

        if let Some(c) = key.chars().next() {
            if c == '#' {
                if let Ok(v) = key[1..].parse::<u32>() {
                    return Some(v);
                } else {
                    error!("invalid slot: {}", key);
                    return None;
                }
            }
        }

        if let Some(slot) = self.data.get(key) {
            Some(slot.to_owned())
        } else {
            debug!("key2slot, slot not found, key={}", key);
            None
        }
    }

    pub fn get_slot_and_set_if_not_found(&mut self, field: &str) -> u32 {
        if let Some(slot) = self.get_slot(field) {
            return slot;
        }

        // create new slot
        let slot = (self.data.len() + 1) as u32;
        self.data.insert(field.to_owned(), slot);
        if let Err(e) = self.store() {
            error!("fail store key2slot, err={:?}", e);
        } else {
            info!("create new slot {}={}", field, slot);
        }
        slot
    }

    pub fn is_need_reload(&mut self) -> Result<bool, XError> {
        let fname = XAPIAN_INFO_PATH.to_owned() + "/key2slot";
        let cur_modified = fs::metadata(fname)?.modified()?;
        Ok(cur_modified != self.modified)
    }

    pub fn load() -> Result<Key2Slot, XError> {
        let fname = XAPIAN_INFO_PATH.to_owned() + "/key2slot";
        let mut ff = OpenOptions::new().read(true).open(fname)?;
        ff.seek(SeekFrom::Start(0))?;

        let mut key2slot = Key2Slot::default();
        key2slot.modified = ff.metadata()?.modified()?;

        for line in BufReader::new(ff).lines() {
            if let Ok(ll) = line {
                let (field, slot) = scan_fmt!(&ll, "\"{}\",{}", String, u32);

                if field.is_some() && slot.is_some() {
                    key2slot.data.insert(field.unwrap(), slot.unwrap());
                } else {
                    error!("fail parse key2slot, line={}", ll);
                }
            }
        }

        Ok(key2slot)
    }

    pub fn store(&mut self) -> Result<(), XError> {
        let (data, hash) = self.serialize();

        if data.len() == self.last_size_key2slot {
            return Ok(());
        }

        let mut ff = OpenOptions::new().write(true).truncate(true).create(true).open(XAPIAN_INFO_PATH.to_owned() + "/key2slot")?;
        ff.write_all(format!("\"{}\",{}\n{}", hash, data.len(), data).as_bytes())?;

        Ok(())
    }

    fn serialize(&self) -> (String, String) {
        let mut outbuff = String::new();

        for (key, value) in self.data.iter() {
            outbuff.push('"');
            outbuff.push_str(key);
            outbuff.push('"');
            outbuff.push(',');
            outbuff.push_str(&value.to_string());
            outbuff.push('\n');
        }

        let mut hash = Hasher::new();
        hash.update(outbuff.as_bytes());

        let hash_hex = format!("{:X}", hash.finalize());

        (outbuff, hash_hex)
    }
}
