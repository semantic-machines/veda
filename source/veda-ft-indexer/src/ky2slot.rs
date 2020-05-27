use crc32fast::Hasher;
use std::collections::HashMap;

pub struct Key2Slot {
    data: HashMap<String, u32>,
    last_size_key2slot: usize,
}

impl Default for Key2Slot {
    fn default() -> Self {
        Key2Slot {
            data: Default::default(),
            last_size_key2slot: 0,
        }
    }
}

impl Key2Slot {
    pub(crate) fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn get_slot(&self, key: &str) -> Option<u32> {
        if key.is_empty() {
            return None;
        }

        if let Some(c) = key.chars().nth(0) {
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
            error!("key2slot, slot not found, key={}", key);
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
        self.store();
        info!("create new slot {}={}", field, slot);
        slot
    }

    pub fn load() -> Key2Slot {
        //XAPIAN_INFO_PATH
        let mut key2slot = Key2Slot::default();
        let src = "";
        key2slot.deserialize(src);
        key2slot
    }

    pub fn store(&mut self) {
        let (data, hash) = self.serialize();

        if data.len() == self.last_size_key2slot {
            return;
        }

        /*
           try {
           ff_key2slot_w.seek(0);
           ff_key2slot_w.write('"');
           ff_key2slot_w.write(hash);
           ff_key2slot_w.write("\",");
           ff_key2slot_w.write(data.length);
           ff_key2slot_w.write('\n');
           ff_key2slot_w.write(data);
           ff_key2slot_w.flush();

           last_size_key2slot = data.length;
           } catch (Throwable tr)   {
           log.trace("fail store__key2slot [%s] [%s]", data, tr.msg);
           return;
           }

        */
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

        return (outbuff, hash_hex);
    }

    fn deserialize(&mut self, src: &str) {}
}
