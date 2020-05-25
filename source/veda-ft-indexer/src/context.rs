use crate::error::Result;
use crate::index_schema::IndexerProperty;
use crate::index_workplace::IndexDocWorkplace;
use crate::XAPIAN_DB_TYPE;
use std::collections::HashMap;
use std::fs;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::DataType;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_onto::resource::Resource;
use xapian_rusty::{Document, Stem, TermGenerator, WritableDatabase, DB_CREATE_OR_OPEN};

pub struct Context {
    pub(crate) onto: Onto,
    pub(crate) index_dbs: HashMap<String, WritableDatabase>,
    pub(crate) indexer: TermGenerator,
    pub(crate) lang: String,
    pub(crate) key2slot: HashMap<String, u32>,
    pub(crate) db2path: HashMap<String, String>,
    pub(crate) iproperty: IndexerProperty,
    pub(crate) use_db: String,
    pub(crate) counter: i64,
}

impl Context {
    pub(crate) fn init(&mut self, use_db: &str) -> Result<()> {
        if !use_db.is_empty() {
            warn!("indexer use only {} db", use_db);
        }

        self.key2slot = get_from_key2slot();
        self.use_db = use_db.to_string();

        for (db_name, path) in self.db2path.iter() {
            fs::create_dir_all(path)?;
            if let Ok(db) = WritableDatabase::new(path, DB_CREATE_OR_OPEN, XAPIAN_DB_TYPE) {
                self.index_dbs.insert(db_name.to_owned(), db);
            }
        }

        let mut stem = Stem::new(&self.lang)?;
        self.indexer.set_stemmer(&mut stem)?;

        Ok(())
    }

    fn reload_index_schema(&mut self, module: &mut Module) {
        self.iproperty.load(true, &self.onto, module);
    }

    pub(crate) fn index_msg(&mut self, new_indv: &mut Individual, prev_indv: &mut Individual, cmd: IndvOp, op_id: i64, module: &mut Module) -> Result<()> {
        if cmd == IndvOp::Remove {
            return Ok(());
        }

        let is_deleted = if new_indv.is_exists_bool("v-s:deleted", true) {
            true
        } else {
            false
        };

        let prev_is_deleted = if prev_indv.is_exists_bool("v-s:deleted", true) {
            true
        } else {
            false
        };

        let is_restored = if prev_is_deleted && !is_deleted && self.use_db.is_empty() {
            info!("index msg: restore individual: {} ", new_indv.get_id());
            true
        } else {
            false
        };

        self.iproperty.load(false, &self.onto, module);

        if !new_indv.is_empty() {
            let is_draft_of = new_indv.get_first_literal("v-s:is_draft_of");
            let actual_version = new_indv.get_first_literal("v-s:actual_version").unwrap_or_default();
            //let previousVersion_prev = prev_indv.get_first_literal("v-s:previousVersion");
            //let previousVersion_new = new_indv.get_first_literal("v-s:previousVersion");

            if is_draft_of.is_some() {
                info!("new_indv [{}] is draft, ignore", new_indv.get_id());
                return Ok(());
            }

            if !is_deleted && !actual_version.is_empty() && actual_version != new_indv.get_id() {
                if actual_version != new_indv.get_id() {
                    info!("new[{}].v-s:actual_version[{}] != [{}], ignore", new_indv.get_id(), actual_version, new_indv.get_id());
                }
                return Ok(());
            }

            let types = new_indv.get_literals("rdf:type").unwrap_or_default();
            let prev_types = prev_indv.get_literals("rdf:type").unwrap_or_default();

            let mut prev_dbname = "base".to_owned();
            for _type in prev_types {
                prev_dbname = self.iproperty.get_dbname_of_class(&_type).to_string();
                if prev_dbname != "base" {
                    break;
                }
            }

            let uuid = "uid_".to_owned() + &to_lower_and_replace_delimeters(new_indv.get_id());

            // используем информацию о типе, для определения, в какой базе следует проводить индексацию
            let mut dbname = "base".to_owned();
            for _type in types.iter() {
                if _type == "vdi:ClassIndex" {
                    self.iproperty.add_schema_data(&mut self.onto, new_indv);
                }

                dbname = self.iproperty.get_dbname_of_class(_type).to_owned();
                if dbname != "base" {
                    break;
                }
            }

            if prev_dbname != dbname && !prev_indv.is_empty() && self.use_db.is_empty() {
                info!("[{}] prev_db[{}] != new_db[{}]", new_indv.get_id(), prev_dbname, dbname);
                info!("[{}] remove from [{}]", new_indv.get_id(), prev_dbname);

                if self.index_dbs.contains_key(&prev_dbname) {
                    if let Some(db) = self.index_dbs.get(&prev_dbname) {
                        //db.delete_document(uuid);
                    }
                }
            }

            if !is_deleted && dbname == "not-indexed" {
                return Ok(());
            }

            if !self.use_db.is_empty() {
                if is_deleted && self.use_db != "deleted" {
                    return Ok(());
                }

                if dbname != self.use_db && self.use_db != "deleted" {
                    return Ok(());
                }
            }

            let mut iwp = IndexDocWorkplace::new(Document::new()?);

            new_indv.parse_all();
            for (predicate, resources) in new_indv.get_obj().get_resources() {
                //let prefix;

                let ttype = "xsd__string";

                let p_text_ru = "";
                let p_text_en = "";

                if !resources.is_empty() {
                    iwp.index_boolean(self, &(predicate.to_owned() + ".isExists"), &Resource::new_bool(true))?;
                }

                for oo in resources {
                    for _type in types.iter() {
                        if let Some(id) = self.iproperty.get_index_id_of_uri_and_property(_type, predicate) {
                            self.prepare_index(&id, oo, predicate, 0);
                        } else {
                            if let Some(id) = self.iproperty.get_index_id_of_property(predicate) {
                                self.prepare_index(&id, oo, predicate, 0);
                            }
                        }
                    }

                    match oo.rtype {
                        DataType::Uri => {
                            iwp.index_uri(self, predicate, oo)?;
                        }
                        DataType::String => {
                            iwp.index_string(self, predicate, oo)?;
                        }
                        DataType::Integer => {
                            iwp.index_integer(self, predicate, oo)?;
                        }
                        DataType::Datetime => {
                            iwp.index_date(self, predicate, oo)?;
                        }
                        DataType::Decimal => {
                            iwp.index_double(self, predicate, oo)?;
                        }
                        DataType::Boolean => {
                            iwp.index_boolean(self, predicate, oo)?;
                        }
                        DataType::Binary => {}
                    }
                }

                if !resources.is_empty() {
                    if !p_text_ru.is_empty() {
                        let slot_l1 = self.get_slot_and_set_if_not_found(&(predicate.to_owned() + "_ru"));
                        self.indexer.index_text_with_prefix(p_text_ru, &format!("X{}X", slot_l1))?;
                        iwp.doc_add_text_value(slot_l1, p_text_ru)?;
                    }

                    if !p_text_en.is_empty() {
                        let slot_l1 = self.get_slot_and_set_if_not_found(&(predicate.to_owned() + "_en"));
                        self.indexer.index_text_with_prefix(p_text_ru, &format!("X{}X", slot_l1))?;
                        iwp.doc_add_text_value(slot_l1, p_text_en)?;
                    }
                }
            }

            self.indexer.index_text(&iwp.all_text)?;

            iwp.doc.add_boolean_term(&uuid)?;
            iwp.doc.set_data(new_indv.get_id())?;

            if self.index_dbs.contains_key("deleted") {
                if is_restored {
                    if let Some(db) = self.index_dbs.get_mut("deleted") {
                        db.delete_document(&uuid)?;
                    }
                }

                if is_deleted {
                    if let Some(db) = self.index_dbs.get_mut("deleted") {
                        db.replace_document(&uuid, &mut iwp.doc)?;
                    }
                    self.indexer.set_document(&mut Document::new()?)?;
                }
            }

            if self.index_dbs.contains_key("dbname") {
                info!("index to {}, uri={}", dbname, new_indv.get_id());
                if let Some(db) = self.index_dbs.get_mut(&dbname) {
                    db.replace_document(&uuid, &mut iwp.doc)?;
                }
            }

            if self.counter % 5000 == 0 {
                if !self.key2slot.is_empty() {
                    //store__key2slot();
                }

                //commit_all_db();
            }
        }

        self.counter = op_id;

        return Ok(());
    }

    pub(crate) fn get_slot_and_set_if_not_found(&mut self, field: &str) -> u32 {
        if let Some(slot) = get_slot(&self.key2slot, field) {
            return slot;
        }

        // create new slot
        let slot = (self.key2slot.len() + 1) as u32;
        self.key2slot.insert(field.to_owned(), slot);
        //store__key2slot();
        info!("create new slot {}={}", field, slot);
        slot
    }

    fn prepare_index(&mut self, idx_id: &str, rs: &Resource, ln: &str, level: i32) {}
}

fn get_slot(key2slot: &HashMap<String, u32>, key: &str) -> Option<u32> {
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

    if let Some(slot) = key2slot.get(key) {
        Some(slot.to_owned())
    } else {
        error!("key2slot, slot not found, key={}", key);
        None
    }
}

pub(crate) fn to_lower_and_replace_delimeters(src: &str) -> String {
    src.chars()
        .map(|x| match x {
            '-' => '_',
            ':' => '_',
            _ => x.to_ascii_lowercase(),
        })
        .collect()
}

fn get_from_key2slot() -> HashMap<String, u32> {
    //XAPIAN_INFO_PATH
    Default::default()
}
