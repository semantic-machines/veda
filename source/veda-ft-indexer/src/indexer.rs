use crate::error::Result;
use crate::index_schema::IndexerProperty;
use crate::index_workplace::IndexDocWorkplace;
use crate::ky2slot::Key2Slot;
use crate::XAPIAN_DB_TYPE;
use std::collections::HashMap;
use std::fs;
use std::process::exit;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::datatype::DataType;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_onto::resource::Resource;
use xapian_rusty::{get_xapian_err_type, Document, Stem, TermGenerator, WritableDatabase, DB_CREATE_OR_OPEN};

pub struct Indexer {
    pub(crate) onto: Onto,
    pub(crate) index_dbs: HashMap<String, WritableDatabase>,
    pub(crate) tg: TermGenerator,
    pub(crate) lang: String,
    pub(crate) key2slot: Key2Slot,
    pub(crate) db2path: HashMap<String, String>,
    pub(crate) idx_prop: IndexerProperty,
    pub(crate) use_db: String,
    pub(crate) counter: i64,
    pub(crate) prev_committed_counter: i64,
}

impl Indexer {
    pub(crate) fn init(&mut self, use_db: &str) -> Result<()> {
        if !use_db.is_empty() {
            warn!("indexer use only {} db", use_db);
        }

        self.key2slot = Key2Slot::load()?;
        self.use_db = use_db.to_string();

        for (db_name, path) in self.db2path.iter() {
            fs::create_dir_all(path)?;
            if let Ok(db) = WritableDatabase::new(path, DB_CREATE_OR_OPEN, XAPIAN_DB_TYPE) {
                self.index_dbs.insert(db_name.to_owned(), db);
            }
        }

        let mut stem = Stem::new(&self.lang)?;
        self.tg.set_stemmer(&mut stem)?;

        Ok(())
    }

    fn reload_index_schema(&mut self, module: &mut Module) {
        self.idx_prop.load(true, &self.onto, module);
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

        self.idx_prop.load(false, &self.onto, module);

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
                prev_dbname = self.idx_prop.get_dbname_of_class(&_type).to_string();
                if prev_dbname != "base" {
                    break;
                }
            }

            let uuid = "uid_".to_owned() + &to_lower_and_replace_delimeters(new_indv.get_id());

            // используем информацию о типе, для определения, в какой базе следует проводить индексацию
            let mut dbname = "base".to_owned();
            for _type in types.iter() {
                if _type == "vdi:ClassIndex" {
                    self.idx_prop.add_schema_data(&mut self.onto, new_indv);
                }

                dbname = self.idx_prop.get_dbname_of_class(_type).to_owned();
                if dbname != "base" {
                    break;
                }
            }

            if prev_dbname != dbname && !prev_indv.is_empty() && self.use_db.is_empty() {
                info!("[{}] prev_db[{}] != new_db[{}]", new_indv.get_id(), prev_dbname, dbname);
                info!("[{}] remove from [{}]", new_indv.get_id(), prev_dbname);

                if self.index_dbs.contains_key(&prev_dbname) {
                    if let Some(db) = self.index_dbs.get_mut(&prev_dbname) {
                        db.delete_document(&uuid)?;
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

                //let ttype = "xsd__string";

                let p_text_ru = "";
                let p_text_en = "";

                if !resources.is_empty() {
                    iwp.index_boolean(self, &(predicate.to_owned() + ".isExists"), &Resource::new_bool(true))?;
                }

                for oo in resources {
                    for _type in types.iter() {
                        if let Some(id) = self.idx_prop.get_index_id_of_uri_and_property(_type, predicate) {
                            self.prepare_index(&id, oo, predicate, 0);
                        } else {
                            if let Some(id) = self.idx_prop.get_index_id_of_property(predicate) {
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
                        let slot_l1 = self.key2slot.get_slot_and_set_if_not_found(&(predicate.to_owned() + "_ru"));
                        self.tg.index_text_with_prefix(p_text_ru, &format!("X{}X", slot_l1))?;
                        iwp.doc_add_text_value(slot_l1, p_text_ru)?;
                    }

                    if !p_text_en.is_empty() {
                        let slot_l1 = self.key2slot.get_slot_and_set_if_not_found(&(predicate.to_owned() + "_en"));
                        self.tg.index_text_with_prefix(p_text_ru, &format!("X{}X", slot_l1))?;
                        iwp.doc_add_text_value(slot_l1, p_text_en)?;
                    }
                }
            }

            self.tg.index_text(&iwp.all_text)?;

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
                    self.tg.set_document(&mut Document::new()?)?;
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
                    if let Err(e) = self.key2slot.store() {
                        error!("fail store key2slot, err={:?}", e);
                    }
                }

                self.commit_all_db();
            }
        }

        self.counter = op_id;

        return Ok(());
    }

    fn commit_all_db(&mut self) {
        let delta = self.counter - self.prev_committed_counter;

        self.prev_committed_counter = self.counter;
        let mut is_fail_commit = false;
        for (name, db) in self.index_dbs.iter_mut() {
            if let Err(e) = db.commit() {
                is_fail_commit = true;
                error!("FT:commit:{} fail={}, err={:?}", name, self.counter, get_xapian_err_type(e));
            }
        }

        if is_fail_commit == true {
            warn!("EXIT");
            exit(-1);
        }
    }

    fn prepare_index(&mut self, idx_id: &str, rs: &Resource, ln: &str, level: i32) {}
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
