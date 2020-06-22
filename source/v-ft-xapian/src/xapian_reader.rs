use crate::index_schema::*;
use crate::key2slot::*;
use crate::vql::*;
use crate::xapian_vql::*;
use crate::xerror::*;
use std::collections::HashMap;
use std::io::{Error, ErrorKind};
use v_api::app::ResultCode;
//use v_module::info::ModuleInfo;
use crate::init_db_path;
use crate::xerror::XError::{Io, Xapian};
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_search::common::QueryResult;
use v_storage::storage::VStorage;
use xapian_rusty::{get_xapian_err_type, Database, Query, QueryParser, Stem, UNKNOWN};

const XAPIAN_DB_TYPE: i8 = UNKNOWN;
const MAX_WILDCARD_EXPANSION: i32 = 20_000;

pub struct DatabaseQueryParser {
    db: Database,
    qp: QueryParser,
}

impl DatabaseQueryParser {
    fn add_database(&mut self, db_name: &str, opened_db: &mut HashMap<String, Database>) -> Result<()> {
        if let Some(add_db) = opened_db.get_mut(db_name) {
            self.db.add_database(add_db)?;
        }
        Ok(())
    }
}

pub struct XapianReader {
    using_dbqp: HashMap<Vec<String>, DatabaseQueryParser>,
    opened_db: HashMap<String, Database>,
    xapian_stemmer: Stem,
    xapian_lang: String,
    index_schema: IndexerSchema,
    // mdif: ModuleInfo,
    key2slot: Key2Slot,
    onto: Onto,
    db2path: HashMap<String, String>,
}

impl XapianReader {
    pub fn new(lang: &str, storage: &mut VStorage, onto: Onto) -> Option<Self> {
        let key2slot = Key2Slot::load();
        if key2slot.is_err() {
            error!("load key2slot, err={:?}", key2slot.err());
            return None;
        }

        let mut xr = XapianReader {
            using_dbqp: Default::default(),
            opened_db: Default::default(),
            xapian_stemmer: Stem::new(lang).unwrap(),
            xapian_lang: "".to_string(),
            index_schema: Default::default(),
            //mdif: module_info.unwrap(),
            key2slot: key2slot.unwrap(),
            onto,
            db2path: init_db_path(),
        };

        xr.load_index_schema(storage);

        Some(xr)
    }

    pub fn query<T>(
        &mut self,
        user_uri: &str,
        str_query: &str,
        str_sort: &str,
        db_names_str: &str,
        from: i32,
        top: i32,
        limit: i32,
        add_out_element: fn(uri: &str, ctx: &mut T),
        op_auth: OptAuthorize,
        ctx: &mut T,
    ) -> Result<QueryResult> {
        let mut sr = QueryResult::default();

        let wtta = TTA::parse_expr(str_query);

        if wtta.is_none() {
            error!("fail parse query (phase 1) [{}], tta is empty", str_query);
            sr.result_code = ResultCode::BadRequest;
            return Ok(sr);
        }

        let mut tta = wtta.unwrap();

        let db_names = self.get_dn_names(&tta, db_names_str);

        info!("db_names={:?}", db_names);
        info!("user_uri=[{}] query=[{}] str_sort=[{}], db_names=[{:?}], from=[{}], top=[{}], limit=[{}]", user_uri, str_query, str_sort, db_names, from, top, limit);
        info!("TTA [{}]", tta);

        //        long cur_committed_op_id = get_info().committed_op_id;
        //        if (cur_committed_op_id > committed_op_id) {
        //log.trace("search:reopen_db: cur_committed_op_id(%d) > committed_op_id(%d)", cur_committed_op_id, committed_op_id);
        //            reopen_dbs();
        //        } else   {
        //log.trace ("search:check reopen_db: cur_committed_op_id=%d, committed_op_id=%d", cur_committed_op_id, committed_op_id);
        //       }

        self.open_dbqp_if_need(&db_names)?;

        let mut query = Query::new()?;
        //loop {
        if let Some(dbqp) = self.using_dbqp.get_mut(&db_names) {
            let mut _rd: f64 = 0.0;
            //if
            transform_vql_to_xapian(&mut tta, "", None, None, &mut query, &self.key2slot, &mut _rd, 0, &mut dbqp.qp, &self.onto)?;
            //.is_ok() {
            //    break;
            //}
        }
        //}

        info!("query={:?}", query.get_description());

        if query.is_empty() {
            sr.result_code = ResultCode::BadRequest;
            error!("fail prepare query [{}]", str_query);
            return Ok(sr);
        }

        if let Some(dbqp) = self.using_dbqp.get_mut(&db_names) {
            let mut xapian_enquire = dbqp.db.new_enquire()?;

            xapian_enquire.set_query(&mut query)?;

            if let Some(mut sorter) = get_sorter(str_sort, &self.key2slot)? {
                xapian_enquire.set_sort_by_key(&mut sorter, true)?;
            }

            sr = exec_xapian_query_and_queue_authorize(user_uri, &mut xapian_enquire, from, top, limit, add_out_element, op_auth, ctx);
        }

        Ok(sr)
    }

    pub fn load_index_schema(&mut self, storage: &mut VStorage) {
        fn add_out_element(id: &str, ctx: &mut Vec<String>) {
            ctx.push(id.to_owned());
        }

        let mut ctx = vec![];

        match self.query("cfg:VedaSystem", "'rdf:type' === 'vdi:ClassIndex'", "", "", 0, 0, 0, add_out_element, OptAuthorize::NO, &mut ctx) {
            Ok(res) => {
                if res.result_code == ResultCode::Ok && res.count > 0 {
                    for id in ctx.iter() {
                        let mut indv = &mut Individual::default();
                        if storage.get_individual(id, &mut indv) {
                            self.index_schema.add_schema_data(&self.onto, indv);
                        }
                    }
                } else {
                    error!("fail load index schema, err={:?}", res.result_code);
                }
            }
            Err(e) => match e {
                Xapian(code) => {
                    error!("fail load index schema, err={} ({})", get_xapian_err_type(code), code);
                }
                Io(e) => {
                    error!("fail load index schema, err={:?}", e);
                }
            },
        }
    }

    fn _reopen_dbs(&mut self) -> Result<()> {
        //let cur_committed_op_id = get_info().committed_op_id;
        //debug!("reopen_db, prev committed_op_id={}, now committed_op_id={}", committed_op_id, cur_committed_op_id);

        for (_, el) in self.using_dbqp.iter_mut() {
            el.db.reopen()?;
            el.qp.set_database(&mut el.db)?;
        }

        for (_, db) in self.opened_db.iter_mut() {
            db.reopen()?;
        }

        //    committed_op_id = cur_committed_op_id;

        Ok(())
    }

    fn _close_dbs(&mut self) -> Result<()> {
        for (_, el) in self.using_dbqp.iter_mut() {
            el.db.close()?;
        }

        for (_, db) in self.opened_db.iter_mut() {
            db.close()?;
        }

        Ok(())
    }

    fn open_db_if_need(&mut self, db_name: &str) -> Result<()> {
        if !self.opened_db.contains_key(db_name) {
            if let Some(path) = self.db2path.get(db_name) {
                let db = Database::new_with_path(&("./".to_owned() + path), XAPIAN_DB_TYPE)?;
                self.opened_db.insert(db_name.to_owned(), db);
            } else {
                return Err(XError::from(Error::new(ErrorKind::Other, "db2path invalid")));
            }
        }
        return Ok(());
    }

    fn open_dbqp_if_need(&mut self, db_names: &Vec<String>) -> Result<()> {
        if !self.using_dbqp.contains_key(db_names) {
            for el in db_names {
                self.open_db_if_need(el)?;
            }

            let mut dbqp = DatabaseQueryParser {
                db: Database::new()?,
                qp: QueryParser::new()?,
            };

            for el in db_names {
                self.open_db_if_need(el)?;
                dbqp.add_database(el, &mut self.opened_db)?;
            }

            dbqp.qp.set_max_wildcard_expansion(MAX_WILDCARD_EXPANSION)?;

            self.xapian_stemmer = Stem::new(&self.xapian_lang)?;

            dbqp.qp.set_stemmer(&mut self.xapian_stemmer)?;

            dbqp.qp.set_database(&mut dbqp.db)?;

            self.using_dbqp.insert(db_names.clone(), dbqp);
        }
        /*
           committed_op_id = get_info().committed_op_id;
        */
        Ok(())
    }

    fn get_dn_names(&self, tta: &TTA, db_names_str: &str) -> Vec<String> {
        let mut db_names = vec![];

        if db_names_str.is_empty() {
            let mut databases = HashMap::new();
            self.db_names_from_tta(&tta, &mut databases);

            for (key, value) in databases.iter() {
                if *value == false {
                    if key != "not-indexed" {
                        db_names.push(key.to_owned());
                    }

                    // при автоопределении баз, если находится база deleted, то другие базы исключаются
                    if key == "deleted" {
                        db_names.clear();
                        db_names.push(key.to_owned());
                        break;
                    }
                }
            }
        } else {
            for el in db_names_str.split(',') {
                db_names.push(String::from(el).trim().to_owned());
            }
        }

        if db_names.is_empty() {
            db_names.push("base".to_owned());
        }

        db_names
    }

    fn db_names_from_tta(&self, tta: &TTA, db_names: &mut HashMap<String, bool>) -> String {
        let mut ll = String::default();
        let mut rr = String::default();

        if let Some(l) = &tta.l {
            ll = self.db_names_from_tta(l, db_names)
        };

        if let Some(r) = &tta.r {
            rr = self.db_names_from_tta(r, db_names);
        }

        if !ll.is_empty() && !rr.is_empty() {
            if ll == "rdf:type" {
                let dbn = self.index_schema.get_dbname_of_class(&rr);
                db_names.insert(dbn.to_owned(), false);
            } else if ll == "v-s:deleted" {
                db_names.insert("deleted".to_owned(), false);
            }
        }

        tta.op.to_owned()
    }
}
