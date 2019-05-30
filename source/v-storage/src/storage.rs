use crate::lmdb_storage::LMDBStorage;
use crate::tt_storage::TTStorage;
use v_onto::individual::Individual;

pub trait Storage {
    //fn new(tt_uri: String, login: &str, pass: &str) -> Self;
    fn set_binobj(&mut self, uri: &str, indv: &mut Individual) -> bool;
}

pub enum TypeStorage {
    LMDB,
    TT,
}

pub struct VStorage {
    tt_storage: TTStorage,
    lmdb_storage: LMDBStorage,
    db_type: TypeStorage,
}

impl VStorage {
    pub fn new_tt(tt_uri: String, login: &str, pass: &str) -> VStorage {
        VStorage {
            tt_storage: TTStorage::new(tt_uri, login, pass),
            lmdb_storage: LMDBStorage::default(),
            db_type: TypeStorage::TT,
        }
    }

    pub fn new_lmdb(db_path: &str) -> VStorage {
        VStorage {
            tt_storage: TTStorage::default(),
            lmdb_storage: LMDBStorage::new(db_path),
            db_type: TypeStorage::TT,
        }
    }

    pub fn set_binobj(&mut self, uri: &str, indv: &mut Individual) -> bool {
        match self.db_type {
            TypeStorage::TT => {
                return self.tt_storage.set_binobj(uri, indv);
            }
            TypeStorage::LMDB => {
                return self.lmdb_storage.set_binobj(uri, indv);
            }
        }
    }
}
