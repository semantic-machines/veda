use crate::lmdb_storage::LMDBStorage;
use crate::tt_storage::TTStorage;
use v_onto::individual::*;

pub enum StorageError {
    None,
    NotReady,
}

#[derive(PartialEq, Debug, Clone)]
pub enum StorageMode {
    ReadOnly,
    ReadWrite,
}

#[derive(PartialEq, Debug, Clone)]
pub enum StorageId {
    Individuals,
    Tickets,
    Az,
}

pub(crate) enum EStorage {
    LMDB(LMDBStorage),
    TT(TTStorage),
}

pub trait Storage {
    fn get_individual_from_db(&mut self, storage: StorageId, uri: &str, iraw: &mut Individual) -> bool;
    fn put_kv(&mut self, storage: StorageId, key: &str, val: &str) -> bool;
    fn get_v(&mut self, storage: StorageId, key: &str) -> Option<String>;
}

pub struct VStorage {
    storage: EStorage,
}

impl VStorage {
    pub fn new_tt(tt_uri: String, login: &str, pass: &str) -> VStorage {
        VStorage {
            storage: EStorage::TT(TTStorage::new(tt_uri, login, pass)),
        }
    }

    pub fn new_lmdb(db_path: &str, mode: StorageMode) -> VStorage {
        VStorage {
            storage: EStorage::LMDB(LMDBStorage::new(db_path, mode)),
        }
    }

    pub fn get_individual(&mut self, uri: &str, iraw: &mut Individual) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.get_individual_from_db(StorageId::Individuals, uri, iraw),
            EStorage::LMDB(s) => s.get_individual_from_db(StorageId::Individuals, uri, iraw),
        }
    }

    pub fn get_individual_from_db(&mut self, storage: StorageId, uri: &str, iraw: &mut Individual) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.get_individual_from_db(storage, uri, iraw),
            EStorage::LMDB(s) => s.get_individual_from_db(storage, uri, iraw),
        }
    }

    pub fn get_value(&mut self, storage: StorageId, uri: &str) -> Option<String> {
        match &mut self.storage {
            EStorage::TT(s) => s.get_v(storage, uri),
            EStorage::LMDB(s) => s.get_v(storage, uri),
        }
    }

    pub fn put_kv(&mut self, storage: StorageId, key: &str, val: &str) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.put_kv(storage, key, val),
            EStorage::LMDB(s) => s.put_kv(storage, key, val),
        }
    }
}
