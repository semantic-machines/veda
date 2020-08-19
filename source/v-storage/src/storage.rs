use crate::lmdb_storage::LMDBStorage;
use crate::remote_storage_client::*;
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
    REMOTE(StorageROClient),
}

pub trait Storage {
    fn get_individual_from_db(&mut self, storage: StorageId, id: &str, iraw: &mut Individual) -> bool;
    fn put_kv(&mut self, storage: StorageId, key: &str, val: &str) -> bool;
    fn put_kv_raw(&mut self, storage: StorageId, key: &str, val: Vec<u8>) -> bool;
    fn get_v(&mut self, storage: StorageId, key: &str) -> Option<String>;
    fn get_raw(&mut self, storage: StorageId, key: &str) -> Vec<u8>;
    fn remove(&mut self, storage: StorageId, key: &str) -> bool;
}

pub struct VStorage {
    storage: EStorage,
}

impl VStorage {
    pub fn new_remote(addr: &str) -> VStorage {
        VStorage {
            storage: EStorage::REMOTE(StorageROClient::new(addr)),
        }
    }

    pub fn new_tt(tt_id: String, login: &str, pass: &str) -> VStorage {
        VStorage {
            storage: EStorage::TT(TTStorage::new(tt_id, login, pass)),
        }
    }

    pub fn new_lmdb(db_path: &str, mode: StorageMode) -> VStorage {
        VStorage {
            storage: EStorage::LMDB(LMDBStorage::new(db_path, mode)),
        }
    }

    pub fn get_individual(&mut self, id: &str, iraw: &mut Individual) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.get_individual_from_db(StorageId::Individuals, id, iraw),
            EStorage::LMDB(s) => s.get_individual_from_db(StorageId::Individuals, id, iraw),
            EStorage::REMOTE(s) => s.get_individual_from_db(StorageId::Individuals, id, iraw),
        }
    }

    pub fn get_individual_from_db(&mut self, storage: StorageId, id: &str, iraw: &mut Individual) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.get_individual_from_db(storage, id, iraw),
            EStorage::LMDB(s) => s.get_individual_from_db(storage, id, iraw),
            EStorage::REMOTE(s) => s.get_individual_from_db(storage, id, iraw),
        }
    }

    pub fn get_value(&mut self, storage: StorageId, id: &str) -> Option<String> {
        match &mut self.storage {
            EStorage::TT(s) => s.get_v(storage, id),
            EStorage::LMDB(s) => s.get_v(storage, id),
            EStorage::REMOTE(_s) => None,
        }
    }

    pub fn get_raw_value(&mut self, storage: StorageId, id: &str) -> Vec<u8> {
        match &mut self.storage {
            EStorage::TT(s) => s.get_raw(storage, id),
            EStorage::LMDB(s) => s.get_raw(storage, id),
            EStorage::REMOTE(_s) => Default::default(),
        }
    }

    pub fn put_kv(&mut self, storage: StorageId, key: &str, val: &str) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.put_kv(storage, key, val),
            EStorage::LMDB(s) => s.put_kv(storage, key, val),
            EStorage::REMOTE(_s) => false,
        }
    }

    pub fn put_kv_raw(&mut self, storage: StorageId, key: &str, val: Vec<u8>) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.put_kv_raw(storage, key, val),
            EStorage::LMDB(s) => s.put_kv_raw(storage, key, val),
            EStorage::REMOTE(_s) => false,
        }
    }

    pub fn remove(&mut self, storage: StorageId, key: &str) -> bool {
        match &mut self.storage {
            EStorage::TT(s) => s.remove(storage, key),
            EStorage::LMDB(s) => s.remove(storage, key),
            EStorage::REMOTE(_s) => false,
        }
    }
}
