use crate::lmdb_storage::LMDBStorage;
use crate::tt_storage::TTStorage;
use v_onto::individual::*;

pub enum StorageError {
    None,
    NotReady,
}

#[derive(PartialEq, Debug, Clone)]
pub enum StorageId {
    Individuals,
    Tickets,
}

pub enum EStorage {
    LMDB(LMDBStorage),
    TT(TTStorage),
}

pub trait Storage {
    fn get_individual_from_db(&mut self, storage: StorageId, uri: &str, iraw: &mut Individual) -> bool;
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

    pub fn new_lmdb(db_path: &str) -> VStorage {
        VStorage {
            storage: EStorage::LMDB(LMDBStorage::new(db_path)),
        }
    }

    pub fn get_individual1<'a>(&mut self, uri: &str, iraw: &'a mut Individual) -> Option<&'a mut Individual> {
        let res = match &mut self.storage {
            EStorage::TT(s) => s.get_individual_from_db(StorageId::Individuals, uri, iraw),
            EStorage::LMDB(s) => s.get_individual_from_db(StorageId::Individuals, uri, iraw),
        };

        if res {
            return Some(iraw);
        }

        None
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
}
