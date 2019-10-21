use crate::storage::*;
use lmdb_rs_m::core::{EnvCreateNoLock, EnvCreateNoMetaSync, EnvCreateNoSync, EnvCreateReadOnly};
use lmdb_rs_m::{DbFlags, DbHandle, EnvBuilder, Environment, MdbError};
use v_onto::individual::*;
use v_onto::parser::*;

pub(crate) struct LMDBStorage {
    db_path: String,
    individuals_db_handle: Result<DbHandle, MdbError>,
    individuals_db_env: Result<Environment, MdbError>,
    tickets_db_handle: Result<DbHandle, MdbError>,
    tickets_db_env: Result<Environment, MdbError>,
    mode: StorageMode,
}

impl LMDBStorage {
    pub fn new(db_path: &str, mode: StorageMode) -> LMDBStorage {
        let mut storage = LMDBStorage {
            db_path: db_path.to_owned(),
            individuals_db_handle: Err(MdbError::NotFound),
            individuals_db_env: Err(MdbError::NotFound),
            tickets_db_handle: Err(MdbError::NotFound),
            tickets_db_env: Err(MdbError::NotFound),
            mode: mode.clone(),
        };

        storage.open(StorageId::Individuals, mode.clone());
        storage.open(StorageId::Tickets, mode.clone());
        storage
    }

    fn open(&mut self, storage: StorageId, mode: StorageMode) {
        let db_handle;

        let db_path = if storage == StorageId::Individuals {
            self.db_path.to_string() + "/lmdb-individuals/"
        } else {
            self.db_path.to_string() + "/lmdb-tickets/"
        };

        let env_builder = if mode == StorageMode::ReadOnly {
            EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync)
        } else {
            EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateNoMetaSync | EnvCreateNoSync)
        };

        let db_env = env_builder.open(db_path, 0o644);

        match &db_env {
            Ok(env) => {
                db_handle = env.get_default_db(DbFlags::empty());
            }
            Err(e) => {
                error!("ERR! LMDB: fail opening read only environment, err={:?}", e);
                db_handle = Err(MdbError::Corrupted);
            }
        }

        if storage == StorageId::Individuals {
            self.individuals_db_handle = db_handle;
            self.individuals_db_env = db_env;
        } else {
            self.tickets_db_handle = db_handle;
            self.tickets_db_env = db_env;
        }
    }
}

impl Storage for LMDBStorage {
    fn get_individual_from_db(&mut self, storage: StorageId, uri: &str, iraw: &mut Individual) -> bool {
        for _it in 0..2 {
            let db_handle;
            let db_env;

            if storage == StorageId::Individuals {
                db_env = &self.individuals_db_env;
                db_handle = &self.individuals_db_handle;
            } else {
                db_env = &self.tickets_db_env;
                db_handle = &self.tickets_db_handle;
            }

            let mut is_need_reopen = false;
            match db_env {
                Ok(env) => match db_handle {
                    Ok(handle) => match env.get_reader() {
                        Ok(txn) => {
                            let db = txn.bind(&handle);

                            match db.get::<&[u8]>(&uri) {
                                Ok(val) => {
                                    iraw.set_raw(val);

                                    if parse_raw(iraw).is_ok() {
                                        return true;
                                    } else {
                                        error!("LMDBStorage: fail parse binobj, len={}, uri={}", iraw.get_raw_len(), uri);
                                        return false;
                                    }
                                }
                                Err(e) => match e {
                                    MdbError::NotFound => {
                                        return false;
                                    }
                                    _ => {
                                        error!("db.get {:?}, {}", e, uri);
                                        return false;
                                    }
                                },
                            }
                        }
                        Err(e) => match e {
                            MdbError::Other(c, _) => {
                                if c == -30785 {
                                    is_need_reopen = true;
                                } else {
                                    error!("fail crate transaction, err={}", e);
                                    return false;
                                }
                            }
                            _ => {
                                error!("fail crate transaction, err={}", e);
                            }
                        },
                    },
                    Err(e) => {
                        error!("db handle, err={}", e);
                        return false;
                    }
                },
                Err(e) => {
                    error!("db environment, err={}", e);
                    return false;
                }
            }

            if is_need_reopen {
                warn!("db {} reopen", self.db_path);

                self.open(storage.clone(), self.mode.clone());
            }
        }

        false
    }

    fn put_kv(&mut self, storage: StorageId, key: &str, val: &str) -> bool {
        /*
            let txn = env.new_transaction().unwrap();
            {
                let db = txn.bind(&db_handle); // get a database bound to this transaction

                let pairs = vec![("Albert", "Einstein",),
                                 ("Joe", "Smith",),
                                 ("Jack", "Daniels")];

                for &(name, surname) in pairs.iter() {
                    db.set(&surname, &name).unwrap();
                }
            }

            // Note: `commit` is choosen to be explicit as
            // in case of failure it is responsibility of
            // the client to handle the error
            match txn.commit() {
                Err(_) => panic!("failed to commit!"),
                Ok(_) => ()
            }
        */
        false
    }
}
