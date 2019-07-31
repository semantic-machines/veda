use crate::storage::Storage;
use lmdb_rs_m::core::{/*Database,*/ EnvCreateNoLock, EnvCreateNoMetaSync, EnvCreateNoSync, EnvCreateReadOnly};
use lmdb_rs_m::{DbFlags, DbHandle, EnvBuilder, Environment, MdbError};
use v_onto::individual::*;
use v_onto::parser::*;

pub struct LMDBStorage {
    db_path: String,
    individuals_db_handle: Result<DbHandle, MdbError>,
    individuals_db_env: Result<Environment, MdbError>,
    tickets_db_handle: Result<DbHandle, MdbError>,
    tickets_db_env: Result<Environment, MdbError>,
}

fn open(db_path: &str) -> (Result<DbHandle, MdbError>, Result<Environment, MdbError>) {
    let db_handle;

    let env_builder = EnvBuilder::new().flags(EnvCreateNoLock | EnvCreateReadOnly | EnvCreateNoMetaSync | EnvCreateNoSync);

    let db_env = env_builder.open(db_path, 0o644);

    match &db_env {
        Ok(env) => {
            db_handle = env.get_default_db(DbFlags::empty());
        }
        Err(e) => {
            error!("ERR! Authorize: Err opening environment: {:?}", e);
            db_handle = Err(MdbError::Corrupted);
        }
    }
    (db_handle, db_env)
}

impl LMDBStorage {
    pub fn new(db_path: &str) -> LMDBStorage {
        let individuals_db = open(&(db_path.to_string() + "/lmdb-individuals/"));
        let tickets_db = open(&(db_path.to_string() + "/lmdb-tickets/"));

        LMDBStorage {
            db_path: db_path.to_owned(),
            individuals_db_handle: individuals_db.0,
            individuals_db_env: individuals_db.1,
            tickets_db_handle: tickets_db.0,
            tickets_db_env: tickets_db.1,
        }
    }
}

impl Storage for LMDBStorage {
    fn set_binobj(&mut self, uri: &str, iraw: &mut Individual) -> bool {
        for _it in 0..2 {
            let mut is_need_reopen = false;
            match &self.individuals_db_env {
                Ok(env) => match &self.individuals_db_handle {
                    Ok(handle) => match env.get_reader() {
                        Ok(txn) => {
                            let db = txn.bind(&handle);

                            match db.get::<Vec<u8>>(&uri) {
                                Ok(val) => {
                                    iraw.raw.data = val;

                                    if let Ok(uri) = parse_raw(iraw) {
                                        iraw.obj.uri = uri;
                                        return true;
                                    } else {
                                        error!("LMDBStorage: fail parse binobj, len={}, uri={}", iraw.raw.data.len(), uri);
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
                let res = open(&(self.db_path.clone() + "/lmdb-individuals/"));

                self.individuals_db_handle = res.0;
                self.individuals_db_env = res.1;
            }
        }

        false
    }

    fn get_sys_ticket_id(&mut self) -> Result<String, i32> {
        for _it in 0..2 {
            let mut is_need_reopen = false;
            match &self.individuals_db_env {
                Ok(env) => match &self.individuals_db_handle {
                    Ok(handle) => match env.get_reader() {
                        Ok(txn) => {
                            let db = txn.bind(&handle);

                            match db.get::<Vec<u8>>(&"systicket") {
                                Ok(val) => {
                                    return Ok(String::from_utf8_lossy(val.as_ref()).to_string());
                                }
                                Err(e) => match e {
                                    MdbError::NotFound => {
                                        return Err(-1);
                                    }
                                    _ => {
                                        error!("db.get {:?}, {}", e, "systicket");
                                        return Err(-1);
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
                                    return Err(-1);
                                }
                            }
                            _ => {
                                error!("fail crate transaction, err={}", e);
                            }
                        },
                    },
                    Err(e) => {
                        error!("db handle, err={}", e);
                        return Err(-1);
                    }
                },
                Err(e) => {
                    error!("db environment, err={}", e);
                    return Err(-1);
                }
            }

            if is_need_reopen {
                warn!("db {} reopen", self.db_path);
                let res = open(&(self.db_path.clone() + "/lmdb-tickets/"));

                self.tickets_db_handle = res.0;
                self.tickets_db_env = res.1;
            }
        }

        Err(-1)
    }
}
