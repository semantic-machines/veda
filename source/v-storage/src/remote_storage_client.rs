use crate::storage::StorageId;
use nng::{Message, Protocol, Socket};
use std::str;
use v_onto::individual::Individual;
use v_onto::parser::*;

// Remote client

pub struct StorageROClient {
    pub(crate) soc: Socket,
    pub(crate) addr: String,
    pub(crate) is_ready: bool,
}

impl Default for StorageROClient {
    fn default() -> Self {
        StorageROClient {
            soc: Socket::new(Protocol::Req0).unwrap(),
            addr: "".to_owned(),
            is_ready: false,
        }
    }
}

impl StorageROClient {
    pub fn new(addr: &str) -> Self {
        StorageROClient {
            soc: Socket::new(Protocol::Req0).unwrap(),
            addr: addr.to_string(),
            is_ready: false,
        }
    }

    pub fn connect(&mut self) -> bool {
        if let Err(e) = self.soc.dial(&self.addr) {
            error!("fail connect to storage_manager ({}), err={:?}", self.addr, e);
            self.is_ready = false;
        } else {
            info!("success connect connect to storage_manager ({})", self.addr);
            self.is_ready = true;
        }
        self.is_ready
    }

    pub fn get_individual_from_db(&mut self, db_id: StorageId, id: &str, iraw: &mut Individual) -> bool {
        if !self.is_ready {
            if !self.connect() {
                error!("REMOTE STORAGE: fail send to storage_manager, not ready");
                return false;
            }
        }

        let req = if db_id == StorageId::Tickets {
            Message::from(("t,".to_string() + id).as_bytes())
        } else {
            Message::from(("i,".to_string() + id).as_bytes())
        };

        if let Err(e) = self.soc.send(req) {
            error!("REMOTE STORAGE: fail send to storage_manager, err={:?}", e);
            return false;
        }

        // Wait for the response from the server.
        match self.soc.recv() {
            Err(e) => {
                error!("REMOTE STORAGE: fail recv from main module, err={:?}", e);
                return false;
            }

            Ok(msg) => {
                let data = msg.as_slice();
                if data == b"[]" {
                    return false;
                }

                iraw.set_raw(data);

                if parse_raw(iraw).is_ok() {
                    return true;
                } else {
                    error!("REMOTE STORAGE: fail parse binobj, len={}, uri=[{}]", iraw.get_raw_len(), id);
                    return false;
                }
            }
        }
    }
}
