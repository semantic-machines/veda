use crate::storage::{StorageId, StorageMode, VStorage};
use nng::{Message, Protocol, Socket};
use std::cell::RefCell;
use std::str;
use std::sync::Mutex;
use uuid::Uuid;
use v_onto::individual::{Individual, RawObj};

pub fn storage_manager(tarantool_addr: String) -> std::io::Result<()> {
    let ro_storage_url = "inproc://nng/".to_owned() + &Uuid::new_v4().to_hyphenated().to_string();
    STORAGE.lock().unwrap().get_mut().addr = ro_storage_url.to_owned();

    let mut storage: VStorage;
    if !tarantool_addr.is_empty() {
        storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
    } else {
        storage = VStorage::new_lmdb("./data", StorageMode::ReadOnly);
    }

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&ro_storage_url) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = req_prepare(&recv_msg, &mut storage);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }
}

fn req_prepare(request: &Message, storage: &mut VStorage) -> Message {
    if let Ok(id) = str::from_utf8(request.as_slice()) {
        let binobj = storage.get_raw_value(StorageId::Individuals, id);
        if binobj.is_empty() {
            return Message::from("[]".as_bytes());
        }
        return Message::from(binobj.as_slice());
    }

    Message::default()
}

// Inproc client

pub struct StorageROClient {
    soc: Socket,
    addr: String,
    is_ready: bool,
}

lazy_static! {
    pub static ref STORAGE: Mutex<RefCell<StorageROClient>> = Mutex::new(RefCell::new(StorageROClient::default()));
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
    fn connect(&mut self) {
        if let Err(e) = self.soc.dial(&self.addr) {
            error!("fail connect to storage_manager ({}), err={:?}", self.addr, e);
            self.is_ready = false;
        } else {
            info!("success connect connect to storage_manager ({})", self.addr);
            self.is_ready = true;
        }
    }
}

pub fn get_individual(id: &str) -> Option<Individual> {
    let req = Message::from(id.to_string().as_bytes());

    let mut sh_client = STORAGE.lock().unwrap();
    let client = sh_client.get_mut();

    if !client.is_ready {
        client.connect();
    }

    if !client.is_ready {
        return None;
    }

    if let Err(e) = client.soc.send(req) {
        error!("fail send to storage_manager, err={:?}", e);
        return None;
    }

    // Wait for the response from the server.
    let wmsg = client.soc.recv();
    if let Err(e) = wmsg {
        error!("fail recv from main module, err={:?}", e);
        return None;
    }

    if let Ok(msg) = wmsg {
        return Some(Individual::new_raw(RawObj::new(msg.as_slice().to_vec())));
    }

    None
}
