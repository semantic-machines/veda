use crate::storage::Storage;
use futures::future::Future;
use rusty_tarantool::tarantool::{Client, ClientConfig};
use std::net::SocketAddr;
use std::str;
use tokio::runtime::current_thread::Runtime;
use v_onto::individual::*;
use v_onto::parser::*;

pub struct TTStorage {
    rt: Runtime,
    client: Client,
}

const INDIVIDUALS_SPACE_ID: i32 = 512;
const TICKETS_SPACE_ID: i32 = 513;

impl TTStorage {
    pub fn new(tt_uri: String, login: &str, pass: &str) -> TTStorage {
        let addr: SocketAddr = tt_uri.parse().unwrap();
        TTStorage {
            rt: Runtime::new().unwrap(),
            client: ClientConfig::new(addr, login, pass).set_timeout_time_ms(1000).set_reconnect_time_ms(10000).build(),
        }
    }
}

impl Storage for TTStorage {
    fn set_binobj(&mut self, uri: &str, iraw: &mut Individual) -> bool {
        let key = (uri,);

        let resp = self.client.select(INDIVIDUALS_SPACE_ID, 0, &key, 0, 100, 0).and_then(move |response| Ok(response.data));

        if let Ok(v) = self.rt.block_on(resp) {
            iraw.raw.data = v[5..].to_vec();

            if let Ok(uri) = parse_raw(iraw) {
                iraw.obj.uri = uri;
                return true;
            } else {
                error!("TTStorage: fail parse binobj, len={}, uri={}", iraw.raw.data.len(), uri);
            }
        }

        false
    }

    fn get_sys_ticket_id(&mut self) -> Result<String, i32> {
        let key = ("systicket",);
        let resp = self.client.select(TICKETS_SPACE_ID, 0, &key, 0, 100, 0).and_then(move |response| Ok(response.data));

        if let Ok(v) = self.rt.block_on(resp) {
            return Ok(String::from_utf8_lossy(&v[5..]).to_string());
        }
        Err(-1)
    }
}
