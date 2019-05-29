#[macro_use]
extern crate log;

use futures::future::Future;
use rusty_tarantool::tarantool::{Client, ClientConfig};
use std::net::SocketAddr;
use tokio::runtime::current_thread::Runtime;
use v_onto::individual::*;
use v_onto::msgpack8individual::msgpack2individual;

pub trait Storage {
    fn new(tt_uri: String, login: &str, pass: &str) -> Self;
    fn get_first_integer(&mut self, uri: &str, predicate: &str) -> i64;
}

pub struct TTStorage {
    rt: Runtime,
    client: Client,
    space_id: i32,
}

impl Storage for TTStorage {
    fn new(tt_uri: String, login: &str, pass: &str) -> TTStorage {
        let addr: SocketAddr = tt_uri.parse().unwrap();
        TTStorage {
            rt: Runtime::new().unwrap(),
            space_id: 512,
            client: ClientConfig::new(addr, login, pass).set_timeout_time_ms(1000).set_reconnect_time_ms(10000).build(),
        }
    }

    fn get_first_integer(&mut self, uri: &str, predicate: &str) -> i64 {
        let key = (uri,);

        let resp = self.client.select(self.space_id, 0, &key, 0, 100, 0).and_then(move |response| {
            let mut indv = Individual::new(response.data[5..].to_vec());

            if msgpack2individual(&mut indv) == false {
                error!("fail parse, uri={}", uri);
                return Ok(0);
            } else {
                return Ok(indv.get_first_integer(predicate));
                //println!("msg: {}", msg);
            }
        });

        if let Ok(c) = self.rt.block_on(resp) {
            return c;
        }

        return 0;
    }
}
