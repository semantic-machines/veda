#[macro_use]
extern crate log;

use nng::{Message, Protocol, Socket};
use serde_json::Value;
use std::{thread, time};

pub struct FTQuery {
    ticket: String,
    user: String,
    query: String,
    sort: String,
    databases: String,
    reopen: bool,
    top: i32,
    limit: i32,
    from: i32,
}

pub struct FTResult {
    pub result_code: i32,
    pub result: Vec<String>,
    pub count: i64,
    pub estimated: u64,
    pub processed: u64,
    pub cursor: u64,
}

impl Default for FTResult {
    fn default() -> FTResult {
        FTResult {
            result_code: 0,
            result: Vec::new(),
            count: 0,
            estimated: 0,
            processed: 0,
            cursor: 0,
        }
    }
}

impl FTQuery {
    pub fn new_with_user(user: &str, query: &str) -> FTQuery {
        FTQuery {
            ticket: "".to_owned(),
            user: user.to_owned(),
            query: query.to_owned(),
            sort: "".to_owned(),
            databases: "".to_owned(),
            reopen: false,
            top: 10000,
            limit: 10000,
            from: 0,
        }
    }

    pub fn new_with_ticket(ticket: &str, query: &str) -> FTQuery {
        FTQuery {
            ticket: ticket.to_owned(),
            user: "".to_owned(),
            query: query.to_owned(),
            sort: "".to_owned(),
            databases: "".to_owned(),
            reopen: false,
            top: 10000,
            limit: 10000,
            from: 0,
        }
    }

    pub fn as_string(&self) -> String {
        let mut s = String::new();

        if self.ticket.is_empty() {
            s.push_str("[\"UU=");
            s.push_str(&self.user);
        } else {
            s.push_str("[\"");
            s.push_str(&self.ticket);
        }

        s.push_str("\",\"");
        s.push_str(&self.query);
        s.push_str("\",\"");
        s.push_str(&self.sort);
        s.push_str("\",\"");
        s.push_str(&self.databases);
        s.push_str("\",");
        s.push_str(&self.reopen.to_string());
        s.push(',');
        s.push_str(&self.top.to_string());
        s.push(',');
        s.push_str(&self.limit.to_string());
        s.push(',');
        s.push_str(&self.from.to_string());
        s.push(']');

        s
    }
}

pub struct FTClient {
    client: Socket,
    addr: String,
    is_ready: bool,
}

impl FTClient {
    pub fn new(_ro_client_addr: String) -> FTClient {
        FTClient {
            client: Socket::new(Protocol::Req0).unwrap(),
            addr: _ro_client_addr,
            is_ready: false,
        }
    }

    pub fn connect(&mut self) -> bool {
        if let Err(e) = self.client.dial(self.addr.as_str()) {
            error!("ft-client:fail dial to ft-service, [{}], err={}", self.addr, e);
        } else {
            info!("sucess connect to ft-service, [{}]", self.addr);
            self.is_ready = true;
        }
        self.is_ready
    }

    pub fn query(&mut self, query: FTQuery) -> FTResult {
        let mut res = FTResult::default();

        if !self.is_ready {
            while !self.connect() {
                thread::sleep(time::Duration::from_millis(3000));
            }
        }

        if !self.is_ready {
            res.result_code = 474;
            return res;
        }

        let req = Message::from(query.as_string().as_bytes());

        if let Err(e) = self.client.send(req) {
            error!("fail send to search module, err={:?}", e);
            res.result_code = 474;
            return res;
        }

        // Wait for the response from the server.
        let wmsg = self.client.recv();

        if let Err(e) = wmsg {
            error!("fail recv from search module, err={:?}", e);
            res.result_code = 474;
            return res;
        }

        let msg = wmsg.unwrap();

        let reply = String::from_utf8_lossy(&msg);

        let v: Value = if let Ok(v) = serde_json::from_str(&reply) {
            v
        } else {
            Value::Null
        };

        res.result_code = v["result_code"].as_i64().unwrap_or_default() as i32;

        if res.result_code == 200 {
            let jarray: &Vec<_> = &v["result"].as_array().expect("array");
            res.result = jarray.iter().map(|v| v.as_str().unwrap_or_default().to_owned()).collect();

            res.count = v["count"].as_i64().unwrap_or_default();
            res.estimated = v["estimated"].as_u64().unwrap_or_default();
            res.processed = v["processed"].as_u64().unwrap_or_default();
            res.cursor = v["cursor"].as_u64().unwrap_or_default();
        }

        //info!("msg={}", v);
        res
    }
}
