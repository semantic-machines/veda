use crate::common::{FTQuery, QueryResult};
use nng::{Message, Protocol, Socket};
use serde_json::Value;
use std::{thread, time};
use v_api::app::ResultCode;

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
            info!("success connect to ft-service, [{}]", self.addr);
            self.is_ready = true;
        }
        self.is_ready
    }

    pub fn query(&mut self, query: FTQuery) -> QueryResult {
        let mut res = QueryResult::default();

        if !self.is_ready {
            while !self.connect() {
                error!("not ready, sleep...");
                thread::sleep(time::Duration::from_millis(3000));
            }
        }

        if !self.is_ready {
            res.result_code = ResultCode::NotReady;
            return res;
        }

        let req = Message::from(query.as_string().as_bytes());

        if let Err(e) = self.client.send(req) {
            error!("fail send to search module, err={:?}", e);
            res.result_code = ResultCode::NotReady;
            return res;
        }

        // Wait for the response from the server.
        let wmsg = self.client.recv();

        if let Err(e) = wmsg {
            error!("fail recv from search module, err={:?}", e);
            res.result_code = ResultCode::NotReady;
            return res;
        }

        let msg = wmsg.unwrap();

        let reply = String::from_utf8_lossy(&msg);

        let v: Value = if let Ok(v) = serde_json::from_str(&reply) {
            v
        } else {
            Value::Null
        };

        res.result_code = ResultCode::from_i64(v["result_code"].as_i64().unwrap_or_default());

        if res.result_code == ResultCode::Ok {
            let jarray: &Vec<_> = &v["result"].as_array().expect("array");
            res.result = jarray.iter().map(|v| v.as_str().unwrap_or_default().to_owned()).collect();

            res.count = v["count"].as_i64().unwrap_or_default();
            res.estimated = v["estimated"].as_i64().unwrap_or_default();
            res.processed = v["processed"].as_i64().unwrap_or_default();
            res.cursor = v["cursor"].as_i64().unwrap_or_default();
        }

        //info!("msg={}", v);
        res
    }
}
