extern crate clickhouse_rs;
extern crate futures;
extern crate tokio;

use crate::common::QueryResult;

use clickhouse_rs::Pool;
use futures::Future;

use std::str;
use std::time::*;
use url::Url;
use v_api::app::{OptAuthorize, ResultCode};
use v_authorization::common::Access;
use v_az_lmdb::_authorize;

fn run<F, T, U>(future: F) -> Result<T, U>
where
    F: Future<Item = T, Error = U> + Send + 'static,
    T: Send + 'static,
    U: Send + 'static,
{
    let mut runtime = tokio::runtime::Runtime::new().unwrap();
    let result = runtime.block_on(future);
    runtime.shutdown_on_idle().wait().unwrap();
    result
}

pub struct CHClient {
    client: Pool,
    addr: String,
    is_ready: bool,
}

impl CHClient {
    pub fn new(client_addr: String) -> CHClient {
        CHClient {
            client: Pool::new(String::new()),
            addr: client_addr,
            is_ready: false,
        }
    }

    pub fn connect(&mut self) -> bool {
        info!("Configuration to connect to Clickhouse: {}", self.addr);
        match Url::parse(self.addr.as_ref()) {
            Ok(url) => {
                let host = url.host_str().unwrap_or("127.0.0.1");
                let port = url.port().unwrap_or(9000);
                let user = url.username();
                let pass = url.password().unwrap_or("123");
                let url = format!("tcp://{}:{}@{}:{}/", user, pass, host, port);
                info!("Trying to connect to Clickhouse, host: {}, port: {}, user: {}, password: {}", host, port, user, pass);
                info!("Connection url: {}", url);
                let pool = Pool::new(url);
                self.client = pool;
                self.is_ready = true;
                true
            }
            Err(e) => {
                error!("Invalid connection url, err={:?}", e);
                self.is_ready = false;
                false
            }
        }
    }

    pub fn select(&mut self, user_uri: &str, query: &str, top: i64, limit: i64, from: i64, op_auth: OptAuthorize) -> QueryResult {
        select_from_clickhouse(&mut self.client, user_uri.to_string(), query, top, limit, from, op_auth)

        //debug!("result={:?}", res);
    }
}

fn select_from_clickhouse(pool: &mut Pool, user_uri: String, query: &str, top: i64, limit: i64, from: i64, op_auth: OptAuthorize) -> QueryResult {
    let start = Instant::now();

    let mut out_res = QueryResult::default();

    let mut authorized_count = 0;
    let mut total_count = 0;

    if query.to_uppercase().split_whitespace().any(|x| x == "INSERT" || x == "UPDATE" || x == "DROP" || x == "DELETE") {
        out_res.result_code = ResultCode::BadRequest;
        return out_res;
    }

    let fq = format!("{} LIMIT {} OFFSET {}", query, limit, from);

    info!("query={}", fq);

    let done = pool.get_handle().and_then(|c| c.query(fq).fetch_all()).and_then(move |(_, block)| -> Result<QueryResult, clickhouse_rs::errors::Error> {
        let mut out_res = QueryResult::default();

        for row in block.rows() {
            total_count += 1;

            let id: String = row.get(row.name(0)?)?;

            if op_auth == OptAuthorize::YES {
                let start = Instant::now();

                match _authorize(&id, &user_uri, Access::CanRead as u8, true, None) {
                    Ok(res) => {
                        if res == Access::CanRead as u8 {
                            out_res.result.push(id);
                            authorized_count += 1;

                            if authorized_count >= top {
                                break;
                            }
                        }
                    }
                    Err(e) => error!("fail authorization {}, err={}", user_uri, e),
                }
                out_res.authorize_time += start.elapsed().as_micros() as i64;
            } else {
                out_res.result.push(id);
            }

            if total_count >= limit {
                break;
            }
        }
        out_res.result_code = ResultCode::Ok;
        out_res.estimated = from + total_count;
        out_res.count = authorized_count;
        out_res.processed = total_count;
        out_res.cursor = from + total_count;
        out_res.authorize_time /= 1000;

        Ok(out_res)
    });

    let ww1: Result<QueryResult, clickhouse_rs::errors::Error> = run(done);

    return match ww1 {
        Ok(mut res) => {
            res.total_time = start.elapsed().as_millis() as i64;
            res.query_time = res.total_time - res.authorize_time;
            res
        }
        Err(e) => {
            error!("fail read from clickhouse: {:?}", e);
            let mut res = QueryResult::default();
            res.result_code = ResultCode::InternalServerError;
            res
        }
    }
}
