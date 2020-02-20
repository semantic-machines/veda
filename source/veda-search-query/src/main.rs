#[macro_use]
extern crate log;

use clickhouse_rs::errors::Error;
use clickhouse_rs::Pool;
use futures::executor::block_on;
use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde::Serialize;
use serde_json::value::Value as JSONValue;
use std::time::*;
use std::{str, thread};
use url::Url;
use v_api::app::ResultCode;
use v_authorization::Access;
use v_az_lmdb::_authorize;
use v_module::module::{get_ticket_from_db, init_log, Module};
use v_module::ticket::Ticket;

#[derive(Serialize)]
struct SearchResult {
    result: Vec<String>,
    count: i64,
    estimated: i64,
    processed: i64,
    cursor: i64,
    total_time: i64,
    query_time: i64,
    authorize_time: i64,
    result_code: ResultCode,
}

impl Default for SearchResult {
    fn default() -> Self {
        SearchResult {
            result: vec![],
            count: 0,
            estimated: 0,
            processed: 0,
            cursor: 0,
            total_time: 0,
            query_time: 0,
            authorize_time: 0,
            result_code: ResultCode::NotReady,
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    init_log();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");
    let query_search_db = section.get("query_search_db").expect("param [query_search_db_url] not found in veda.properties");

    let query_url = section.get("search_query_url").expect("param [search_query_url] not found in veda.properties");

    let mut module = Module::default();

    let mut pool;

    loop {
        pool = match connect_to_clickhouse(query_search_db) {
            Err(e) => {
                error!("Failed to connect to clickhouse: {}", e);
                error!("sleep and repeate...");
                thread::sleep(Duration::from_millis(10000));
                continue;
            }
            Ok(pool) => pool,
        };
        break;
    }

    let server = Socket::new(Protocol::Rep0).unwrap();
    if let Err(e) = server.listen(&query_url) {
        error!("fail listen {}, {:?}", query_url, e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let out_msg = req_prepare(&mut module, &recv_msg, &mut pool);
            if let Err(e) = server.send(out_msg) {
                error!("fail send answer, err={:?}", e);
            }
        }
    }
}

const TICKET: usize = 0;
const QUERY: usize = 1;
const TOP: usize = 5;
const LIMIT: usize = 6;
const FROM: usize = 7;

fn req_prepare(module: &mut Module, request: &Message, pool: &mut Pool) -> Message {
    if let Ok(s) = str::from_utf8(request.as_slice()) {
        let v: JSONValue = if let Ok(v) = serde_json::from_slice(s.as_bytes()) {
            v
        } else {
            JSONValue::Null
        };

        if let Some(a) = v.as_array() {
            let ticket = a.get(TICKET).unwrap().as_str().unwrap_or_default();
            let query = a.get(QUERY).unwrap().as_str().unwrap_or_default();

            let top = a.get(TOP).unwrap().as_i64().unwrap_or_default();
            let limit = a.get(LIMIT).unwrap().as_i64().unwrap_or_default();
            let from = a.get(FROM).unwrap().as_i64().unwrap_or_default();

            let res = select(module, pool, ticket, query, top, limit, from);

            if let Ok(s) = serde_json::to_string(&res) {
                return Message::from(s.as_bytes());
            }
        }
    }
    return Message::from("[]".as_bytes());
}

fn select(module: &mut Module, pool: &mut Pool, ticket_id: &str, query: &str, top: i64, limit: i64, from: i64) -> SearchResult {
    let start = Instant::now();
    let mut res = SearchResult::default();

    let mut user_uri = "cfg:Guest".to_owned();
    if !ticket_id.is_empty() {
        let mut ticket = Ticket::default();
        get_ticket_from_db(&ticket_id, &mut ticket, module);
        if ticket.result == ResultCode::Ok {
            user_uri = ticket.user_uri;
        }
    }

    if let Err(e) = block_on(select_to_ch(pool, &user_uri, query, top, limit, from, &mut res)) {
        error!("fail read from clickhouse: {:?}", e);
        res.result_code = ResultCode::InternalServerError
    }

    res.total_time = start.elapsed().as_millis() as i64;

    res
}

async fn select_to_ch(pool: &mut Pool, user_uri: &str, query: &str, top: i64, limit: i64, from: i64, out_res: &mut SearchResult) -> Result<(), Error> {
    let mut authorize_count = 0;
    let mut total_count = 0;

    if query.to_uppercase().contains("INSERT") || query.to_uppercase().contains("UPDATE") || query.to_uppercase().contains("DROP") {
        out_res.result_code = ResultCode::BadRequest;
        return Ok(());
    }

    let fq = format!("{} LIMIT {} OFFSET {}", query, limit, from);

    info!("query={}", fq);

    let mut client = pool.get_handle().await?;
    let block = client.query(fq).fetch_all().await?;
    for row in block.rows() {
        total_count += 1;
        if total_count >= limit {
            break;
        }

        let id: String = row.get(row.name(0)?)?;

        let start = Instant::now();

        match _authorize(&id, user_uri, Access::CanRead as u8, true, None) {
            Ok(res) => {
                if res == Access::CanRead as u8 {
                    out_res.result.push(id);
                    authorize_count += 1;

                    if authorize_count >= top {
                        break;
                    }
                }
            }
            Err(e) => error!("fail authorization {}, err={}", user_uri, e),
        }
        out_res.authorize_time += start.elapsed().as_micros() as i64;
    }

    out_res.result_code = ResultCode::Ok;

    out_res.estimated = block.row_count() as i64;
    out_res.count = total_count;
    out_res.processed = authorize_count;
    out_res.authorize_time = out_res.authorize_time / 1000;

    Ok(())
}

fn connect_to_clickhouse(query_db_url: &str) -> Result<Pool, &'static str> {
    info!("Configuration to connect to Clickhouse: {}", query_db_url);
    match Url::parse(query_db_url) {
        Ok(url) => {
            let host = url.host_str().unwrap_or("127.0.0.1");
            let port = url.port().unwrap_or(9000);
            let user = url.username();
            let pass = url.password().unwrap_or("123");
            let url = format!("tcp://{}:{}@{}:{}/", user, pass, host, port);
            info!("Trying to connect to Clickhouse, host: {}, port: {}, user: {}, password: {}", host, port, user, pass);
            info!("Connection url: {}", url);
            let pool = Pool::new(url);
            return Ok(pool);
        }
        Err(e) => {
            error!("{:?}", e);
            return Err("Invalid connection url");
        }
    }
}
