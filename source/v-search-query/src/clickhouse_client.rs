use clickhouse_rs::errors::Error;
use clickhouse_rs::Pool;
use futures::executor::block_on;
use serde::Serialize;
use std::str;
use std::time::*;
use url::Url;
use v_api::app::ResultCode;
use v_authorization::Access;
use v_az_lmdb::_authorize;
use v_module::module::{get_ticket_from_db, Module};
use v_module::ticket::Ticket;

#[derive(Serialize, Debug)]
pub struct QueryResult {
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

impl Default for QueryResult {
    fn default() -> Self {
        QueryResult {
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

pub fn select(module: &mut Module, pool: &mut Pool, ticket_id: &str, query: &str, top: i64, limit: i64, from: i64) -> QueryResult {
    let start = Instant::now();
    let mut res = QueryResult::default();

    let mut user_uri = "cfg:Guest".to_owned();
    if !ticket_id.is_empty() {
        let mut ticket = Ticket::default();
        get_ticket_from_db(&ticket_id, &mut ticket, module);
        if ticket.result == ResultCode::Ok {
            user_uri = ticket.user_uri;
        }
    }

    if let Err(e) = block_on(select_from_clickhouse(pool, &user_uri, query, top, limit, from, &mut res)) {
        error!("fail read from clickhouse: {:?}", e);
        res.result_code = ResultCode::InternalServerError
    }

    res.total_time = start.elapsed().as_millis() as i64;
    res.query_time = res.total_time - res.authorize_time;
    info!("result={:?}", res);

    res
}

async fn select_from_clickhouse(pool: &mut Pool, user_uri: &str, query: &str, top: i64, limit: i64, from: i64, out_res: &mut QueryResult) -> Result<(), Error> {
    let mut authorized_count = 0;
    let mut total_count = 0;

    if query.to_uppercase().contains("INSERT") || query.to_uppercase().contains("UPDATE") || query.to_uppercase().contains("DROP") {
        out_res.result_code = ResultCode::BadRequest;
        return Ok(());
    }

    let fq = format!("{} LIMIT {} OFFSET {}", query, 10000, from);

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
                    authorized_count += 1;

                    if authorized_count >= top {
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
    out_res.count = authorized_count;
    out_res.processed = total_count;
    out_res.cursor = from + total_count;
    out_res.authorize_time = out_res.authorize_time / 1000;

    Ok(())
}

pub fn connect_to_clickhouse(query_db_url: &str) -> Result<Pool, &'static str> {
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
