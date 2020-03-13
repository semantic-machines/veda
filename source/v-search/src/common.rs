use serde::Serialize;
use v_api::app::ResultCode;

#[derive(Serialize, Debug)]
pub struct QueryResult {
    pub result: Vec<String>,
    pub count: i64,
    pub estimated: i64,
    pub processed: i64,
    pub cursor: i64,
    pub total_time: i64,
    pub query_time: i64,
    pub authorize_time: i64,
    pub result_code: ResultCode,
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
