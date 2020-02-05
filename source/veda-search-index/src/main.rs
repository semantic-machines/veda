#[macro_use]
extern crate log;

#[macro_use]
extern crate lazy_static;

use std::process;
use std::collections::HashMap;

use clickhouse_rs::{Pool, errors::Error, Block, ClientHandle};
use chrono::prelude::*;
use chrono_tz::Tz;
use std::time::Instant;

use regex::Regex;

use tokio;
use futures::executor::block_on;

use v_module::info::ModuleInfo;
use v_module::module::*;
use v_module::onto::load_onto;
use v_onto::individual::*;
use v_onto::onto::Onto;
use v_onto::resource::Value;
use v_onto::datatype::DataType;
use v_onto::datatype::Lang;
use v_queue::consumer::*;

type TypedBatch = HashMap<String, Batch>;
type Batch = Vec<BatchElement>;
type BatchElement = (String, Individual, Individual, bool);

const BATCH_SIZE: u32 = 100_000;
const BLOCK_LIMIT: usize = 10_000;

pub struct Stats {
    total_prepare_duration: usize,
    total_insert_duration: usize,
    total_rows: usize,
    started: Instant,
    last: Instant,
}

pub struct Context {
    onto: Onto,
    pool: Pool,
    db_columns: HashMap<String, String>,
    typed_batch: TypedBatch,
    stats: Stats,
}

enum ColumnData {
    Str(Vec<Vec<String>>),
    Date(Vec<Vec<DateTime<Tz>>>),
    Int(Vec<Vec<i64>>),
    Num(Vec<Vec<f64>>),
}

impl Context {

    pub fn add_to_typed_batch(&mut self, element: BatchElement) {
        let (class, _new_state, _prev_state, _is_new) = &element;
        if !self.typed_batch.contains_key(class) {
            let new_batch = Batch::new();
            self.typed_batch.insert(class.clone(), new_batch);
        }
        let batch = self.typed_batch.get_mut(class).unwrap();
        batch.push(element);
    }

    pub async fn process_typed_batch(&mut self) -> Result<(), Error> {
        if !self.typed_batch.is_empty() {
            let now = Instant::now();
            let client = &mut self.pool.get_handle().await?;
            let db_columns = &mut self.db_columns;
            let stats = &mut self.stats;
            for (class, batch) in self.typed_batch.iter_mut() {
                while batch.len() > BLOCK_LIMIT {
                    let mut slice = batch.drain(0..BLOCK_LIMIT).collect();
                    Context::process_batch(&class, &mut slice, client, db_columns, stats).await?;
                }
                Context::process_batch(&class, batch, client, db_columns, stats).await?;
            }
            self.typed_batch.clear();
            stats.last = Instant::now();
            info!("Batch processed in {} ms", now.elapsed().as_millis());
        }
        Ok(())
    }

    async fn process_batch(class: &str, batch: &mut Batch, client: &mut ClientHandle, db_columns: &mut HashMap<String, String>, stats: &mut Stats) -> Result<(), Error> {

        let now = Instant::now();

        info!("---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------");

        info!("Processing class batch: {}, count: {}", class, batch.len().clone());

        let mut id_column: Vec<String> = Vec::new();

        let mut sign_column: Vec<i8> = Vec::new();

        let mut version_column: Vec<u32> = Vec::new();

        let mut columns: HashMap<String, ColumnData> = HashMap::new();

        for element in batch {
            let (_, new_state, prev_state, is_new) = element;

            let id = new_state.get_id().to_string();
            let actual_version = new_state.get_first_literal("v-s:actualVersion").unwrap_or_default();
            if !actual_version.is_empty() && actual_version != id {
                info!("Skip not actual version. {}.v-s:actualVersion {} != {}", id, &actual_version, id);
                continue;
            }

            if !*is_new {
                Context::add_to_table(prev_state, -1, &mut id_column, &mut sign_column, &mut version_column, &mut columns, client, db_columns).await?;
            }
            Context::add_to_table(new_state, 1, &mut id_column, &mut sign_column, &mut version_column, &mut columns, client, db_columns).await?;
        }

        let rows= id_column.len();

        let block = Context::mk_block(id_column, sign_column, version_column, &mut columns);

        //info!("Block {:?}", block);

        info!("Block prepared in {} us", now.elapsed().as_micros());

        stats.total_prepare_duration += now.elapsed().as_millis() as usize;

        let now = Instant::now();

        client.insert("veda.individuals", block).await?;

        let mut insert_duration = now.elapsed().as_millis() as usize;
        if insert_duration == 0 {
            insert_duration = 1;
        }

        let cps = (rows * 1000 / insert_duration) as f64;

        info!("Block inserted successfully! Rows = {}, columns = {}, duration = {} ms, cps = {}", rows, columns.keys().len() + 2, insert_duration, cps);

        stats.total_insert_duration += insert_duration;

        stats.total_rows += rows;

        let total_cps = (stats.total_rows * 1000 / stats.total_insert_duration) as f64;

        let uptime = stats.started.elapsed();
        let uptime_ms = if uptime.as_millis() == 0 {
            1
        } else {
            uptime.as_millis()
        } as usize;

        let uptime_cps = (stats.total_rows * 1000 / uptime_ms) as f64;

        info!("Total rows inserted = {}, total prepare duration = {} ms, total insert duration = {} ms, avg. insert cps = {}, uptime = {}h {}m {}s, avg. uptime cps = {}", stats.total_rows, stats.total_prepare_duration, stats.total_insert_duration, total_cps, (uptime_ms / 1000) / 3600, (uptime_ms / 1000) % 3600 / 60, (uptime_ms / 1000) % 3600 % 60, uptime_cps);

        Ok(())
    }

    async fn add_to_table(
        individual: &mut Individual,
        sign: i8,
        id_column: &mut Vec<String>,
        sign_column: &mut Vec<i8>,
        version_column: &mut Vec<u32>,
        columns: &mut HashMap<String, ColumnData>,
        client: &mut ClientHandle,
        db_columns: &mut HashMap<String, String>
    )  -> Result<(), Error> {

        let rows = id_column.len();

        let id = individual.get_id().to_owned();

        let version = individual.get_first_integer("v-s:updateCounter").unwrap_or(0) as u32;

        id_column.push(id);

        version_column.push(version);

        sign_column.push(sign);

        for predicate in individual.get_predicates() {
            if let Some(resources) = individual.get_resources(&predicate) {
                lazy_static! {
                   static ref RE: Regex = Regex::new("[^a-zA-Z0-9]").unwrap();
                }
                let mut column_name = RE.replace_all(&predicate, "_").into_owned();
                match &resources[0].rtype {
                    DataType::Integer => {
                        column_name.push_str("_int");
                        let column_type = "Array(Int64)";
                        create_predicate_column(&column_name, &column_type, client, db_columns).await?;

                        let column_value: Vec<i64> = resources.iter().map(|resource| resource.get_int()).collect();

                        if !columns.contains_key(&column_name) {
                            let new_column = ColumnData::Int(Vec::new());
                            columns.insert(column_name.clone(), new_column);
                        }

                        let column_data = columns.get_mut(&column_name).unwrap();
                        if let ColumnData::Int(column) = column_data {
                            let column_size = column.len();
                            let mut empty = vec![vec![0]; rows - column_size];
                            column.append(&mut empty);
                            column.push(column_value);
                        }
                    },
                    DataType::String => {
                        column_name.push_str("_str");
                        let column_type = "Array(String)";
                        create_predicate_column(&column_name, &column_type, client, db_columns).await?;

                        let column_value: Vec<String> = resources.iter().map(|resource| {
                            let str_value = resource.get_str();
                            let lang = match resource.get_lang() {
                                Lang::NONE => String::from(""),
                                lang => format!("@{}", lang.to_string()),
                            };
                            format!("{}{}", str_value.replace("'", "\\'"), lang)
                        }).collect();

                        if !columns.contains_key(&column_name) {
                            let new_column = ColumnData::Str(Vec::new());
                            columns.insert(column_name.clone(), new_column);
                        }

                        let column_data = columns.get_mut(&column_name).unwrap();
                        if let ColumnData::Str(column) = column_data {
                            let column_size = column.len();
                            let mut empty = vec![vec!["".to_owned()]; rows - column_size];
                            column.append(&mut empty);
                            column.push(column_value);
                        }
                    },
                    DataType::Uri => {
                        column_name.push_str("_str");
                        let column_type = "Array(String)";
                        create_predicate_column(&column_name, &column_type, client, db_columns).await?;

                        let column_value: Vec<String> = resources.iter().map(|resource| resource.get_uri().to_string()).collect();

                        if !columns.contains_key(&column_name) {
                            let new_column = ColumnData::Str(Vec::new());
                            columns.insert(column_name.clone(), new_column);
                        }

                        let column_data = columns.get_mut(&column_name).unwrap();
                        if let ColumnData::Str(column) = column_data {
                            let column_size = column.len();
                            let mut empty = vec![vec!["".to_owned()]; rows - column_size];
                            column.append(&mut empty);
                            column.push(column_value);
                        }
                    },
                    DataType::Boolean => {
                        column_name.push_str("_int");
                        let column_type = "Array(Int64)";
                        create_predicate_column(&column_name, &column_type, client, db_columns).await?;

                        let column_value: Vec<i64> = resources.iter().map(|resource| {
                            match resource.value {
                                Value::Bool(true) => 1,
                                _ => 0
                            }
                        }).collect();

                        if !columns.contains_key(&column_name) {
                            let new_column = ColumnData::Int(Vec::new());
                            columns.insert(column_name.clone(), new_column);
                        }

                        let column_data = columns.get_mut(&column_name).unwrap();
                        if let ColumnData::Int(column) = column_data {
                            let column_size = column.len();
                            let mut empty = vec![vec![0]; rows - column_size];
                            column.append(&mut empty);
                            column.push(column_value);
                        }
                    },
                    DataType::Decimal => {
                        column_name.push_str("_num");
                        let column_type = "Array(Float64)";
                        create_predicate_column(&column_name, &column_type, client, db_columns).await?;

                        let column_value: Vec<f64> = resources.iter().map(|resource| resource.get_float()).collect();

                        if !columns.contains_key(&column_name) {
                            let new_column = ColumnData::Num(Vec::new());
                            columns.insert(column_name.clone(), new_column);
                        }

                        let column_data = columns.get_mut(&column_name).unwrap();
                        if let ColumnData::Num(column) = column_data {
                            let column_size = column.len();
                            let mut empty = vec![vec![0 as f64]; rows - column_size];
                            column.append(&mut empty);
                            column.push(column_value);
                        }
                    },
                    DataType::Datetime => {
                        column_name.push_str("_date");
                        let column_type = "Array(DateTime)".to_string();
                        create_predicate_column(&column_name, &column_type, client, db_columns).await?;

                        let column_value: Vec<DateTime<Tz>> = resources.iter().map(|resource| Tz::UTC.timestamp(resource.get_datetime(), 0)).collect();

                        if !columns.contains_key(&column_name) {
                            let new_column = ColumnData::Date(Vec::new());
                            columns.insert(column_name.clone(), new_column);
                        }

                        let column_data = columns.get_mut(&column_name).unwrap();
                        if let ColumnData::Date(column) = column_data {
                            let column_size = column.len();
                            let mut empty = vec![vec![Tz::UTC.timestamp(0, 0)]; rows - column_size];
                            column.append(&mut empty);
                            column.push(column_value);
                        }
                    },
                    _ => {
                        error!("Value type is not supported");
                    }
                }
            }
        }
        Ok(())
    }

    fn mk_block(
        id_column: Vec<String>,
        sign_column: Vec<i8>,
        version_column: Vec<u32>,
        columns: &mut HashMap<String, ColumnData>
    ) -> Block {

        let rows = id_column.len();

        let mut block = Block::new()
            .column("id", id_column)
            .column("sign", sign_column)
            .column("version", version_column);

        for (column_name, column_data) in columns.iter_mut() {
            if let ColumnData::Int(column) = column_data {
                let column_size = column.len();
                let mut empty = vec![vec![0]; rows - column_size];
                column.append(&mut empty);
                //info!("column: {}, size: {}, {:?}", column_name, column.len(), column);
                block = block.column(&column_name, column.to_owned());
            }
            if let ColumnData::Str(column) = column_data {
                let column_size = column.len();
                let mut empty = vec![vec!["".to_string()]; rows - column_size];
                column.append(&mut empty);
                //info!("column: {}, size: {}, {:?}", column_name, column.len(), column);
                block = block.column(&column_name, column.to_owned());
            }
            if let ColumnData::Num(column) = column_data {
                let column_size = column.len();
                let mut empty = vec![vec![0 as f64]; rows - column_size];
                column.append(&mut empty);
                //info!("column: {}, size: {}, {:?}", column_name, column.len(), column);
                block = block.column(&column_name, column.to_owned());
            }
            if let ColumnData::Date(column) = column_data {
                let column_size = column.len();
                let mut empty = vec![vec![Tz::UTC.timestamp(0, 0)]; rows - column_size];
                column.append(&mut empty);
                //info!("column: {}, size: {}, {:?}", column_name, column.len(), column);
                block = block.column(&column_name, column.to_owned());
            }
        }
        block
    }

}

#[tokio::main]
async fn main() ->  Result<(), Error> {
    init_log();

    //return test().await;

    if get_info_of_module("input-onto").unwrap_or((0, 0)).0 == 0 {
        wait_module("fulltext_indexer", wait_load_ontology());
    }

    let mut queue_consumer = Consumer::new("./data/queue", "search_index", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    let module_info = ModuleInfo::new("./data", "search_index", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        process::exit(101);
    }
    let mut module = Module::default();

    let mut pool = match connect_to_clickhouse(&mut module) {
        Err(e) => {
            error!("Failed to connect to clickhouse: {}", e);
            process::exit(101)
        },
        Ok(pool) => pool,
    };

    init_clickhouse(&mut pool).await?;

    let db_columns = read_columns(&mut pool).await?;

    info!("Columns: {:?}", db_columns);

    let typed_batch: TypedBatch = HashMap::new();
    let stats = Stats {
        total_prepare_duration: 0,
        total_insert_duration: 0,
        total_rows: 0,
        started: Instant::now(),
        last: Instant::now(),
    };

    let mut ctx = Context {
        onto: Onto::default(),
        pool,
        db_columns,
        typed_batch,
        stats
    };

    load_onto(&mut module.fts, &mut module.storage, &mut ctx.onto);

    info!("Rusty search-index: start listening to queue");

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before as fn(&mut Module, &mut Context, u32) -> Option<u32>),
        &mut (process as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual) -> Result<bool, PrepareError>),
        &mut (after as fn(&mut Module, &mut Context, u32) -> bool),
    );
    Ok(())
}

fn before(_module: &mut Module, _ctx: &mut Context, _batch_size: u32) -> Option<u32> {
    Some(BATCH_SIZE)
}

fn after(_module: &mut Module, ctx: &mut Context, _processed_batch_size: u32) -> bool {
    if let Err(e) = block_on(ctx.process_typed_batch()) {
        error!("Error processing batch: {}", e);
        process::exit(101);
    }
    true
}

fn process(_module: &mut Module, module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) -> Result<bool, PrepareError> {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("Queue element cmd is none. Skip element.");
        return Ok(false);
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();
    if let Err(e) = module_info.put_info(op_id, op_id) {
        error!("Failed to write module_info, op_id={}, err={:?}", op_id, e);
    }

    let mut prev_state = Individual::default();
    let is_new = !get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if let Some(resources) = new_state.get_resources("rdf:type") {
        if let Some(class) = resources.get(0) {
            if let Value::Uri(class) = &class.value {
                ctx.add_to_typed_batch((class.into(), new_state, prev_state, is_new));
            };
        };
    }

    Ok(false)
}

async fn create_predicate_column(column_name: &str, column_type: &str, client: &mut ClientHandle, db_columns: &mut HashMap<String, String>) -> Result<(), Error> {
    if let Some(_) = db_columns.get(column_name) {
        return Ok(());
    }
    let query = format!("ALTER TABLE veda.individuals ADD COLUMN IF NOT EXISTS `{}` {}", column_name, column_type);
    client.execute(query).await?;
    db_columns.insert(column_name.to_string(), column_type.to_string());
    Ok(())
}

fn connect_to_clickhouse(module: &mut Module) -> Result<Pool, &'static str> {
    if let Some(node) = module.get_individual("cfg:standart_node", &mut Individual::default()) {
        if let Some(v) = node.get_literals("v-s:push_individual_by_event") {
            for el in v {
                let mut connection = Individual::default();
                if module.storage.get_individual(&el, &mut connection) && !connection.is_exists_bool("v-s:deleted", true) {
                    if let Some(transport) = connection.get_first_literal("v-s:transport") {
                        if transport == "clickhouse" {
                            info!("Found configuration to connect to Clickhouse: {}", connection.get_id());
                            let host = connection.get_first_literal("v-s:host").unwrap_or(String::from("127.0.0.1"));
                            let port = connection.get_first_integer("v-s:port").unwrap_or(9000) as u16;
                            let user = connection.get_first_literal("v-s:login").unwrap_or(String::from("default"));
                            let pass = connection.get_first_literal("v-s:password").unwrap_or(String::from(""));
                            let url = format!("tcp://{}:{}@{}:{}/", user, pass, host, port);
                            info!("Trying to connect to Clickhouse, host: {}, port: {}, user: {}, password: {}", host, port, user, pass);
                            info!("Connection url: {}", url);
                            let pool = Pool::new(url);
                            return Ok(pool);
                        }
                    }
                }
            }
        }
    }
    Err("Configuration to connect to Clickhouse not found")
}

async fn init_clickhouse(pool: &mut Pool) -> Result<(), Error> {
    let init_veda_db = "CREATE DATABASE IF NOT EXISTS veda";
    let init_individuals_table = r"
        CREATE TABLE IF NOT EXISTS veda.individuals (
            id String,
            sign Int8 DEFAULT 1,
            version UInt32,
            `rdf_type_str` Array(String),
            `v_s_created_date` Array(DateTime) DEFAULT [toDateTime(0)]
        )
        ENGINE = VersionedCollapsingMergeTree(sign, version)
        ORDER BY (`rdf_type_str`[1], `v_s_created_date`[1], id)
        PARTITION BY (`rdf_type_str`[1])
    ";
    let mut client = pool.get_handle().await?;
    client.execute(init_veda_db).await?;
    client.execute(init_individuals_table).await?;
    Ok(())
}

async fn read_columns(pool: &mut Pool) -> Result<HashMap<String, String>, Error> {
    let read_columns = "DESCRIBE veda.individuals";
    let mut columns: HashMap<String, String> = HashMap::new();
    let mut client = pool.get_handle().await?;
    let block = client.query(read_columns).fetch_all().await?;
    for row in block.rows() {
        let name: String      = row.get("name")?;
        let data_type: String = row.get("type")?;
        columns.insert(name, data_type);
    }
    Ok(columns)
}

////////////////

/*
#[cfg(test)]
mod test {

    use super::*;
    use clickhouse_rs::types::Value as clickValue;
    use std::sync::Arc;

    #[test]
    async fn test() -> Result<(), Error> {
        let url = format!("tcp://default:cyqqxc@127.0.0.1:9000/");
        let pool = Pool::new(url);

        let ddl = r"
        CREATE TABLE IF NOT EXISTS test (
            id  UInt32,
            amount Array(UInt32),
            name Array(String)
        ) Engine = MergeTree() order by id";

        let mut block_rows = Block::new();
        block_rows.push(vec![
            (String::from("id"), clickValue::UInt32(11)),
            (String::from("amount"), clickValue::Array(&types::SqlType::UInt32, Arc::new(vec![clickValue::UInt32(11)]))),
            //(String::from("name"), clickValue::String(Arc::new(String::from("aaa").into_bytes())))
            (String::from("name"), clickValue::Array(&types::SqlType::String, Arc::new(vec![clickValue::String(Arc::new(String::from("11").into_bytes()))])))
        ])?;
        block_rows.push(vec![
            (String::from("id"), clickValue::UInt32(22)),
            (String::from("amount"), clickValue::Array(&types::SqlType::UInt32, Arc::new(vec![clickValue::UInt32(22)]))),
            //(String::from("name"), clickValue::String(Arc::new(String::from("aaa").into_bytes())))
            (String::from("name"), clickValue::Array(&types::SqlType::String, Arc::new(vec![clickValue::String(Arc::new(String::from("22").into_bytes()))])))
        ])?;
        block_rows.push(vec![
            (String::from("id"), clickValue::UInt32(33)),
            (String::from("amount"), clickValue::Array(&types::SqlType::UInt32, Arc::new(vec![clickValue::UInt32(33)]))),
            //(String::from("name"), clickValue::String(Arc::new(String::from("aaa").into_bytes())))
            (String::from("name"), clickValue::Array(&types::SqlType::String, Arc::new(vec![clickValue::String(Arc::new(String::from("33").into_bytes()))])))
        ])?;

        let mut column1: Vec<u32> = Vec::new();
        column1.push(1);
        column1.push(2);
        column1.push(3);

        let mut column2: Vec<Vec<u32>> = Vec::new();
        column2.push(vec![1]);
        column2.push(vec![2]);
        column2.push(vec![3]);

        let mut column3: Vec<Vec<String>> = Vec::new();
        column3.push(vec!["1".to_string()]);
        column3.push(vec!["2".to_string()]);
        column3.push(vec!["3".to_string()]);

        let block_cols = Block::new()
            .column("id", column1)
            .column("amount", column2)
            .column("name", column3);

        let mut client = pool.get_handle().await?;
        client.execute(ddl).await?;
        client.insert("test", block_rows).await?;
        client.insert("test", block_cols).await?;

        let block = client.query("SELECT * FROM test").fetch_all().await?;

        for row in block.rows() {
            let id: u32             = row.get("id")?;
            let amount: Vec<u32>    = row.get("amount")?;
            let name: Vec<&str>     = row.get("name")?;
            info!("Found payment {}: {:?} {:?}", id, amount, name);
        }

        Ok(())
    }
}*/
