#[macro_use]
extern crate log;

#[macro_use]
extern crate mysql;

use std::collections::HashMap;
use std::{thread, time};

use v_api::ResultCode;
use v_module::info::ModuleInfo;
use v_module::module::*;
use v_module::onto::load_onto;
use v_onto::individual::*;
use v_onto::onto::Onto;
use v_onto::resource::Resource;
use v_onto::datatype::DataType;
use v_queue::consumer::*;

pub struct Context{
    onto: Onto,
    conn: Connection,
    tables: HashMap<String, bool>,
}

pub struct Connection {
    opts: mysql::Opts,
    pool: mysql::Pool,
}

fn main() -> Result<(), i32> {
    init_log();

    let mut queue_consumer = Consumer::new("./data/queue", "fanout_sql", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let module_info = ModuleInfo::new("./data", "fanout_sql", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    let mut module = Module::default();

    if let Ok(conn) = connect_to_mysql(&mut module, -1, 30000) {
        if let Ok(tables) = read_tables(&conn) {
            let mut ctx = Context {
                onto: Onto::default(),
                conn,
                tables,
            };
            load_onto(&mut module.fts, &mut module.storage, &mut ctx.onto);
            info!("Ontology: {}", ctx.onto);
            module.listen_queue(
                &mut queue_consumer,
                &mut module_info.unwrap(),
                &mut ctx,
                &mut (void as fn(&mut Module, &mut Context)),
                &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual)),
                &mut (void as fn(&mut Module, &mut Context)),
            );
            return Ok(());
        }
    }
    Err(-1)
}

fn void(_module: &mut Module, _ctx: &mut Context) {}

fn prepare(module: &mut Module, module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return;
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let mut prev_state = Individual::default();
    let is_new = !get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if let Err(e) = module_info.put_info(op_id, op_id) {
        error!("fail write module_info, op_id={}, err={:?}", op_id, e)
    }

    if let Some(types) = new_state.get_literals("rdf:type") {
        for itype in types {
            if ctx.onto.is_some_entered(&itype, &["v-s:Exportable"]) {
                export(&mut new_state,&mut prev_state, is_new, module, ctx);
                break;
            }
        }
    }
}

fn export(new_state: &mut Individual, prev_state: &mut Individual, is_new: bool, module: &mut Module, ctx: &mut Context) {
    let uri = new_state.get_id();
    info!("<< Exporting {}", uri);

    let is_deleted = new_state.is_exists_bool("v-s:deleted", true);

    let actual_version = new_state.get_first_literal("v-s:actual_version").unwrap_or_default();

    if !actual_version.is_empty() && actual_version != new_state.get_id() {
        info!("new {}.v-s:actual_version {} != {}, ignore", new_state.get_id(), &actual_version, new_state.get_id());
        return;
    }
    let predicates = new_state.get_predicates();

    for predicate in predicates {
        if let Some(resources) = new_state.get_resources(&predicate) {
            if let Ok(resource) = resources.first() {
                if let Err(_) = check_create_property_table(&predicate, &resource, ctx) {
                    error!("Unable to create table for property: `{}`, export aborted for individual: `{}`", predicate, uri);
                    return;
                }
            }
        }
    }

    if let Ok(mut conn) = ctx.conn.pool.get_conn() {
        if let Ok(mut transaction) = conn.start_transaction(true, Option::from(mysql::IsolationLevel::ReadCommitted), Option::from(false)) {
            for predicate in predicates {
                if let Some(resources) = new_state.get_resources(&predicate) {
                    for resource in resources {
                        if resource.order == 0 {
                            let query = format!("DELETE FROM `{}` WHERE doc_id = `{}`", &predicate, new_state.get_id());
                            if let Err(e) = transaction.query(query) {
                                error!("Delete individual `{}` from property table `{}` failed", uri, predicate);
                                match transaction.rollback() {
                                    Ok(_) => {
                                        info!("Export individual `{}` transaction rollback SUCCESS", uri);
                                        return;
                                    },
                                    Err(_) => {
                                        error!("Export individual `{}` transaction rollback FAILED", uri);
                                        return;
                                    }
                                }
                            }
                        }

                    }
                } else {
                    continue;
                }
            }
        }
    }

    for predicate in predicates {
        if let Some(resources) = new_state.get_resources(&predicate) {
            for resource in resources {
                if let Ok(_) = check_create_property_table(&predicate, &resource, ctx) {
                    //info!("predicate: {}, type: {:#?}, order: {}, value: {:?}", predicate, resource.rtype, resource.order, resource.value)
                    if let Ok(mut conn) = ctx.conn.pool.get_conn() {
                        if let Ok(mut transaction) = conn.start_transaction(true, Option::from(mysql::IsolationLevel::ReadCommitted), Option::from(false)) {
                            let query = format!("DELETE FROM `{}` WHERE doc_id = `{}`", &predicate, new_state.get_id());
                            if let Err(e) = transaction.query(query) {
                                error!("{}", e);
                            }
                        }
                    }
                }
            }
        }
    }
    info!("Export done! >>");
}

fn check_create_property_table(property: &str, resource: &Resource, ctx: &mut Context) -> Result<(), &'static str> {
    if let Some(true) = ctx.tables.get(property) {
        Ok(())
    } else {
        if let Some(db) = ctx.conn.opts.get_db_name() {
            if let Ok(mut conn) = ctx.conn.pool.get_conn() {
                let mut sql_type = "";
                let mut sql_value_index = ", INDEX civ(`value`)";
                match &resource.rtype {
                    DataType::Boolean => sql_type = "BOOL",
                    DataType::Datetime => sql_type = "DATETIME",
                    DataType::Decimal => sql_type = "DECIMAL (14,4)",
                    DataType::Integer => sql_type = "INTEGER",
                    DataType::String => {
                        sql_type = "TEXT";
                        sql_value_index = "";
                    }
                    DataType::Uri => sql_type = "CHAR(128)",
                    _unsupported => error!("Unsupported property value type: {:#?}", _unsupported),
                }
                let query = format!(
                "CREATE TABLE `{}`.`{}` ( \
                `ID` BIGINT NOT NULL AUTO_INCREMENT, \
                `doc_id` CHAR(128) NOT NULL, \
                `doc_type` CHAR(128) NOT NULL, \
                `created` DATETIME NULL, \
                `value` {} NULL, \
                `lang` CHAR(2) NULL, \
                `deleted` BOOL NULL, \
                 PRIMARY KEY (`ID`), \
                 INDEX c1(`doc_id`), INDEX c2(`doc_type`), INDEX c3 (`created`), INDEX c4(`lang`) {} \
                ) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;",
                    db, property, sql_type, sql_value_index
                );
                info!("Query: {}", query);
                match conn.query(query) {
                    Ok(_) => {
                        info!("Table created: {}", property);
                        ctx.tables.insert(property.to_string(), true);
                        Ok(())
                    },
                    Err(e) => {
                        error!("Table create error: {}", e);
                        Err("Table create error")
                    }
                }
            } else {
                Err("No connection in pool")
            }
        } else {
            error!("No DB specified");
            Err("No DB specified")
        }
    }
}

fn read_tables(connection: &Connection) -> Result<HashMap<String, bool>, &'static str> {
    let mut tables: HashMap<String, bool> = HashMap::new();
    if let Some(db) = connection.opts.get_db_name() {
        if let Ok(result) = connection.pool.prep_exec("SELECT TABLE_NAME FROM information_schema.tables WHERE table_schema = :db;", params!{"db" => db}) {
            result.for_each(|row| {
                if let Some(name) = row.unwrap().get(0) {
                    tables.insert(name, true);
                }
            });
            info!("Tables {:#?}", tables);
            Ok(tables)
        } else {
            Err("Query error")
        }
    } else {
        Err("No DB specified")
    }
}

fn connect_to_mysql(module: &mut Module, tries: i64, timeout: u64) -> Result<Connection, &'static str> {
    if let Some(node) = module.get_individual("cfg:standart_node", &mut Individual::default()) {
        if let Some(v) = node.get_literals("v-s:push_individual_by_event") {
            for el in v {
                let mut connection = Individual::default();
                if module.storage.get_individual(&el, &mut connection) && !connection.is_exists_bool("v-s:deleted", true) {
                    if let Some(transport) = connection.get_first_literal("v-s:transport") {
                        if transport == "mysql" {
                            info!("Found configuration to connect to MySQL: {}", connection.get_id());
                            let host = connection.get_first_literal("v-s:host").unwrap_or_default();
                            let port = connection.get_first_integer("v-s:port").unwrap_or(3306) as u16;
                            let login = connection.get_first_literal("v-s:login").unwrap();
                            let pass = connection.get_first_literal("v-s:password").unwrap();
                            let db = connection.get_first_literal("v-s:sql_database").unwrap();
                            info!("Trying to connect to mysql, host: {}, port: {}, login: {}, pass: {}, db: {}", host, port, login, pass, db);
                            let mut builder = mysql::OptsBuilder::new();
                            builder.ip_or_hostname(Some(host))
                                .tcp_port(port)
                                .user(Some(login))
                                .pass(Some(pass))
                                .db_name(Some(db));
                            let opts: mysql::Opts = builder.into();
                            match mysql::Pool::new(opts.clone()) {
                                Ok(pool) => {
                                    info!("Connection to MySQL established successfully");
                                    let conn = Connection {
                                      opts,
                                      pool,
                                    };
                                    return Ok(conn)
                                },
                                Err(_) => {
                                    error!("Connection to MySQL failed");
                                    return Err("Connection to MySQL failed")
                                },
                            };
                        }
                    }
                }
            }
        }
    }
    if tries != 0 {
        let tries = tries - 1;
        thread::sleep(time::Duration::from_millis(timeout));
        error!("Configuration not found yet, retry.");
        connect_to_mysql(module, tries, timeout)
    } else {
        error!("Configuration to connect to mysql not found");
        Err("Configuration to connect to mysql not found")
    }
}
