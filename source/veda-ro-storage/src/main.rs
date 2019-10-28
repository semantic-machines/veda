#[macro_use]
extern crate log;

use chrono::Local;
use env_logger::Builder;
use ini::Ini;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::io::Write;
use std::str;
use v_onto::individual::Individual;
use v_storage::storage::*;

/**
 * storage service
 *      protocol - nanomsg
 *		request: "T,url" - get ticket of url, response: JSON format
 *		request: "I,url" - get individual of url, response: JSON format
 *		request: "t,url" - get ticket of url, response: BINOBJ format
 *		request: "i,url" - get individual of url, response: BINOBJ format
 */

fn main() -> std::io::Result<()> {
    let env_var = "RUST_LOG";
    match std::env::var_os(env_var) {
        Some(val) => println!("use env var: {}: {:?}", env_var, val.to_str()),
        None => std::env::set_var(env_var, "info"),
    }

    Builder::new()
        .format(|buf, record| writeln!(buf, "{} [{}] - {}", Local::now().format("%Y-%m-%dT%H:%M:%S%.3f"), record.level(), record.args()))
        .filter(None, LevelFilter::Info)
        .init();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let ro_storage_url = section.get("ro_storage_url").expect("param [ro_storage_url] not found in veda.properties");

    let tarantool_addr = if let Some(p) = section.get("tarantool_url") {
        p.to_owned()
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    if !tarantool_addr.is_empty() {
        info!("tarantool addr={}", &tarantool_addr);
    }

    let mut storage: VStorage;
    if !tarantool_addr.is_empty() {
        storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
    } else {
        storage = VStorage::new_lmdb("./data", StorageMode::ReadOnly);
    }

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&ro_storage_url) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = req_prepare(&recv_msg, &mut storage);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }
}

fn req_prepare(request: &Message, storage: &mut VStorage) -> Message {
    if let Ok(s) = str::from_utf8(request.as_slice()) {
        let rel: Vec<&str> = s.split(',').collect();

        if rel.len() > 1 {
            let filters: Option<&[&str]> = if rel.len() > 2 {
                Some(&rel[2..])
            } else {
                None
            };

            match rel[0] {
                "T" => {
                    let mut indv = Individual::default();
                    if storage.get_individual_from_db(StorageId::Tickets, rel[1], &mut indv) {
                        indv.parse_all();
                        if let Some(s) = indv.get_obj().as_json().as_str() {
                            return Message::from(s.as_bytes());
                        }
                    } else {
                        return Message::from("[]".as_bytes());
                    }
                }
                "I" => {
                    let mut indv = Individual::default();
                    if storage.get_individual_from_db(StorageId::Individuals, rel[1], &mut indv) {
                        indv.parse_all();
                        if let Some(s) = indv.get_obj().as_json().as_str() {
                            return Message::from(s.as_bytes());
                        }
                    } else {
                        return Message::from("[]".as_bytes());
                    }
                }
                "F" => {
                    let mut indv = Individual::default();
                    if storage.get_individual_from_db(StorageId::Individuals, rel[1], &mut indv) {
                        if let Some(ffs) = filters {
                            for f in ffs {
                                indv.get_literals(f);
                            }
                        }
                        if let Some(s) = indv.get_obj().as_json().as_str() {
                            return Message::from(s.as_bytes());
                        }
                    } else {
                        return Message::from("[]".as_bytes());
                    }
                }
                "t" => {
                    let binobj = storage.get_raw_value(StorageId::Tickets, rel[1]);
                    if binobj.is_empty() {
                        return Message::from("[]".as_bytes());
                    }
                    return Message::from(binobj.as_slice());
                }
                "i" => {
                    let binobj = storage.get_raw_value(StorageId::Individuals, rel[1]);
                    if binobj.is_empty() {
                        return Message::from("[]".as_bytes());
                    }
                    return Message::from(binobj.as_slice());
                }
                _ => error!("invalid query: {}", s),
            }
        }
    }

    Message::default()
}
