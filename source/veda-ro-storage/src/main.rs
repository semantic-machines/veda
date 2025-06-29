#[macro_use]
extern crate log;

use nng::{Message, Protocol, Socket};
use std::str;
use v_common::module::module_impl::init_log;
use v_common::module::module_impl::Module;
use v_common::module::veda_backend::get_storage_use_prop;
use v_individual_model::onto::individual::Individual;
use v_common::storage::common::{StorageId, StorageMode, VStorage};
use v_common::v_api::obj::ResultCode;

/**
 * storage service
 *      protocol - nanomsg
 *    request: "T,url" - get ticket of url, response: JSON format
 *    request: "I,url" - get individual of url, response: JSON format
 *    request: "t,url" - get ticket of url, response: BINOBJ format
 *    request: "i,url" - get individual of url, response: BINOBJ format
 */

fn main() -> std::io::Result<()> {
    init_log("RO_STORAGE");

    let ro_storage_url = Module::get_property::<String>("ro_storage_url").expect("param [ro_storage_url] not found in veda.properties");
    let mut storage = get_storage_use_prop(StorageMode::ReadOnly);

    info!("total count: {}", storage.count(StorageId::Individuals));

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&ro_storage_url) {
        error!("failed to listen, err = {:?}", e);
        return Ok(());
    }

    const NUMBER_OF_OPERATIONS_BEFORE_STORAGE_RECONNECTION: i32 = 10000;

    let mut readed_count = 0;
    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = req_prepare(&recv_msg, &mut storage);
            if let Err(e) = server.send(res) {
                error!("failed to send reply, err = {:?}", e);
            }
            readed_count += 1;
            if readed_count % NUMBER_OF_OPERATIONS_BEFORE_STORAGE_RECONNECTION == 0 {
                storage = get_storage_use_prop(StorageMode::ReadOnly);
                info!("reconnect to storage");
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

            let db_id = match rel[0] {
                "T" | "t" => StorageId::Tickets,
                "I" | "i" => StorageId::Individuals,
                _ => StorageId::Individuals,
            };

            match rel[0] {
                "T" | "I" => {
                    let mut indv = Individual::default();
                    if storage.get_individual_from_db(db_id, rel[1], &mut indv) == ResultCode::Ok {
                        indv.parse_all();
                        return Message::from(indv.get_obj().as_json().to_string().as_bytes());
                    } else {
                        return Message::from("[]".as_bytes());
                    }
                }
                "F" => {
                    let mut indv = Individual::default();
                    if storage.get_individual_from_db(StorageId::Individuals, rel[1], &mut indv) == ResultCode::Ok {
                        if let Some(ffs) = filters {
                            for f in ffs {
                                indv.get_literals(f);
                            }
                        }
                        return Message::from(indv.get_obj().as_json().to_string().as_bytes());
                    } else {
                        return Message::from("[]".as_bytes());
                    }
                }
                "t" | "i" => {
                    let binobj = storage.get_raw_value(db_id, rel[1]);
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
