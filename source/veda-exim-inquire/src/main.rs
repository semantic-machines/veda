/*
 * Инициирует и проводит сеансы обмена данными с внешними системами, является
 * master частью в протоколе связи. Знает о всех точках обмена
 * (load_linked_nodes)
 */
#[macro_use]
extern crate log;

use std::collections::HashMap;
use std::{thread, time};
use v_exim::configuration::Configuration;
use v_exim::*;
use v_module::module::*;
use v_queue::consumer::*;
use v_storage::storage::StorageMode;

fn main() -> std::io::Result<()> {
    init_log("EXIM_INQUIRE");

    let mut module = Module::new(StorageMode::ReadOnly, false);

    let sys_ticket;
    if let Ok(t) = module.get_sys_ticket_id() {
        sys_ticket = t;
    } else {
        error!("fail get system ticket");
        return Ok(());
    }

    // загрузка адресов связанных нод
    let mut node_upd_counter = 0;
    let mut link_node_addresses = HashMap::new();

    let mut node_id = get_db_id(&mut module);
    if node_id.is_none() {
        node_id = create_db_id(&mut module);
    }

    if node_id.is_none() {
        error!("fail create node_id");
        return Ok(());
    }
    let node_id = node_id.unwrap();
    info!("my node_id={}", node_id);

    load_linked_nodes(&mut module, &mut node_upd_counter, &mut link_node_addresses);

    loop {
        for (remote_node_id, remote_node_addr) in &link_node_addresses {
            info!("@A1");

            let mut queue_consumer = Consumer::new("./data/out", remote_node_id, "extract").expect("!!!!!!!!! FAIL QUEUE EXTRACT");

            let exim_resp_api = Configuration::new(remote_node_addr, "", "");

            send_changes_to_node(&mut queue_consumer, &exim_resp_api, remote_node_id);

            // request changes from slave node
            loop {
                /*
                                let req = Message::from(("?,".to_owned() + &node_id).as_bytes());
                                //info!("send request for changes to {}", remote_node_addr);
                                if let Err(e) = soc.send(req) {
                                    error!("fail send request to slave node, err={:?}", e);
                                    break;
                                }

                                // Wait for the response from the server (slave)
                                let wmsg = soc.recv();
                                if let Err(e) = wmsg {
                                    error!("fail recv from slave node, err={:?}", e);
                                    break;
                                }

                                let msg = wmsg.unwrap().to_vec();

                                if msg.len() == 2 && msg[0] == b'[' && msg[1] == b']' {
                                    // this empty result
                                    break;
                                }
                */
                //let res = processing_message_contains_one_change(&node_id,
                // msg, &sys_ticket, &mut module.api);
                // if res.1 != ExImCode::Ok {
                //    error!("fail accept changes, uri={}, err={:?}", res.0,
                // res.1);
                //}
            }
        }
        thread::sleep(time::Duration::from_millis(5000));
    }
}
