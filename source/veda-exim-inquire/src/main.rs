#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::collections::HashMap;
use std::io::Write;
use std::{thread, time};
use v_exim::*;
use v_module::module::*;
use v_queue::consumer::*;

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

    let mut module = Module::default();

    let systicket;
    if let Ok(t) = module.get_sys_ticket_id() {
        systicket = t;
    } else {
        error!("fail get systicket");
        return Ok(());
    }

    // загрузка адресов связанных нод
    let mut node_upd_counter = 0;
    let mut link_node_addresses = HashMap::new();

    get_linked_nodes(&mut module, &mut node_upd_counter, &mut link_node_addresses);

    //    let mut total_prepared_count: u64 = 0;

    loop {
        for (node_id, node_addr) in &link_node_addresses {
            let mut queue_consumer = Consumer::new("./data/out", node_id, "extract").expect("!!!!!!!!! FAIL QUEUE");

            let mut soc = Socket::new(Protocol::Req0).unwrap();

            if let Err(e) = soc.dial(node_addr) {
                error!("fail connect to, {} {}, err={}", node_id, node_addr, e);
                continue;
            }
            debug!("success connect to, {} {}", node_id, node_addr);

            processing_consumer_of_node(&mut queue_consumer, &mut soc, node_id, node_addr);

            // request changes from slave node
            loop {
                let req = Message::from(("?,".to_owned() + &node_id).as_ref());
                info!("send request for changes to {}", node_addr);
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

                if msg.len() > 2 && msg[0] == b'[' && msg[1] == b']' {
                    // this empty result
                    break;
                }

                let res = processing_message_contains_changes(msg, &systicket, &mut module);
                if res.1 != ExImCode::Ok {
                    error!("fail accept changes, uri={}, err={:?}", res.0, res.1);
                }
            }
        }
        thread::sleep(time::Duration::from_millis(5000));
    }
}

