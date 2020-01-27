/*
 * Ожидает и обрабатывает сеанс обмена данными с другими системами, является slave частью в протоколе связи
*/
#[macro_use]
extern crate log;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::io::Write;
use std::str::from_utf8;
use v_exim::*;
use v_module::module::*;
use v_onto::individual::{Individual, RawObj};
use v_onto::individual2msgpack::to_msgpack;
use v_queue::consumer::Consumer;
use v_queue::record::ErrorQueue;

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

    let param_name = "exim_url";
    let exim_slave_port = Module::get_property(param_name);
    if exim_slave_port.is_none() {
        error!("not found param {} in properties file", param_name);
        return Ok(());
    }

    let systicket;
    if let Ok(t) = module.get_sys_ticket_id() {
        systicket = t;
    } else {
        error!("fail get systicket");
        return Ok(());
    }

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&exim_slave_port.unwrap()) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

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

    loop {
        if let Ok(recv_msg) = server.recv() {
            let msg = recv_msg.to_vec();

            if msg.len() > 1 && msg[0] == b'?' {
                // this request changes from master
                if let Ok(remote_node_id) = from_utf8(&msg[2..msg.len()]) {
                    // читаем элемент очереди, создаем обьект и отправляем на server
                    let mut queue_consumer = Consumer::new("./data/out", remote_node_id, "extract").expect("!!!!!!!!! FAIL QUEUE");

                    if let Err(e) = queue_consumer.queue.get_info_of_part(queue_consumer.id, true) {
                        error!("get_info_of_part {}: {}", queue_consumer.id, e.as_str());
                    }

                    // пробуем взять из очереди заголовок сообщения
                    if queue_consumer.pop_header() {
                        let mut raw = RawObj::new(vec![0; (queue_consumer.header.msg_length) as usize]);

                        if let Err(e) = queue_consumer.pop_body(&mut raw.data) {
                            if e != ErrorQueue::FailReadTailMessage {
                                error!("get msg from queue: {}", e.as_str());
                            }
                        } else {
                            let indv = &mut Individual::new_raw(raw);
                            match create_out_obj(indv, remote_node_id) {
                                Ok(out_obj) => {
                                    let mut raw1: Vec<u8> = Vec::new();
                                    if to_msgpack(&out_obj, &mut raw1).is_ok() {
                                        info!("send {} to {}", out_obj.get_id(), remote_node_id);
                                        let req = Message::from(raw1.as_slice());

                                        if let Err(e) = server.send(req) {
                                            error!("fail send {:?}", e);
                                        } else {
                                            queue_consumer.commit_and_next();
                                        }
                                        continue;
                                    }
                                }
                                Err(e) => {
                                    error!("fail create out message {:?}", e);
                                }
                            }
                        }
                    }
                }

                let req = Message::from("[]".as_bytes());
                if let Err(e) = server.send(req) {
                    error!("fail send {:?}", e);
                }
            } else {
                let res = processing_message_contains_one_change(&node_id, msg, &systicket, &mut module);
                if let Err(e) = server.send(Message::from(enc_slave_resp(&res.0, res.1).as_bytes())) {
                    error!("fail send {:?}", e);
                }
            }

        //msg.clear();
        } else {
            error!("fail recv")
        }
    }
}
