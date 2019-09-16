#[macro_use]
extern crate log;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::io::Write;
use v_exim::*;
use v_module::module::*;

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

    let mut server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&exim_slave_port.unwrap()) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let msg = recv_msg.to_vec();

            if msg.len() > 1 && msg[0] == b'?' {
                let node_id: &[u8] = &msg[2..msg.len()];

            //""
            } else {
                let res = processing_message_contains_changes(msg, &systicket, &mut module);
                if let Err(e) = server.send(Message::from(enc_slave_resp(&res.0, res.1).as_ref())) {
                    error!("fail send {:?}", e);
                }
            }

        //msg.clear();
        } else {
            error!("fail recv")
        }
    }
}
