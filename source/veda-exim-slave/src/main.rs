#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::io::Write;
use v_module::module::*;
use v_onto::individual::{Individual, RawObj};
use v_onto::parser::parse_raw;

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

    let module = Module::new();
    let param_name = "exim_slave_port";
    let exim_slave_port = module.get_property(param_name);

    if exim_slave_port.is_none() {
        error!("not found param {} in properties file", param_name);
        return Ok(());
    }

    let mut server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&exim_slave_port.unwrap()) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let mut recv_indv = Individual::new_raw(RawObj::new(recv_msg.to_vec()));

            let mut resp_msg = "err".to_string();

            if let Ok(uri) = parse_raw(&mut recv_indv) {
                recv_indv.obj.uri = uri;
                resp_msg = recv_indv.obj.uri.clone();

                let wcmd = recv_indv.get_first_integer("cmd");
                if wcmd.is_err() {
                    continue;
                }
                let cmd = IndvOp::from_i64(wcmd.unwrap_or_default().clone());

                let target_veda = recv_indv.get_first_literal("target_veda");
                if target_veda.is_err() {
                    continue;
                }
            }

            if let Err(e) = server.send(Message::from(resp_msg.as_ref())) {
                error!("fail send {:?}", e);
            } else {

            }

        //msg.clear();
        } else {
            error!("fail recv")
        }
    }
}
