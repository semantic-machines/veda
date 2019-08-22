#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::io::Write;
use v_api::*;
use v_exim::*;
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
            let resp_msg = prepare_recv_msg(recv_msg.to_vec(), &systicket, &mut module);

            if let Err(e) = server.send(Message::from(resp_msg.as_ref())) {
                error!("fail send {:?}", e);
            }

        //msg.clear();
        } else {
            error!("fail recv")
        }
    }
}

fn prepare_recv_msg(recv_msg: Vec<u8>, systicket: &str, module: &mut Module) -> String {
    let mut recv_indv = Individual::new_raw(RawObj::new(recv_msg));

    if let Ok(uri) = parse_raw(&mut recv_indv) {
        recv_indv.obj.uri = uri;

        let wcmd = recv_indv.get_first_integer("cmd");
        if wcmd.is_err() {
            return enc_slave_resp(&recv_indv.obj.uri, ExImCode::InvalidCmd);
        }
        let cmd = IndvOp::from_i64(wcmd.unwrap_or_default());

        let source_veda = recv_indv.get_first_literal("source_veda");
        if source_veda.is_err() {
            return enc_slave_resp(&recv_indv.obj.uri, ExImCode::InvalidTarget);
        }

        let source_veda = source_veda.unwrap_or_default();
        if source_veda.len() < 32 {
            return enc_slave_resp(&recv_indv.obj.uri, ExImCode::InvalidTarget);
        }

        let target_veda = recv_indv.get_first_literal("target_veda");
        if target_veda.is_err() {
            return enc_slave_resp(&recv_indv.obj.uri, ExImCode::InvalidTarget);
        }

        let new_state = recv_indv.get_first_binobj("new_state");
        if cmd != IndvOp::Remove && !new_state.is_err() {
            let mut indv = Individual::new_raw(RawObj::new(new_state.unwrap_or_default()));
            if let Ok(uri) = parse_raw(&mut indv) {
                indv.parse_all();
                indv.obj.uri = uri.clone();
                indv.obj.add_uri("sys:source", &source_veda, 0);

                let res = module.api.update(systicket, cmd, &mut indv);

                if res.result != ResultCode::Ok {
                    error!("fail update, uri={}, result_code={:?}", recv_indv.obj.uri, res.result);
                    return enc_slave_resp(&recv_indv.obj.uri, ExImCode::FailUpdate);
                } else {
                    return enc_slave_resp(&recv_indv.obj.uri, ExImCode::Ok);
                }
            }
        }
    }

    enc_slave_resp(&recv_indv.obj.uri, ExImCode::FailUpdate)
}