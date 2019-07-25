#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use nng::{Message, Protocol, Socket};
use std::collections::HashMap;
use std::io::Write;
use v_module::module::*;
use v_onto::individual::*;

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

    let mut module = Module::new();

    let param_name = "exim_slave_addr";

    let exim_slave_addr = module.get_property(param_name);

    if exim_slave_addr.is_none() {
        error!("not found param {} in properties file", param_name);
        return Ok(());
    }

    Ok(())
}
