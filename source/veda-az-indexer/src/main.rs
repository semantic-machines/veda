#[macro_use]
extern crate log;

use chrono::Local;
use env_logger::Builder;
use ini::Ini;
use log::LevelFilter;

use std::io::Write;
use std::{thread, time};
use v_module::module::Module;
use v_onto::individual::*;
use v_queue::consumer::*;

struct Context {
    id: u32,
}

fn main() -> Result<(), i32> {
    info! ("AZ-INDEXER");
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

    let mut module = Module::default();

    let mut queue_consumer = Consumer::new("./data/queue", "az-indexer", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let mut ctx = Context {
        id: 0,
    };

    module.listen_queue(
        &mut queue_consumer,
        &mut ctx,
        &mut (before_bath as fn(&mut Context)),
        &mut (prepare as fn(&mut Context, &mut Individual)),
        &mut (after_bath as fn(&mut Context)),
    );
    Ok(())
}

fn before_bath(ctx: &mut Context) {}

fn after_bath(ctx: &mut Context) {
    thread::sleep(time::Duration::from_millis(3000));
}

fn prepare(ctx: &mut Context, indv: &mut Individual) {}
