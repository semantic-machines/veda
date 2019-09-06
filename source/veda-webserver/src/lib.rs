#[macro_use]
extern crate log;
extern crate actix_web;
extern crate futures;
extern crate rusty_tarantool;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate url;
#[macro_use]
extern crate redis_async;

pub mod geo;
pub mod individuals;
pub mod server;

use crate::server::*;

fn main() -> std::io::Result<()> {
    start_server()
}
