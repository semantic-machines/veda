#[macro_use]
extern crate log;

use v_module::module::init_log;
use ini::Ini;
use nng::{Socket, Protocol, Message};

fn main()  -> std::io::Result<()>{
    init_log ();

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let ro_storage_url = section.get("auth_url").expect("param [auth_url] not found in veda.properties");

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&ro_storage_url) {
        error!("fail listen, {:?}", e);
        return Ok(());
    }

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = req_prepare(&recv_msg);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }

}

fn req_prepare(request: &Message) -> Message {
    Message::default()
}