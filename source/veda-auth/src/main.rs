#[macro_use]
extern crate log;

use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde_json::Value;
use v_api::ResultCode;
use v_module::module::init_log;

struct Ticket {
    id: String,

    /// Uri пользователя
    user_uri: String,

    /// login пользователя
    user_login: String,

    /// Код результата, если тикет не валидный != ResultCode.Ok
    result: ResultCode,

    /// Дата начала действия тикета
    start_time: i64,

    /// Дата окончания действия тикета
    end_time: i64,
}

fn main() -> std::io::Result<()> {
    init_log();

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
    let v: Value = if let Ok(v) = serde_json::from_slice(request.as_slice()) {
        v
    } else {
        Value::Null
    };

    match v["function"].as_str().unwrap_or_default() {
        "authenticate" => {
            authenticate(v["login"].as_str(), v["password"].as_str(), v["secret"].as_str());
        }
        "get_ticket_trusted" => {}
        _ => {}
    }

    Message::default()
}

fn authenticate(login: Option<&str>, password: Option<&str>, secret: Option<&str>) -> Ticket {
    Ticket {
        id: String::default(),
        user_uri: String::default(),
        user_login: String::default(),
        result: ResultCode::InternalServerError,
        start_time: 0,
        end_time: 0,
    }
}
