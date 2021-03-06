#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

mod auth;
mod common;

use crate::auth::*;
use crate::common::{get_ticket_trusted, read_auth_configuration, AuthConf, UserStat};
use ini::Ini;
use nng::{Message, Protocol, Socket};
use serde_json::json;
use serde_json::value::Value as JSONValue;
use std::collections::HashMap;
use v_ft_xapian::xapian_reader::XapianReader;
use v_module::module::{create_sys_ticket, init_log};
use v_module::v_storage::storage::StorageMode;
use v_module::veda_backend::*;

fn main() -> std::io::Result<()> {
    init_log("AUTH");

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");
    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let auth_url = section.get("auth_url").expect("param [auth_url] not found in veda.properties");

    let server = Socket::new(Protocol::Rep0)?;
    if let Err(e) = server.listen(&auth_url) {
        error!("failed to listen, err = {:?}", e);
        return Ok(());
    }

    let mut backend = Backend::create(StorageMode::ReadWrite, false);

    let systicket = if let Ok(t) = backend.get_sys_ticket_id() {
        t
    } else {
        error!("failed to get systicket, create new");

        create_sys_ticket(&mut backend.storage).id
    };

    let mut suspicious: HashMap<String, UserStat> = HashMap::new();

    let conf = read_auth_configuration(&mut backend);

    if let Some(mut xr) = XapianReader::new("russian", &mut backend.storage) {
        loop {
            if let Ok(recv_msg) = server.recv() {
                let res = req_prepare(&conf, &recv_msg, &systicket, &mut xr, &mut backend, &mut suspicious);
                if let Err(e) = server.send(res) {
                    error!("failed to send, err = {:?}", e);
                }
            }
        }
    } else {
        error!("failed to init ft-query");
    }
    Ok(())
}

fn req_prepare(conf: &AuthConf, request: &Message, systicket: &str, xr: &mut XapianReader, module: &mut Backend, suspicious: &mut HashMap<String, UserStat>) -> Message {
    let v: JSONValue = if let Ok(v) = serde_json::from_slice(request.as_slice()) {
        v
    } else {
        JSONValue::Null
    };

    match v["function"].as_str().unwrap_or_default() {
        "authenticate" => {
            let login = v["login"].as_str().unwrap_or_default();
            let password = v["password"].as_str().unwrap_or_default();
            let secret = v["secret"].as_str().unwrap_or_default();

            let user_stat = suspicious.entry(login.to_owned()).or_insert_with(UserStat::default);

            let mut ah = AuthWorkPlace {
                conf,
                login,
                password,
                secret,
                sys_ticket: systicket,
                xr,
                backend: module,
                user_stat,
                stored_password: "".to_owned(),
                stored_salt: "".to_string(),
                edited: 0,
                credential: &mut Default::default(),
                is_permanent: false,
            };

            let ticket = ah.authenticate();

            info!("{:?}", ticket);

            let mut res = JSONValue::default();
            res["type"] = json!("ticket");
            res["id"] = json!(ticket.id);
            res["user_uri"] = json!(ticket.user_uri);
            res["user_login"] = json!(ticket.user_login);
            res["result"] = json!(ticket.result as i64);
            res["end_time"] = json!(ticket.end_time);

            return Message::from(res.to_string().as_bytes());
        }
        "get_ticket_trusted" => {
            let ticket = get_ticket_trusted(conf, v["ticket"].as_str(), v["login"].as_str(), xr, module);

            let mut res = JSONValue::default();
            res["type"] = json!("ticket");
            res["id"] = json!(ticket.id);
            res["user_uri"] = json!(ticket.user_uri);
            res["user_login"] = json!(ticket.user_login);
            res["result"] = json!(ticket.result as i64);
            res["end_time"] = json!(ticket.end_time);

            return Message::from(res.to_string().as_bytes());
        }
        _ => {
            error!("unknown command {:?}", v["function"].as_str());
        }
    }

    Message::default()
}
