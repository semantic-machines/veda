#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

use chrono::Utc;
use ini::Ini;
use nng::{Message, Protocol, Socket};
use regex::Regex;
use serde_json::json;
use serde_json::value::Value as JSONValue;
use v_api::{IndvOp, ResultCode};
use v_module::module::{init_log, Module};
use v_onto::datatype::Lang;
use v_onto::individual::Individual;
use v_search::FTQuery;

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

const EMPTY_SHA256_HASH: &str = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";

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

    let mut module = Module::default();

    let systicket = if let Ok(t) = module.get_sys_ticket_id() {
        t
    } else {
        error!("fail get systicket");
        return Ok(());
    };

    loop {
        if let Ok(recv_msg) = server.recv() {
            let res = req_prepare(&recv_msg, &systicket, &mut module);
            if let Err(e) = server.send(res) {
                error!("fail send {:?}", e);
            }
        }
    }
}

fn req_prepare(request: &Message, systicket: &str, module: &mut Module) -> Message {
    let v: JSONValue = if let Ok(v) = serde_json::from_slice(request.as_slice()) {
        v
    } else {
        JSONValue::Null
    };

    match v["function"].as_str().unwrap_or_default() {
        "authenticate" => {
            let ticket = authenticate(v["login"].as_str(), v["password"].as_str(), v["secret"].as_str(), systicket, module);

            let mut res = JSONValue::default();
            res["type"] = json!("ticket");
            res["id"] = json!(ticket.id);
            res["user_uri"] = json!(ticket.user_uri);
            res["user_login"] = json!(ticket.user_login);
            res["result"] = json!(ticket.result as i64);
            res["end_time"] = json!(ticket.end_time);

            return Message::from(res.to_string().as_bytes());
        }
        "get_ticket_trusted" => {}
        _ => {}
    }

    Message::default()
}

fn authenticate(login: Option<&str>, password: Option<&str>, secret: Option<&str>, systicket: &str, module: &mut Module) -> Ticket {
    let mut ticket = Ticket {
        id: String::default(),
        user_uri: String::default(),
        user_login: String::default(),
        result: ResultCode::AuthenticationFailed,
        start_time: 0,
        end_time: 0,
    };

    info!("authenticate, login={:?} password={:?}, secret={:?}", login, password, secret);

    let login = login.unwrap_or_default();
    let password = password.unwrap_or_default();
    let secret = secret.unwrap_or_default();

    if login.is_empty() || login.len() < 3 {
        return ticket;
    }

    if !secret.is_empty() && secret.len() > 5 && password == EMPTY_SHA256_HASH {
        ticket.result = ResultCode::EmptyPassword;
        return ticket;
    }

    if secret.is_empty() && secret.len() > 5 && (password.is_empty() || password.len() < 64) {
        ticket.result = ResultCode::InvalidPassword;
        return ticket;
    }

    if !secret.is_empty() && secret != "?" && secret.len() < 6 {
        ticket.result = ResultCode::InvalidSecret;
        return ticket;
    }

    lazy_static! {
        static ref RE: Regex = Regex::new("[-]").unwrap();
    }

    let query = format!("'v-s:login' == '{}'", RE.replace_all(login, " +"));

    let candidate_account_ids = module.fts.query(FTQuery::new_with_ticket(systicket, &query));
    if candidate_account_ids.result_code == 200 && candidate_account_ids.count > 0 {
        for account_id in &candidate_account_ids.result {
            if let Some(account) = module.get_individual(&account_id, &mut Individual::default()) {
                let user_id = account.get_first_literal("v-s:owner").unwrap_or_default();
                if user_id.is_empty() {
                    error!("authenticate:user id is null, user_indv={}", account);
                    continue;
                }

                let user_login = account.get_first_literal("v-s:login").unwrap_or_default();
                if user_login.is_empty() {
                    error!("authenticate:user login {:?} not equal request login {}", user_login, login);
                    continue;
                }

                if user_login.to_lowercase() != login.to_lowercase() {
                    error!("authenticate:user login {} not equal request login {}", user_login, login);
                    continue;
                }

                let mut person = Individual::default();
                if module.get_individual(&user_id, &mut person).is_none() {
                    error!("authenticate:user {} not found", user_id);
                    continue;
                }

                let exist_password;
                let edited;
                let mut uses_credential = Individual::default();

                match account.get_first_literal("v-s:usesCredential") {
                    Some(uses_credential_uri) => {
                        if let Some(uses_credential) = module.get_individual(&user_id, &mut uses_credential) {
                            exist_password = uses_credential.get_first_literal("v-s:password").unwrap_or_default();
                            edited = uses_credential.get_first_datetime("v-s:dateFrom").unwrap_or_default();
                        }
                    }
                    None => {
                        exist_password = account.get_first_literal("v-s:password").unwrap_or_default();
                        edited = Utc::now().naive_utc().timestamp();

                        uses_credential.set_id(&(account_id.to_owned() + "-crdt"));
                        uses_credential.add_uri("rdf:type", "v-s:Credential");
                        uses_credential.add_string("v-s:password", &exist_password, Lang::NONE);
                        uses_credential.add_datetime("v-s:dateFrom", edited);

                        let res = module.api.update(&systicket, IndvOp::Put, &mut uses_credential);
                        if res.result != ResultCode::Ok {
                            error!("fail update, uri={}, result_code={:?}", uses_credential.get_id(), res.result);
                            continue;
                        } else {
                            info!("authenticate: create v-s:Credential{}, res={:?}", uses_credential.get_id(), res);

                            account.remove("v-s:password");
                            account.add_uri("v-s:usesCredential", uses_credential.get_id());

                            let res = module.api.update(&systicket, IndvOp::Put, account);
                            if res.result != ResultCode::Ok {
                                error!("fail update, uri={}, res={:?}", account.get_id(), res);
                                continue;
                            }
                            info!("authenticate: update user {}, res={:?}", account.get_id(), res);
                        }
                    }
                }

                let origin = person.get_first_literal("v-s:origin");
                let old_secret = uses_credential.get_first_literal("v-s:secret").unwrap_or_default();

                if !secret.is_empty() && secret.len() > 5 {
                    if old_secret.is_empty() {
                        error!("authenticate:update password: secret not found, user={}", person.get_id());
                        ticket.result = ResultCode::InvalidSecret;
                        //remove_secret(ctx, i_usesCredential, iuser.uri, storage, &sticket);
                        return ticket;
                    }

                    if secret != old_secret {
                        error!("authenticate:request for update password: send secret not equal request secret {}, user={}", secret, person.get_id());
                        ticket.result = ResultCode::InvalidSecret;
                        //remove_secret(ctx, i_usesCredential, iuser.uri, storage, &sticket);
                        return ticket;
                    }

                    let prev_secret_date = uses_credential.get_first_datetime("v-s:SecretDateFrom");
                }
            } else {
                error!("fail read, uri={}", &account_id);
            }
        }
    }

    ticket
}
