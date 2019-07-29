#[macro_use]
extern crate log;

use nng::{Message, Protocol, Socket};
use serde_json::Value;
use v_onto::individual::{Individual, RawObj};

#[derive(PartialEq, Debug, Clone)]
#[repr(u16)]
pub enum IndvOp {
    /// Сохранить
    Put = 1,

    /// Сохранить
    Get = 2,

    /// Получить тикет
    GetTicket = 3,

    /// Авторизовать
    Authorize = 8,

    /// Установить в
    SetIn = 45,

    /// Добавить в
    AddIn = 47,

    /// Убрать из
    RemoveFrom = 48,

    /// Убрать
    Remove = 51,

    None = 52,
}

impl IndvOp {
    pub fn from_i64(value: i64) -> IndvOp {
        match value {
            1 => IndvOp::Get,
            2 => IndvOp::Put,
            51 => IndvOp::Remove,
            47 => IndvOp::AddIn,
            45 => IndvOp::SetIn,
            48 => IndvOp::RemoveFrom,
            8 => IndvOp::Authorize,
            3 => IndvOp::GetTicket,
            // ...
            _ => IndvOp::None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
#[repr(u16)]
enum ResultCode {
    /// 0
    zero = 0,

    /// 200
    Ok = 200,

    /// 201
    Created = 201,

    /// 204
    NoContent = 204,

    /// 400
    BadRequest = 400,

    /// 403
    Forbidden = 403,

    /// 404
    NotFound = 404,

    /// 422
    UnprocessableEntity = 422,

    /// 429
    TooManyRequests = 429,

    /// 464
    SecretExpired = 464,

    /// 465
    EmptyPassword = 465,

    /// 466
    NewPasswordIsEqualToOld = 466,

    /// 467
    InvalidPassword = 467,

    /// 468
    InvalidSecret = 468,

    /// 469
    PasswordExpired = 469,

    /// 470
    TicketNotFound = 470,

    /// 471
    TicketExpired = 471,

    /// 472
    NotAuthorized = 472,

    /// 473
    AuthenticationFailed = 473,

    /// 474
    NotReady = 474,

    /// 475
    FailOpenTransaction = 475,

    /// 476
    FailCommit = 476,

    /// 477
    FailStore = 477,

    /// 500
    InternalServerError = 500,

    /// 501
    NotImplemented = 501,

    /// 503
    ServiceUnavailable = 503,

    InvalidIdentifier = 904,

    /// 999
    DatabaseModifiedError = 999,

    /// 1021
    DiskFull = 1021,

    /// 1022
    DuplicateKey = 1022,

    /// 1118
    SizeTooLarge = 1118,

    /// 4000
    ConnectError = 4000,
}

pub struct APIClient {
    client: Socket,
    addr: String,
    is_ready: bool,
}

impl APIClient {
    pub fn new(_ro_client_addr: String) -> APIClient {
        APIClient {
            client: Socket::new(Protocol::Req0).unwrap(),
            addr: _ro_client_addr,
            is_ready: false,
        }
    }

    pub fn connect(&mut self) -> bool {
        if let Err(e) = self.client.dial(self.addr.as_str()) {
            error!("ft-client:fail dial to ro-storage, [{}], err={}", self.addr, e);
        } else {
            info!("sucess connect to ro-storage, [{}]", self.addr);
            self.is_ready = true;
        }
        self.is_ready
    }

    pub fn put(&mut self, indv: &mut Individual) {
        if !self.is_ready {
            self.connect();
        }

        if !self.is_ready {
            //res.result_code = 474;
            return;
        }

        let req = Message::from("query".as_bytes());

        if let Err(e) = self.client.send(req) {
            error!("fail send to main module, err={:?}", e);
            //res.result_code = 474;
            return;
        }

        // Wait for the response from the server.
        let wmsg = self.client.recv();

        if let Err(e) = wmsg {
            error!("fail recv from main module, err={:?}", e);
            //res.result_code = 474;
            return;
        }

        let msg = wmsg.unwrap();

        let reply = String::from_utf8_lossy(&msg);

        let v: Value = if let Ok(v) = serde_json::from_str(&reply) {
            v
        } else {
            Value::Null
        };

        //res.result_code = v["result_code"].as_i64().unwrap_or_default() as i32;

        //if res.result_code == 200 {
        //}
    }
}
