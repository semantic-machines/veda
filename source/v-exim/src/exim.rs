#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;
use num_traits::{FromPrimitive, ToPrimitive};
use std::str::*;

const TRANSMIT_FAILED: i64 = 32;

#[derive(Primitive, PartialEq, Debug, Clone)]
#[repr(i64)]
pub enum ExImCode {
    Unknown = 0,
    Ok = 1,
    InvalidMessage = 2,
    InvalidCmd = 4,
    InvalidTarget = 8,
    FailUpdate = 16,
    TransmitFailed = TRANSMIT_FAILED,
    SendFailed = 64 | TRANSMIT_FAILED,
    ReceiveFailed = 128 | TRANSMIT_FAILED,
}

impl From<i64> for ExImCode {
    fn from(value: i64) -> Self {
        if let Some(v) = ExImCode::from_i64(value) {
            v
        } else {
            ExImCode::Unknown
        }
    }
}

impl From<ExImCode> for i64 {
    fn from(value: ExImCode) -> Self {
        if let Some(v) = value.to_i64() {
            v
        } else {
            0
        }
    }
}

impl ExImCode {
    pub fn as_string(&self) -> String {
        match self {
            ExImCode::Ok => "ok",
            ExImCode::InvalidMessage => "invalid message",
            ExImCode::InvalidCmd => "invalid cmd",
            ExImCode::InvalidTarget => "invalid target",
            ExImCode::FailUpdate => "fail update",
            ExImCode::TransmitFailed => "fail transmit",
            ExImCode::SendFailed => "fail send",
            ExImCode::ReceiveFailed => "fail receive",
            // ...
            ExImCode::Unknown => "unknown",
        }
        .to_string()
    }
}

pub fn enc_slave_resp(uri: &str, code: ExImCode) -> String {
    let q : i64 = code.into();
    uri.to_owned() + "," + &q.to_string()
}

pub fn dec_slave_resp(msg: &[u8]) -> (&str, ExImCode) {
    let mut iter = msg.split(|ch| *ch == b',');

    if let Some(wuri) = iter.next() {
        if let Ok(uri) = from_utf8(wuri) {
            if let Some(wcode) = iter.next() {
                if let Ok(s) = from_utf8(wcode) {
                    if let Ok(code) = s.parse::<i64>() {
                        return (uri, code.into());
                    }
                }
            }
        }
    }
    ("?", ExImCode::Unknown)
}
