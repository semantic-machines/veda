use std::str::*;

#[derive(PartialEq, Debug, Clone)]
#[repr(i16)]
pub enum ExImCode {
    Ok = 200,
    InvalidMessage = -1000,
    InvalidCmd = -1001,
    InvalidTarget = -1002,
    FailUpdate = -3,
    FailTransmit = -2000,
    FailSend = -2001,
    FailReceive = -2002,
    Unknown = -999,
}

impl ExImCode {
    pub fn from_i64(value: i64) -> ExImCode {
        match value {
            200 => ExImCode::Ok,
            -1000 => ExImCode::InvalidMessage,
            -1001 => ExImCode::InvalidCmd,
            -1002 => ExImCode::InvalidTarget,
            -3 => ExImCode::FailUpdate,
            -2000 => ExImCode::FailTransmit,
            -2001 => ExImCode::FailSend,
            -2002 => ExImCode::FailReceive,
            // ...
            _ => ExImCode::Unknown,
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            ExImCode::Ok => 200,
            ExImCode::InvalidMessage => -1000,
            ExImCode::InvalidCmd => -1001,
            ExImCode::InvalidTarget => -1002,
            ExImCode::FailUpdate => -3,
            ExImCode::FailTransmit => -2000,
            ExImCode::FailSend => -2001,
            ExImCode::FailReceive => -2002,
            // ...
            ExImCode::Unknown => -999,
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            ExImCode::Ok => "ok",
            ExImCode::InvalidMessage => "invalid message",
            ExImCode::InvalidCmd => "invalid cmd",
            ExImCode::InvalidTarget => "invalid target",
            ExImCode::FailUpdate => "fail update",
            ExImCode::FailTransmit => "fail transmit",
            ExImCode::FailSend => "fail send",
            ExImCode::FailReceive => "fail receive",
            // ...
            ExImCode::Unknown => "unknown",
        }
        .to_string()
    }
}

pub fn enc_slave_resp(uri: &str, code: ExImCode) -> String {
    uri.to_owned() + "," + &code.to_i64().to_string()
}

pub fn dec_slave_resp(msg: &[u8]) -> (&str, ExImCode) {

    let mut iter = msg.split(|ch| *ch == b',');

    if let Some (wuri) = iter.next() {
        let uri = from_utf8 (wuri);

        if uri.is_ok() {
            if let Some (wcode) = iter.next() {
                if let Ok (s) = from_utf8(wcode) {
                    if let Ok (code) = s.parse::<i64>() {
                        return (uri.unwrap(), ExImCode::from_i64(code));
                    }
                }

            }
        }
    }
    ("?", ExImCode::Unknown)
}
