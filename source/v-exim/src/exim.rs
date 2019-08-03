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

    pub fn as_string(&self) -> String {
        match self {
            ExImCode::Ok => "ok",
            ExImCode::InvalidMessage => "invalid_message",
            ExImCode::InvalidCmd => "invalid_cmd",
            ExImCode::InvalidTarget => "invalid_target",
            ExImCode::FailUpdate => "fail_update",
            ExImCode::FailTransmit => "fail_transmit",
            ExImCode::FailSend => "fail_send",
            ExImCode::FailReceive => "fail_receive",
            // ...
            ExImCode::Unknown => "unknown",
        }
        .to_string()
    }
}

pub fn enc_slave_resp(uri: &str, code: ExImCode) -> String {
    uri.clone().to_owned() + "," + &code.as_string()
}

pub fn dec_slave_resp(msg: &[u8]) -> (&str, ExImCode) {
    ("?", ExImCode::Unknown)
}
