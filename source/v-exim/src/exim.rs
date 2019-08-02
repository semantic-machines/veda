#[derive(PartialEq, Debug, Clone)]
#[repr(i16)]
pub enum ExImCode {
    Ok = 200,
    InvalidCmd = -1,
    InvalidTarget = -2,
    FailUpdate = -3,
    Unknown = -999,
}

impl ExImCode {
    pub fn from_i64(value: i64) -> ExImCode {
        match value {
            200 => ExImCode::Ok,
            -1 => ExImCode::InvalidCmd,
            -2 => ExImCode::InvalidTarget,
            -3 => ExImCode::FailUpdate,
            // ...
            _ => ExImCode::Unknown,
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            ExImCode::Ok => "ok",
            ExImCode::InvalidCmd => "invalid_cmd",
            ExImCode::InvalidTarget => "invalid_target",
            ExImCode::FailUpdate => "fail_update",
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
