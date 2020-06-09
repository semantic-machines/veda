use std::error::Error as StdError;
use std::fmt::{self, Display};
use std::io;

pub type Result<T, E = XError> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum XError {
    Xapian(i8),
    Io(io::Error),
}

impl Display for XError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            XError::Xapian(err) => write!(f, "xapian err={}", err),
            XError::Io(err) => err.fmt(f),
        }
    }
}

impl StdError for XError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            XError::Io(err) => Some(err),
            _ => None,
        }
    }
}

impl From<io::Error> for XError {
    fn from(err: io::Error) -> Self {
        XError::Io(err)
    }
}

impl From<i8> for XError {
    fn from(err: i8) -> Self {
        XError::Xapian(err)
    }
}
