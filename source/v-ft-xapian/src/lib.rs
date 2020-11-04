#[macro_use]
extern crate log;

#[macro_use]
extern crate scan_fmt;

#[macro_use]
extern crate maplit;

use std::collections::HashMap;

pub mod index_schema;
pub mod key2slot;
pub mod vql;
pub mod xapian_reader;
pub mod xapian_vql;

pub fn init_db_path() -> HashMap<String, String> {
    hashmap! { "base".to_owned() => "data/xapian-search-base".to_owned(), "system".to_owned()=>"data/xapian-search-system".to_owned(), "deleted".to_owned()=>"data/xapian-search-deleted".to_owned(), "az".to_owned()=>"data/xapian-search-az".to_owned() }
}

pub fn to_lower_and_replace_delimiters(src: &str) -> String {
    src.chars()
        .map(|x| match x {
            '-' => '_',
            ':' => '_',
            _ => x.to_ascii_lowercase(),
        })
        .collect()
}
