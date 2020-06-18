#[macro_use]
extern crate log;

use crate::vql::TTA;
use crate::xapian_reader::XapianReader;
use crate::xapian_vql::OptAuthorize;
use ini::Ini;
use std::process;
use v_api::app::ResultCode;
use v_ft_xapian::index_schema::IndexerSchema;
use v_ft_xapian::init_db_path;
use v_ft_xapian::key2slot::Key2Slot;
use v_ft_xapian::xerror::Result;
use v_module::info::ModuleInfo;
use v_module::module::init_log;
use v_module::onto::load_onto;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_storage::storage::*;
use xapian_rusty::Stem;

mod vql;
mod xapian_reader;
mod xapian_vql;

const BASE_PATH: &str = "./data";

fn main() {
    init_log();

    let res = TTA::parse_expr("'rdf:type' == '*'");
    if let Some(bt) = res {
        println!("{}", bt.as_ref());
    }

    let module_info = ModuleInfo::new(BASE_PATH, "fulltext_indexer", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        process::exit(101);
    }

    let key2slot = Key2Slot::load();
    if key2slot.is_err() {
        error!("load key2slot, err={:?}", key2slot.err());
        return;
    }

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");

    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let tarantool_addr = if let Some(p) = section.get("tarantool_url") {
        p.to_owned()
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    if !tarantool_addr.is_empty() {
        info!("tarantool addr={}", &tarantool_addr);
    }

    let mut storage: VStorage;
    if !tarantool_addr.is_empty() {
        storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
    } else {
        storage = VStorage::new_lmdb("./data", StorageMode::ReadOnly);
    }

    let mut onto = Onto::default();
    load_onto(&mut storage, &mut onto);

    let mut xr = XapianReader {
        using_dbqp: Default::default(),
        opened_db: Default::default(),
        xapian_stemmer: Stem::new("russian").unwrap(),
        xapian_lang: "".to_string(),
        index_schema: Default::default(),
        mdif: module_info.unwrap(),
        key2slot: key2slot.unwrap(),
        onto: Default::default(),
        db2path: init_db_path(),
    };

    let schema = load_index_schema(&onto, &mut storage, &mut xr);

    //xr.query("", "'rdf:type' == '*'", "", "", 0, 0, 0, add_out_element, OptAuthorize::NO);
}

fn add_out_element(id: &str) {
    info!("out id={}", id);
}

pub fn load_index_schema(onto: &Onto, storage: &mut VStorage, xr: &mut XapianReader) {
    match xr.query("cfg:VedaSystem", "'rdf:type' === 'vdi:ClassIndex'", "", "", 0, 0, 0, add_out_element, OptAuthorize::NO) {
        Ok(res) => {
            if res.result_code == ResultCode::Ok && res.count > 0 {
                for id in res.result.iter() {
                    let mut indv = &mut Individual::default();
                    if storage.get_individual(id, &mut indv) {
                        xr.index_schema.add_schema_data(onto, indv);
                    }
                }
            }
        }
        Err(e) => {
            error!("fail load index schema, err={:?}", e);
        }
    }
}
