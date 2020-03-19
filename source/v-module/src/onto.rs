use pickledb::{PickleDb, PickleDbDumpPolicy, SerializationMethod};
use std::collections::HashSet;
use v_onto::individual::*;
use v_onto::onto::*;
use v_storage::storage::VStorage;

pub const DATA_BASE_PATH: &str = "./data";

pub fn load_onto(storage: &mut VStorage, onto: &mut Onto) -> bool {
    let onto_index = PickleDb::load(DATA_BASE_PATH.to_owned() + "/onto-index.db", PickleDbDumpPolicy::DumpUponRequest, SerializationMethod::Json).unwrap();

    info!("load {} onto elements", onto_index.total_keys());

    for kv in onto_index.iter() {
        let id = kv.get_key();
        let mut indv: Individual = Individual::default();
        if storage.get_individual(&id, &mut indv) {
            onto.update(&mut indv);
        }
    }

    info!("add to ierarhy {} elements", onto.relations.len());

    let keys: Vec<String> = onto.relations.iter().map(|(key, _)| key.clone()).collect();

    for el in keys.iter() {
        let mut buf: HashSet<String> = HashSet::new();
        onto.get_subs(el, &mut buf);
        if !buf.is_empty() {
            onto.update_subs(el, &mut buf);
            //info!("{}, subs={:?}", el, buf);
        }
    }

    info!("end update subs");

    true
}
