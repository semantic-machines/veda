use std::collections::HashSet;
use v_onto::individual::*;
use v_onto::onto::*;
use v_onto::onto_index::OntoIndex;
use v_storage::storage::VStorage;

pub const DATA_BASE_PATH: &str = "./data";

pub fn load_onto(storage: &mut VStorage, onto: &mut Onto) -> bool {
    let onto_index = OntoIndex::load();

    info!("load {} onto elements", onto_index.len());

    for id in onto_index.data.keys() {
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
