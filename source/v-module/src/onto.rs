use std::collections::HashSet;
use v_onto::individual::*;
use v_onto::onto::*;
use v_search::{FTClient, FTQuery};
use v_storage::storage::VStorage;

pub fn load_onto(ft_client: &mut FTClient, storage: &mut VStorage, onto: &mut Onto) -> bool {
    let onto_types = vec!["rdfs:Class", "owl:Class", "rdfs:Datatype", "rdf:Property", "owl:DatatypeProperty", "owl:ObjectProperty"];

    let mut query = String::new();

    for el in &onto_types {
        if !query.is_empty() {
            query.push_str(" || ");
        }
        query.push_str("'rdf:type' === '");
        query.push_str(el);
        query.push_str("'");
    }

    let res = ft_client.query(FTQuery::new_with_user("cfg:VedaSystem", &query));
    if res.result_code == 200 && res.count > 0 {
        info!("load {} onto elements", res.count);
        for el in &res.result {
            let mut indv: Individual = Individual::new();
            if storage.set_binobj(&el, &mut indv) {
                onto.update(&mut indv);
            }
        }
        info!("add to ierarhy {} elements", onto.relations.len());

        let keys: Vec<String> = onto.relations.iter().map(|(key, _)| key.clone()).collect();

        for el in keys.iter() {
            let mut buf: HashSet<String> = HashSet::new();
            onto.get_subs(el, &mut buf);
            if buf.len() > 0 {
                onto.update_subs(el, &mut buf);
                //info!("{}, subs={:?}", el, buf);
            }
        }

        info!("end update subs");
    }

    true
}
