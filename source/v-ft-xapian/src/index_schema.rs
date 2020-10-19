use crate::xapian_reader::XapianReader;
use std::collections::{HashMap, HashSet};
use std::fmt;
use v_api::app::ResultCode;
use v_module::module::Module;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_search::common::FTQuery;

#[derive(Debug)]
pub struct IndexerSchema {
    class_property_2_id: HashMap<String, String>,
    class_2_database: HashMap<String, String>,
    id_2_individual: HashMap<String, Individual>,
    database_2_true: HashMap<String, bool>,
}

impl fmt::Display for IndexerSchema {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (k, v) in self.class_property_2_id.iter() {
            writeln!(f, "{} -> {}", k, v)?
        }
        Ok(())
    }
}

impl IndexerSchema {
    pub fn load(&mut self, force: bool, onto: &Onto, module: &mut Module, xr: &mut XapianReader) {
        if self.class_property_2_id.is_empty() || force {
            if force {
                info!("force reload schema");
            } else {
                info!("reload schema");
            }

            let res = xr.query(FTQuery::new_with_user("cfg:VedaSystem", "'rdf:type' === 'vdi:ClassIndex'"), &mut module.storage);
            if res.result_code == ResultCode::Ok && res.count > 0 {
                for id in res.result.iter() {
                    if let Some(i) = module.get_individual(id, &mut Individual::default()) {
                        i.parse_all();
                        self.add_schema_data(onto, i);
                    }
                }
            }
            info!("SCHEMA \n{}", self);
        }
    }

    pub fn get_dbname_of_class(&self, id: &str) -> &str {
        if let Some(v) = self.class_2_database.get(id) {
            return v;
        }
        "base"
    }

    pub fn get_copy_of_index(&self, id: &str) -> Option<Individual> {
        if let Some(indv) = self.id_2_individual.get(id) {
            Some(Individual::new_from_obj(indv.get_obj()))
        } else {
            None
        }
    }

    pub fn get_index(&self, id: &str) -> Option<&Individual> {
        self.id_2_individual.get(id)
    }

    pub fn get_index_id_of_uri_and_property(&mut self, id: &str, predicate: &str) -> Option<String> {
        if let Some(id) = self.class_property_2_id.get(&format!("{}+{}", id.to_owned(), predicate)) {
            return Some(id.to_owned());
        }
        None
    }

    pub fn get_index_id_of_property(&mut self, predicate: &str) -> Option<String> {
        if let Some(id) = self.class_property_2_id.get(&format!("+{}", predicate)) {
            return Some(id.to_owned());
        }

        None
    }

    pub fn add_schema_data(&mut self, onto: &Onto, indv: &mut Individual) {
        self.id_2_individual.insert(indv.get_id().to_owned(), Individual::new_from_obj(indv.get_obj()));

        let for_classes = indv.get_literals("vdi:forClass").unwrap_or_default();
        let for_properties = indv.get_literals("vdi:forProperty").unwrap_or_default();
        let indexed_to = indv.get_literals("vdi:indexed_to").unwrap_or_default();

        for for_class in for_classes.iter() {
            let mut sub_classes = HashSet::new();
            onto.get_subs(&for_class, &mut sub_classes);

            if let Some(i) = indexed_to.get(0) {
                self.class_2_database.insert(for_class.to_owned(), i.to_owned());
                self.database_2_true.insert(i.to_owned(), true);
            }

            if sub_classes.is_empty() {
                sub_classes.insert("".to_owned());
            }

            for sub_class in sub_classes.iter() {
                if let Some(i) = indexed_to.get(0) {
                    self.class_2_database.insert(sub_class.to_owned(), i.to_owned());
                    self.database_2_true.insert(i.to_owned(), true);
                }

                for for_property in for_properties.iter() {
                    self.class_property_2_id.insert(format!("{}+{}", sub_class, for_property), indv.get_id().to_owned());
                }
            }

            for for_property in for_properties.iter() {
                self.class_property_2_id.insert(format!("{}+{}", for_class, for_property), indv.get_id().to_owned());
            }
        }
    }
}

impl Default for IndexerSchema {
    fn default() -> Self {
        Self {
            class_property_2_id: Default::default(),
            class_2_database: Default::default(),
            id_2_individual: Default::default(),
            database_2_true: Default::default(),
        }
    }
}
