use crate::individual::*;
use std::collections::{HashMap, HashSet};
use std::fmt;

// TODO: Load ontology individuals to have them by hand at runtime.

#[derive(PartialEq, Debug)]
pub enum RelType {
    Sub,
    Super,
}

#[derive(Debug)]
pub struct Onto {
    pub relations: HashMap<String, HashMap<String, RelType>>,
    pub prefixes: HashMap<String, String>,
}

impl fmt::Display for Onto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self.relations)
    }
}

impl Default for Onto {
    fn default() -> Self {
        Onto {
            relations: HashMap::new(),
            prefixes: HashMap::new(),
        }
    }
}

impl Onto {
    pub fn update(&mut self, indv: &mut Individual) -> bool {
        if let Some(vtype) = indv.get_first_literal("rdf:type") {
            if vtype == "owl:Class"
                || vtype == "rdfs:Class"
                || vtype == "rdf:Property"
                || vtype == "rdfs:Datatype"
                || vtype == "owl:ObjectProperty"
                || vtype == "owl:DatatypeProperty"
            {
                let subs = if vtype == "owl:Class" || vtype == "rdfs:Class" {
                    if let Some(_subclasses) = indv.get_literals("rdfs:subClassOf") {
                        _subclasses
                    } else {
                        Vec::new()
                    }
                } else if vtype == "rdf:Property" || vtype == "owl:ObjectProperty" || vtype == "owl:DatatypeProperty" {
                    if let Some(_subproperties) = indv.get_literals("rdfs:subPropertyOf") {
                        _subproperties
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                // if subs.len() > 0 {
                let onto_el = self.relations.entry(indv.obj.uri.clone()).or_default();

                for sub in subs.clone() {
                    let rel_type = onto_el.entry(sub).or_insert(RelType::Super);
                    *rel_type = RelType::Super;
                }

                for sub in subs {
                    let onto_el = self.relations.entry(sub).or_default();
                    let rel_type = onto_el.entry(indv.obj.uri.clone()).or_insert(RelType::Sub);
                    *rel_type = RelType::Sub;
                }
            //}
            } else if vtype == "owl:Ontology" {
                if let Some(full_url) = indv.get_first_literal("v-s:fullUrl") {
                    debug!("prefix : {} -> {}", indv.get_id(), full_url);
                    self.prefixes.insert(indv.get_id().to_owned(), full_url);
                }
            } else {
                debug!("is not onto element: {}", indv.obj.uri)
            }
        }
        true
    }

    pub fn is_some_entered(&mut self, el: &str, subs: &[&str]) -> bool {
        for sub in subs {
            if self.relations.contains_key(*sub) {
                let onto_el = self.relations.entry((*sub).to_string()).or_default();
                if let Some(rtype) = onto_el.get(el) {
                    if *rtype == RelType::Sub {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn is_some_entered_it<'a, I>(&mut self, el: &str, subs: I) -> bool
    where
        I: Iterator<Item = &'a String>,
    {
        for sub in subs {
            if self.relations.contains_key(sub) {
                let onto_el = self.relations.entry((*sub).to_string()).or_default();
                if let Some(rtype) = onto_el.get(el) {
                    if *rtype == RelType::Sub {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn get_subs(&self, el: &str, collector: &mut HashSet<String>) {
        if self.relations.contains_key(el) {
            let mut buf = Vec::new();
            if let Some(qqq) = self.relations.get(el) {
                for (x, t) in qqq {
                    if *t == RelType::Sub && !collector.contains(x) {
                        collector.insert(x.to_string());
                        buf.push(x.to_string());
                    }
                }
            }

            for x in buf {
                self.get_subs(&x, collector);
            }
        }
    }

    pub fn get_supers(&self, el: &str, collector: &mut HashSet<String>) {
        if self.relations.contains_key(el) {
            let mut buf = Vec::new();
            if let Some(qqq) = self.relations.get(el) {
                for (x, t) in qqq {
                    if *t == RelType::Super && !collector.contains(x) {
                        collector.insert(x.to_string());
                        buf.push(x.to_string());
                    }
                }
            }

            for x in buf {
                self.get_supers(&x, collector);
            }
        }
    }

    pub fn update_subs(&mut self, el: &str, subs: &mut HashSet<String>) {
        if self.relations.contains_key(el) {
            let onto_el = self.relations.entry(el.to_string()).or_default();

            for sub in subs.iter() {
                let rel_type = onto_el.entry(sub.to_string()).or_insert(RelType::Sub);
                *rel_type = RelType::Sub;
            }
        }
    }

    pub fn get_full_prefix(&self, short_prefix: &str) -> Option<&String> {
        self.prefixes.get(short_prefix)
    }
}
