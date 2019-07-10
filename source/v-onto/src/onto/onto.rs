use crate::individual::*;
use std::collections::HashMap;
use std::fmt;

#[derive(PartialEq, Debug)]
pub enum RelType {
    Sub,
    Super,
}

#[derive(Debug)]
pub struct Onto {
    pub relations: HashMap<String, HashMap<String, RelType>>,
}

impl fmt::Display for Onto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self.relations)
    }
}

impl Onto {
    pub fn new() -> Self {
        Onto {
            relations: HashMap::new(),
        }
    }

    pub fn update(&mut self, el: &mut Individual) -> bool {
        if let Ok(vtype) = el.get_first_literal("rdf:type") {
            if vtype == "owl:Class" || vtype == "rdfs:Class" || vtype == "rdf:Property" || vtype == "owl:ObjectProperty" || vtype == "owl:DatatypeProperty" {
                let onto_el = self.relations.entry(el.obj.uri.clone()).or_default();

                let subs = if vtype == "owl:Class" || vtype == "rdfs:Class" {
                    if let Ok(_subclasses) = el.get_literals("rdfs:subClassOf") {
                        _subclasses
                    } else {
                        Vec::new()
                    }
                } else if vtype == "rdf:Property" || vtype == "owl:ObjectProperty" || vtype == "owl:DatatypeProperty" {
                    if let Ok(_subproperties) = el.get_literals("rdfs:subPropertyOf") {
                        _subproperties
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                for sub in subs.clone() {
                    let mut rel_type = onto_el.entry(sub).or_insert(RelType::Super);
                    rel_type = &mut RelType::Super;
                }

                for sub in subs {
                    let onto_el = self.relations.entry(sub).or_default();
                    let mut rel_type = onto_el.entry(el.obj.uri.clone()).or_insert(RelType::Sub);
                    rel_type = &mut RelType::Sub;
                }
            }
        }
        true
    }
}
