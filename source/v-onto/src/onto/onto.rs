use crate::individual::*;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub enum RelType {
    Sub,
    Super,
}

pub struct Onto {
    pub relations: HashMap<String, HashMap<String, RelType>>,
}

impl Onto {
    pub fn new(src: &mut Vec<Individual>) -> Self {
        let onto = Onto {
            relations: HashMap::new(),
        };

        for el in src.iter_mut() {
            let vtype = el.get_first_literal("rdf:type");
        }

        onto
    }
}
