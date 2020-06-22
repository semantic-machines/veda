use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use std::collections::HashMap;
use std::io;
use std::io::Write;

#[derive(Copy, Clone)]
enum NamedOrBlankNodeType {
    NamedNode,
    BlankNode,
}

impl NamedOrBlankNodeType {
    fn with_value<'a>(&self, value: &'a str) -> NamedOrBlankNode<'a> {
        match self {
            NamedOrBlankNodeType::NamedNode => NamedNode {
                iri: value,
            }
            .into(),
            NamedOrBlankNodeType::BlankNode => BlankNode {
                id: value,
            }
            .into(),
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////

pub struct TurtleFormatterWithPrefixes<W: Write> {
    write: W,
    current_subject: String,
    current_subject_type: Option<NamedOrBlankNodeType>,
    current_predicate: String,
}

impl<W: Write> TurtleFormatterWithPrefixes<W> {
    /// Builds a new formatter from a `Write` implementation
    pub fn new(write: W, prefixes: &HashMap<String, String>) -> Self {
        let mut f = TurtleFormatterWithPrefixes {
            write,
            current_subject: String::default(),
            current_subject_type: None,
            current_predicate: String::default(),
        };
        f.write_prefixes(prefixes).unwrap_or_default();
        f
    }

    pub fn write_prefixes(&mut self, prefixes: &HashMap<String, String>) -> Result<(), io::Error> {
        let mut keys: Vec<&String> = prefixes.keys().collect();
        keys.sort();
        for prefix in keys.iter() {
            writeln!(self.write, "@prefix {}: <{}> .", prefix, prefixes.get(prefix.to_owned()).unwrap())?;
        }
        writeln!(self.write)?;
        Ok(())
    }

    /// Finishes to write and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        if self.current_subject_type.is_some() {
            writeln!(self.write, " .")?;
        }
        Ok(self.write)
    }
}

impl<W: Write> TriplesFormatter for TurtleFormatterWithPrefixes<W> {
    type Error = io::Error;

    fn format(&mut self, triple: &Triple<'_>) -> Result<(), io::Error> {
        if let Some(current_subject_type) = self.current_subject_type {
            let current_subject = current_subject_type.with_value(&self.current_subject);
            if current_subject == triple.subject {
                if self.current_predicate == *triple.predicate.iri {
                    write!(self.write, " , {:?}", triple.object)?;
                } else {
                    write!(self.write, " ;\n  {:?} {:?}", triple.predicate, triple.object)?;
                }
            } else {
                write!(self.write, " .\n\n{:?} \n  {:?} {:?}", triple.subject, triple.predicate, triple.object)?;
            }
        } else {
            write!(self.write, "{:?} \n  {:?} {:?}", triple.subject, triple.predicate, triple.object)?;
        }

        self.current_subject.clear();
        match triple.subject {
            NamedOrBlankNode::NamedNode(n) => {
                self.current_subject.push_str(n.iri);
                self.current_subject_type = Some(NamedOrBlankNodeType::NamedNode);
            }
            NamedOrBlankNode::BlankNode(n) => {
                self.current_subject.push_str(n.id);
                self.current_subject_type = Some(NamedOrBlankNodeType::BlankNode);
            }
        }
        self.current_predicate.clear();
        self.current_predicate.push_str(triple.predicate.iri);

        Ok(())
    }
}
