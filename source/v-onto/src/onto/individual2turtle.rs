use crate::datatype::*;
use crate::individual::*;
use crate::resource::*;
use crate::turtle_formatters_with_prefixes::TurtleFormatterWithPrefixes;
use chrono::{TimeZone, Utc};
use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::io;

fn from_boolean<'a>(id: &'a str, in_predicate: &'a str, v: &'a str) -> Triple<'a> {
    let subject = NamedNode {
        iri: id,
    };

    let predicate = NamedNode {
        iri: in_predicate,
    };

    let obj = Literal::Typed {
        value: v,
        datatype: NamedNode {
            iri: "xsd:boolean",
        },
    };

    Triple {
        subject: subject.into(),
        predicate,
        object: obj.into(),
    }
}

fn from_integer<'a>(id: &'a str, in_predicate: &'a str, v: &'a str) -> Triple<'a> {
    let subject = NamedNode {
        iri: id,
    };

    let predicate = NamedNode {
        iri: in_predicate,
    };

    let obj = Literal::Typed {
        value: v,
        datatype: NamedNode {
            iri: "xsd:integer",
        },
    };

    Triple {
        subject: subject.into(),
        predicate,
        object: obj.into(),
    }
}

fn from_decimal<'a>(id: &'a str, in_predicate: &'a str, v: &'a str) -> Triple<'a> {
    let subject = NamedNode {
        iri: id,
    };

    let predicate = NamedNode {
        iri: in_predicate,
    };

    let obj = Literal::Typed {
        value: v,
        datatype: NamedNode {
            iri: "xsd:decimal",
        },
    };

    Triple {
        subject: subject.into(),
        predicate,
        object: obj.into(),
    }
}

fn from_datetime<'a>(id: &'a str, in_predicate: &'a str, v: &'a str) -> Triple<'a> {
    let subject = NamedNode {
        iri: id,
    };

    let predicate = NamedNode {
        iri: in_predicate,
    };

    let obj = Literal::Typed {
        value: v,
        datatype: NamedNode {
            iri: "xsd:dateTime",
        },
    };

    Triple {
        subject: subject.into(),
        predicate,
        object: obj.into(),
    }
}

fn from_uri<'a>(id: &'a str, in_predicate: &'a str, v: &'a str) -> Triple<'a> {
    let subject = NamedNode {
        iri: id,
    };

    let predicate = NamedNode {
        iri: in_predicate,
    };

    let obj = NamedNode {
        iri: v,
    };

    Triple {
        subject: subject.into(),
        predicate,
        object: obj.into(),
    }
}

fn from_string<'a>(id: &'a str, in_predicate: &'a str, s: &'a str, l: Lang) -> Triple<'a> {
    let subject = NamedNode {
        iri: id,
    };

    let predicate = NamedNode {
        iri: in_predicate,
    };

    let obj = match l {
        Lang::NONE => Literal::Simple {
            value: s,
        },
        Lang::RU => Literal::LanguageTaggedString {
            value: s,
            language: "ru",
        },
        Lang::EN => Literal::LanguageTaggedString {
            value: s,
            language: "en",
        },
    };

    Triple {
        subject: subject.into(),
        predicate,
        object: obj.into(),
    }
}

fn format_resources(subject: &str, predicate: &str, resources: &[Resource], formatter: &mut TurtleFormatterWithPrefixes<Vec<u8>>) -> Result<(), io::Error> {
    for r in resources {
        match r.rtype {
            DataType::Boolean => {
                formatter.format(&from_boolean(subject, &predicate, &r.get_bool().to_string()))?;
            }
            DataType::Integer => {
                formatter.format(&from_integer(subject, &predicate, &r.get_int().to_string()))?;
            }
            DataType::Uri => {
                formatter.format(&from_uri(subject, &predicate, &r.get_uri()))?;
            }
            DataType::String => {
                formatter.format(&from_string(subject, &predicate, r.get_str(), r.get_lang()))?;
            }
            DataType::Datetime => {
                formatter.format(&from_datetime(subject, &predicate, &format!("{:?}", &Utc.timestamp(r.get_datetime(), 0))))?;
            }
            DataType::Decimal => {
                let (m, e) = r.get_num();
                let c = exponent_to_scale(&m, &e);
                let d = Decimal::new(c.0, c.1);
                formatter.format(&from_decimal(subject, &predicate, &format!("{:?}", d.to_string())))?;
            }
            _ => {}
        }
    }
    Ok(())
}

pub fn get_prefix(v: &str) -> Option<&str> {
    if let Some(el) = v.split(':').next() {
        return Some(el);
    }
    None
}

fn collect_prefix(v: &str, all_prefixes: &HashMap<String, String>, used_prefixes: &mut HashMap<String, String>) {
    if let Some(p) = get_prefix(v) {
        if !used_prefixes.contains_key(p) {
            if let Some(up) = all_prefixes.get(&(p.to_owned() + ":")) {
                used_prefixes.insert(p.to_owned(), up.to_owned());
            }
        }
    }
}

fn extract_prefixes(indvs: &[Individual], all_prefixes: &HashMap<String, String>) -> HashMap<String, String> {
    let mut used_prefixes = HashMap::new();

    for indv in indvs.iter() {
        collect_prefix(indv.get_id(), &all_prefixes, &mut used_prefixes);
        for (predicate, resources) in &indv.obj.resources {
            collect_prefix(predicate, &all_prefixes, &mut used_prefixes);
            for r in resources {
                if let DataType::Uri = r.rtype {
                    collect_prefix(&r.get_uri(), &all_prefixes, &mut used_prefixes);
                }
            }
        }
    }

    used_prefixes
}

pub fn to_turtle(indvs: Vec<Individual>, all_prefixes: &mut HashMap<String, String>) -> Result<Vec<u8>, io::Error> {
    let used_prefixes = extract_prefixes(&indvs, all_prefixes);
    let mut formatter = TurtleFormatterWithPrefixes::new(Vec::default(), &used_prefixes);

    for indv in indvs.iter() {
        for (predicate, resources) in &indv.obj.resources {
            if predicate == "rdf:type" {
                format_resources(&indv.get_id(), predicate, resources, &mut formatter)?;
                break;
            }
        }
        for (predicate, resources) in &indv.obj.resources {
            if predicate == "rdf:type" || predicate == "v-s:updateCounter" {
                continue;
            }
            format_resources(&indv.get_id(), predicate, resources, &mut formatter)?;
        }
    }

    formatter.finish()
}
