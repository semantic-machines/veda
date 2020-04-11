use crate::datatype::*;
use crate::individual::*;
use chrono::{TimeZone, Utc};
use rio_api::formatter::TriplesFormatter;
use rio_api::model::*;
use rio_turtle::TurtleFormatter;
use rust_decimal::Decimal;
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
            iri: "http://www.w3.org/2001/XMLSchema#boolean",
        },
    };

    Triple {
        subject: subject.into(),
        predicate: predicate,
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
            iri: "http://www.w3.org/2001/XMLSchema#integer",
        },
    };

    Triple {
        subject: subject.into(),
        predicate: predicate,
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
            iri: "http://www.w3.org/2001/XMLSchema#decimal",
        },
    };

    Triple {
        subject: subject.into(),
        predicate: predicate,
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
            iri: "http://www.w3.org/2001/XMLSchema#dateTime",
        },
    };

    Triple {
        subject: subject.into(),
        predicate: predicate,
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
        predicate: predicate,
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
        predicate: predicate,
        object: obj.into(),
    }
}

pub fn to_turtle(indvs: Vec<Individual>) -> Result<Vec<u8>, io::Error> {
    let mut formatter = TurtleFormatter::new(Vec::default());

    for indv in indvs.iter() {
        for (predicate, resources) in &indv.obj.resources {
            for r in resources {
                match r.rtype {
                    DataType::Boolean => {
                        formatter.format(&from_boolean(&indv.get_id(), predicate, &r.get_bool().to_string()))?;
                    }
                    DataType::Integer => {
                        formatter.format(&from_integer(&indv.get_id(), predicate, &r.get_int().to_string()))?;
                    }
                    DataType::Uri => {
                        formatter.format(&from_uri(&indv.get_id(), predicate, &r.get_uri()))?;
                    }
                    DataType::String => {
                        formatter.format(&from_string(&indv.get_id(), predicate, r.get_str(), r.get_lang()))?;
                    }
                    DataType::Datetime => {
                        formatter.format(&from_datetime(&indv.get_id(), predicate, &format!("{:?}", &Utc.timestamp(r.get_datetime(), 0))))?;
                    }
                    DataType::Decimal => {
                        let (m, e) = r.get_num();
                        let c = exponent_to_scale(&m, &e);
                        let d = Decimal::new(c.0, c.1);
                        formatter.format(&from_decimal(&indv.get_id(), predicate, &format!("{:?}", d.to_string())))?;
                    }
                    _ => {}
                }
            }
        }
    }

    let nt = formatter.finish();
    nt
}
