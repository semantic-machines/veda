#[macro_use]
extern crate log;

use chrono::offset::LocalResult::Single;
use chrono::{DateTime, Local, NaiveDateTime, TimeZone};
use crossbeam_channel::unbounded;
use env_logger::Builder;
use log::LevelFilter;
use md5::{Digest, Md5};
use notify::{EventKind, RecommendedWatcher, RecursiveMode, Result as NotifyResult, Watcher};
use rio_api::model::Literal::{LanguageTaggedString, Simple, Typed};
use rio_api::model::NamedOrBlankNode;
use rio_api::model::Term::{BlankNode, Literal, NamedNode};
use rio_api::parser::TriplesParser;
use rio_turtle::{TurtleError, TurtleParser};
use rust_decimal::Decimal;
use std::collections::HashMap;
use std::fs::{DirEntry, File};
use std::io::BufReader;
use std::io::Write;
use std::ops::Sub;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time as std_time;
use std::{fs, io};
use v_api::*;
use v_module::module::*;
use v_module::onto::*;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;
use v_onto::onto::*;

fn main() -> NotifyResult<()> {
    let env_var = "RUST_LOG";
    match std::env::var_os(env_var) {
        Some(val) => println!("use env var: {}: {:?}", env_var, val.to_str()),
        None => std::env::set_var(env_var, "info"),
    }

    Builder::new()
        .format(|buf, record| writeln!(buf, "{} [{}] - {}", Local::now().format("%Y-%m-%dT%H:%M:%S%.3f"), record.level(), record.args()))
        .filter(None, LevelFilter::Info)
        .init();

    let mut module = Module::default();

    let systicket;
    if let Ok(t) = module.get_sys_ticket_id() {
        systicket = t;
    } else {
        error!("fail get systicket");
        return Ok(());
    }

    let mut list_candidate_files: Vec<PathBuf> = Vec::new();

    let onto_path = "ontology".to_owned();

    let mut onto = Onto::default();
    info!("load onto start");
    load_onto(&mut module.fts, &mut module.storage, &mut onto);
    info!("load onto end");
    if onto.relations.is_empty() {
        info!("ontology not found");
        collect_file_paths(&onto_path, &mut list_candidate_files);
    }

    info!("start prepare files");

    if !list_candidate_files.is_empty() {
        processing_files(list_candidate_files, &mut module, &systicket);
    }

    info!("watch file changes...");

    let (tx, rx) = unbounded();
    let mut watcher: RecommendedWatcher = Watcher::new(tx, std_time::Duration::from_secs(2))?;

    watcher.watch(onto_path, RecursiveMode::Recursive)?;

    loop {
        match rx.recv() {
            Ok(w_event) => {
                if let Ok(event) = w_event {
                    match event.kind {
                        EventKind::Create(_) | EventKind::Modify(_) => {
                            if event.flag().is_some() {
                                println!("changed: {:?}", event);
                                println!("paths {:?}", event.paths);
                                processing_files(event.paths, &mut module, &systicket);
                            }
                        }
                        _ => {}
                    }
                }
            }
            Err(err) => println!("watch error: {:?}", err),
        };
    }
}

fn get_hash_of_file(file_path: &str) -> io::Result<String> {
    let mut rfile = File::open(&file_path)?;
    let mut hasher = Md5::new();
    io::copy(&mut rfile, &mut hasher)?;
    Ok(hex::encode(hasher.result()).to_uppercase())
}

fn extract_path_and_name(path: &PathBuf) -> Option<(&str, &str)> {
    let sfp;
    if let Some(s) = path.to_str() {
        sfp = s;
    } else {
        return None;
    }

    if let Some(s) = path.file_name() {
        if let Some(ss) = s.to_str() {
            return Some((sfp, ss));
        }
    }

    None
}

fn processing_files(file_paths: Vec<PathBuf>, module: &mut Module, systicket: &str) {
    let mut file2indv: HashMap<String, HashMap<String, Individual>> = HashMap::new();
    let mut priority_list: Vec<(i64, String, String)> = Vec::new();

    for file_path in file_paths {
        let path;
        let name;
        if let Some(w) = extract_path_and_name(&file_path) {
            path = w.0;
            name = w.1;
        } else {
            continue;
        }

        let mut file_need_for_load = true;
        let mut file_info_indv: Individual = Individual::default();
        let new_id = "d:".to_string() + name;

        let new_hash = match get_hash_of_file(path) {
            Ok(new_h) => {
                if module.get_individual(&new_id, &mut file_info_indv).is_some() {
                    if let Some(old_h) = file_info_indv.get_first_literal("v-s:hash") {
                        if old_h == new_h {
                            file_need_for_load = false;
                        }
                    }
                }
                Some(new_h)
            }
            Err(e) => {
                error!("fail calculate HASH of file {}, err={}", &path, e);
                None
            }
        };
        file_info_indv.set_id(&new_id);

        if file_need_for_load {
            let mut individuals = file2indv.entry(path.to_owned()).or_default();
            let (onto_id, _onto_url, load_priority) = parse_file(path, &mut individuals);
            //        info!("ontology: {} {} {}", &file, onto_id, load_priority);
            full_file_info_indv(&onto_id, individuals, &mut file_info_indv, new_hash, path, name);
            priority_list.push((load_priority, onto_id, path.to_owned()));
        }
    }

    priority_list.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    //info!("priority_list: {:?}", priority_list);

    for (load_priority, _onto_id, path) in priority_list {
        if let Some(indvs) = file2indv.get_mut(&path) {
            for indv_file in indvs.values_mut() {
                if !indv_file.is_exists("rdf:type") {
                    error!("{}: [{}] not contain [rdf:type], ignore it !!!", path, indv_file.get_id());
                    continue;
                }

                let is_need_store = if let Some(indv_db) = module.get_individual(indv_file.get_id(), &mut Individual::default()) {
                    indv_db.parse_all();
                    if !indv_db.compare(indv_file, vec!["v-s:updateCounter", "v-s:previousVersion", "v-s:actualVersion", "v-s:fullUrl"]) {
                        true
                    } else {
                        false
                    }
                } else {
                    true
                };

                if is_need_store {
                    let res = module.api.update(systicket, IndvOp::Put, &indv_file);

                    if res.result != ResultCode::Ok {
                        error!("fail update, {}, file={}, uri={}, result_code={:?}", load_priority, path, indv_file.get_id(), res.result);
                    } else {
                        info!("success update, {}, file={}, uri={}", load_priority, path, indv_file.get_id());
                    }
                }
            }
        }
    }

    info!("end prepare {} files", file2indv.len());
}

fn full_file_info_indv(onto_id: &str, individuals: &mut HashMap<String, Individual>, new_indv: &mut Individual, hash: Option<String>, path: &str, name: &str) {
    if let Some(h) = hash {
        new_indv.set_string("v-s:hash", &h, Lang::NONE);
    }
    new_indv.set_uri("rdf:type", "v-s:TTLFile");
    //    new_indv.obj.set_uri("v-s:created", Resource(DataType.Datetime, Clock.currTime().toUnixTime()));
    new_indv.set_uri("v-s:filePath", path);
    new_indv.set_uri("v-s:fileUri", name);
    new_indv.clear("v-s:resource");

    for indv in individuals.values_mut() {
        new_indv.add_uri("v-s:resource", &indv.get_id());

        if !indv.is_exists("rdfs:isDefinedBy") {
            indv.set_uri("rdfs:isDefinedBy", onto_id);
        }
    }
}

fn parse_file(file_path: &str, individuals: &mut HashMap<String, Individual>) -> (String, String, i64) {
    let mut parser = TurtleParser::new(BufReader::new(File::open(file_path).unwrap()), "").unwrap();

    let mut namespaces2id: HashMap<String, String> = HashMap::new();
    let mut id2orignamespaces: HashMap<String, String> = HashMap::new();
    let mut id2namespaces: HashMap<String, String> = HashMap::new();

    let mut onto_id = String::default();
    let mut onto_url = String::default();
    let mut load_priority = 999;

    loop {
        for ns in &parser.namespaces {
            if !namespaces2id.contains_key(ns.1) {
                if let Some(s) = ns.1.get(0..ns.1.len() - 1) {
                    namespaces2id.insert(s.to_owned(), ns.0.clone());
                    id2orignamespaces.insert(ns.0.to_owned() + ":", ns.1.to_owned());
                    id2namespaces.insert(ns.0.to_owned() + ":", s.to_string());
                }
            }
        }

        let mut id = String::default();
        let mut idx = 0;
        let res = parser.parse_step(&mut |t| {
            //info!("namespaces: {:?}", namespaces);

            let subject = match t.subject {
                NamedOrBlankNode::BlankNode(n) => n.id,
                NamedOrBlankNode::NamedNode(n) => n.iri,
            };

            let s = to_prefix_form(&subject, &namespaces2id);
            if s.is_empty() {
                error!("invalid subject={:?}", subject);
            }

            let indv = individuals.entry(s.to_owned()).or_default();

            if indv.get_id().is_empty() {
                id.insert_str(0, &s);
                indv.set_id (&s);
            }

            let predicate = to_prefix_form(t.predicate.iri, &namespaces2id);

            //info!("[{:?}]", predicate);
            match t.object {
                BlankNode(n) => error!("BlankNode {}", n.id),
                NamedNode(n) => indv.add_uri(&predicate, &to_prefix_form(n.iri, &namespaces2id)),

                Literal(l) => match l {
                    Simple {
                        value,
                    } => indv.add_string(&predicate, value, Lang::NONE),
                    LanguageTaggedString {
                        value,
                        language,
                    } => indv.add_string(&predicate, value, Lang::from_str(language)),
                    Typed {
                        value,
                        datatype,
                    } => match datatype.iri {
                        "http://www.w3.org/2001/XMLSchema#string" => {
                            indv.add_string(&predicate, value, Lang::NONE);
                        }
                        "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" => {
                            if let Ok(v) = value.parse::<i64>() {
                                indv.add_integer(&predicate, v);
                            } else {
                                error!("fail parse [{}] to integer", value);
                            }
                        }
                        "http://www.w3.org/2001/XMLSchema#integer" => {
                            if let Ok(v) = value.trim().parse::<i64>() {
                                indv.add_integer(&predicate, v);
                            } else {
                                error!("fail parse [{}] to integer", value);
                            }
                        }
                        "http://www.w3.org/2001/XMLSchema#boolean" => {
                            if let Ok(v) = value.parse::<bool>() {
                                indv.add_bool(&predicate, v);
                            } else {
                                error!("fail parse [{}] to bool", value);
                            }
                        }
                        "http://www.w3.org/2001/XMLSchema#decimal" => {
                            let qq = Decimal::from_str(value);
                            if let Ok(v) = qq {
                                let exp = v.scale() as i32 * -1;
                                if let Ok(m) = value.replace('.', "").parse::<i64>() {
                                    indv.add_decimal_d(&predicate, m, exp as i64);
                                    //                                    info!("{}{}", m, exp);
                                }
                            } else {
                                error!("fail parse [{}] to decimal", value);
                            }
                        }
                        "http://www.w3.org/2001/XMLSchema#dateTime" => {
                            if value.contains('Z') {
                                if let Ok(v) = DateTime::parse_from_rfc3339(&value) {
                                    indv.add_datetime(&predicate, v.timestamp());
                                } else {
                                    error!("fail parse [{}] to datetime", value);
                                }
                            } else {
                                let ndt;
                                if value.len() == 10 {
                                    ndt = NaiveDateTime::parse_from_str(&(value.to_owned() + "T00:00:00"), "%Y-%m-%dT%H:%M:%S");
                                } else {
                                    ndt = NaiveDateTime::parse_from_str(&value, "%Y-%m-%dT%H:%M:%S")
                                }

                                if let Ok(v) = ndt {
                                    if let Single(offset) = Local.offset_from_local_datetime(&v) {
                                        indv.add_datetime(&predicate, v.sub(offset).timestamp());
                                    } else {
                                        indv.add_datetime(&predicate, v.timestamp());
                                    }
                                } else {
                                    error!("fail parse [{}] to datetime", value);
                                }
                            }
                        }
                        _ => {
                            error!("unknown type {}", datatype.iri);
                        }
                    },
                },
            }

            idx += 1;
            Ok(()) as Result<(), TurtleError>
        });

        if let Err(e) = res {
            error!("fail parse {}, err={}", file_path, e);
            break;
        }

        if !id.is_empty() {
            let indv = individuals.entry(id).or_default();

            if indv.get_id().is_empty() {
                error!("individual not content uri");
            }

            if indv.any_exists("rdf:type", &["owl:Ontology"]) {
                if let Some(v) = indv.get_first_integer("v-s:loadPriority") {
                    load_priority = v;
                }

                if let Some(s) = id2orignamespaces.get(indv.get_id()) {
                    indv.set_string("v-s:fullUrl", &s, Lang::NONE);
                }

                if let Some(s) = id2namespaces.get(indv.get_id()) {
                    onto_url.insert_str(0, s.as_str());
                }

                onto_id.insert_str(0, indv.get_id());
            }
        }

        if parser.is_end() {
            break;
        }
    }

    //info!("{}, load {}", file_path, individuals.len());
    (onto_id, onto_url, load_priority)
}

fn to_prefix_form(iri: &str, namespaces2id: &HashMap<String, String>) -> String {
    let mut res = String::default();

    if let Some(s) = namespaces2id.get(iri) {
        res.push_str(s);
        res.push(':');
        return res;
    }

    let pos = if let Some(pos) = iri.rfind(|c| c == '#' || c == '/') {
        pos
    } else {
        return iri.to_owned();
    };

    if let Some(s) = iri.get(0..pos) {
        if let Some(s) = namespaces2id.get(s) {
            res.push_str(s);
            res.push(':');
            if let Some(s) = iri.get(pos + 1..) {
                res.push_str(s);
            }
        } else {
            return iri.to_owned();
        }
    }

    res
}

fn collect_file_paths(onto_path: &str, res: &mut Vec<PathBuf>) {
    fn prepare_file(d: &DirEntry, res: &mut Vec<PathBuf>) {
        let path = d.path().as_path().to_owned();
        if let Some(ext) = path.extension() {
            if let Some(ext) = ext.to_str() {
                if ext != "ttl" {
                    return;
                }
            }
        } else {
            return;
        }
        res.push(path);
    }
    visit_dirs(Path::new(&onto_path), res, &prepare_file).unwrap_or_default();
}

fn visit_dirs(dir: &Path, res: &mut Vec<PathBuf>, cb: &dyn Fn(&DirEntry, &mut Vec<PathBuf>)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, res, cb)?;
            } else {
                cb(&entry, res);
            }
        }
    }
    Ok(())
}
