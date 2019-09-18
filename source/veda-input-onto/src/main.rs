#[macro_use]
extern crate log;

use chrono::Local;
use crossbeam_channel::unbounded;
use env_logger::Builder;
use log::LevelFilter;
use notify::{RecommendedWatcher, RecursiveMode, Result as NotifyResult, Watcher};
use rio_api::parser::TriplesParser;
use rio_turtle::{TurtleError, TurtleParser};
use std::collections::HashMap;
use std::fs::{DirEntry, File};
use std::io::BufReader;
use std::io::Write;
use std::path::Path;
use std::time::Duration;
use std::{fs, io};
use v_module::module::*;
use v_module::onto::*;
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

    let mut list_candidate_files: Vec<String> = Vec::new();

    let onto_path = "ontology".to_owned();

    let mut onto = Onto::default();
    info!("load onto start");
    load_onto(&mut module.fts, &mut module.storage, &mut onto);
    info!("load onto end");
    if onto.relations.len() > 0 {
        info!("ontology not found");
        collect_file_paths(&onto_path, &mut list_candidate_files);
    }

    if !list_candidate_files.is_empty() {
        processing_files(list_candidate_files);
    }

    let (tx, rx) = unbounded();
    let mut watcher: RecommendedWatcher = Watcher::new(tx, Duration::from_secs(2))?;

    watcher.watch(onto_path, RecursiveMode::Recursive)?;

    loop {
        match rx.recv() {
            Ok(event) => println!("changed: {:?}", event),
            Err(err) => println!("watch error: {:?}", err),
        };
    }
}

fn collect_file_paths(onto_path: &str, res: &mut Vec<String>) {
    fn prepare_file(d: &DirEntry, res: &mut Vec<String>) {
        let path = d.path().as_path().to_owned();
        if let Some(ext) = path.extension() {
            if let Some(ext) = ext.to_str() {
                if ext != "ttl" {
                    return;
                }
            }
        }
        if let Some(path) = path.to_str() {
            res.push(path.to_owned());
        }
    }
    visit_dirs(Path::new(&onto_path), res, &prepare_file).unwrap_or_default();
}

fn processing_files(files: Vec<String>) {
    for file in files {
        parse_file(&file);
    }
}

fn parse_file(file_path: &str) {
    let mut individuals: HashMap<String, Individual> = HashMap::new();

    let mut parser = TurtleParser::new(BufReader::new(File::open(file_path).unwrap()), "").unwrap();

    let mut namespaces: HashMap<String, String> = HashMap::new();
    loop {
        for ns in &parser.namespaces {
            if !namespaces.contains_key(ns.1) {
                namespaces.insert(ns.1.clone(), ns.0.clone());
            }
        }

        let res = parser.parse_step(&mut |t| {
            //info!("namespaces: {:?}", namespaces);
            let indv = individuals.entry(t.subject.to_string()).or_default();

            if indv.obj.uri.is_empty() {
                indv.obj.uri = t.subject.to_string();
            }

            let predicate = t.predicate.iri;

            info!("{:?}", predicate);
            Ok(()) as Result<(), TurtleError>
        });

        if let Err(e) = res {
            error!("fail parse ttl, err={}", e);
            break;
        }

        if parser.is_end() {
            break;
        }
    }

    for indv in individuals {
        info!("ind: {}", indv.1);
    }
}

fn visit_dirs(dir: &Path, res: &mut Vec<String>, cb: &dyn Fn(&DirEntry, &mut Vec<String>)) -> io::Result<()> {
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
