#[macro_use]
extern crate log;

use chrono::prelude::*;
use env_logger::Builder;
use log::LevelFilter;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::io::{Error, ErrorKind};
use std::path::Path;
use std::process::Command;
use sysinfo::{ProcessExt, SystemExt};

#[derive(Debug)]
struct Module {
    name: String,
    exec_name: String,
    args: String,
    order: u32,
    is_enabled: bool,
}

fn main() {
    let env_var = "RUST_LOG";
    match std::env::var_os(env_var) {
        Some(val) => println!("use env var: {}: {:?}", env_var, val.to_str()),
        None => std::env::set_var(env_var, "info"),
    }

    Builder::new()
        .format(|buf, record| writeln!(buf, "{} [{}] - {}", Local::now().format("%Y-%m-%dT%H:%M:%S%.3f"), record.level(), record.args()))
        .filter(None, LevelFilter::Info)
        .init();

    let modules = get_modules_info();
    if modules.is_err() {
        error!("fail read modules info, err={:?}", modules.err());
        return;
    }
    let modules = modules.unwrap();

    let mut sys = sysinfo::System::new();

    println!("total memory: {} kB", sys.get_total_memory());
    println!("used memory : {} kB", sys.get_used_memory());
    println!("total swap  : {} kB", sys.get_total_swap());
    println!("used swap   : {} kB", sys.get_used_swap());

    sys.refresh_processes();
    for (pid, proc) in sys.get_process_list() {
        if proc.name().starts_with("veda") && proc.name() != "veda-bootstrap" {
            error!("unable start, found other running process: pid={}, {:?} ({:?}) ", pid, proc.exe(), proc.status());
            return;
        }
    }

    let mut vmodules: Vec<&Module> = Vec::new();
    for el in modules.values() {
        vmodules.push(el);
    }
    vmodules.sort_by(|a, b| a.order.partial_cmp(&b.order).unwrap());

    let started = start_modules(vmodules);
    info!("started {:?}", started);
}

fn start_modules(modules: Vec<&Module>) -> io::Result<Vec<(String, u32)>> {
    let mut res = Vec::new();
    for module in modules {
        info!("start {:?}", module);
        let child;
        if module.args.is_empty() {
            child = Command::new(module.exec_name.to_string()).spawn();
        } else {
            child = Command::new(module.exec_name.to_string()).arg(&module.args).spawn();
        }
        if child.is_err() {
            return Err(Error::new(ErrorKind::Other, format!("fail execute {}, err={:?}", module.exec_name, child.err())));
        } else {
            let pid = child.unwrap().id();
            info!("{} start module {}, {}, {}", pid, module.name, module.exec_name, module.args);
            res.push((module.name.to_owned(), pid));
        }
    }
    Ok(res)
}

fn get_modules_info() -> io::Result<HashMap<String, Module>> {
    let mut modules: HashMap<String, Module> = HashMap::new();
    let path = "./";

    let f = File::open("veda.modules")?;
    let file = &mut BufReader::new(&f);

    let mut order = 0;
    loop {
        if let Some(l) = file.lines().next() {
            if let Ok(line) = l {
                if line.starts_with('#') || line.starts_with('\t') || line.starts_with('\n') || line.starts_with(' ') || line.is_empty() {
                    continue;
                }

                let mut params = HashMap::new();

                loop {
                    if let Some(p) = file.lines().next() {
                        if let Ok(p) = p {
                            if p.starts_with('\t') || p.starts_with(' ') {
                                info!("param={}", p);
                                if let Some(eq_pos) = p.find('=') {
                                    let nm: &str = &p[0..eq_pos].trim();
                                    let vl: &str = &p[eq_pos + 1..].trim();

                                    params.insert(nm.to_string(), vl.to_string());
                                }
                            }
                        }
                    } else {
                        break;
                    }
                }

                let mut module = Module {
                    name: line.to_string(),
                    args: String::new(),
                    order,
                    is_enabled: true,
                    exec_name: String::new(),
                };
                order += 1;

                if let Some(m) = params.get("args") {
                    module.args = m.to_owned();
                }

                let module_name = if let Some(m) = params.get("module") {
                    "veda-".to_string() + m
                } else {
                    "veda-".to_string() + line.trim()
                };

                if Path::new(&module_name).exists() {
                    module.exec_name = path.to_string() + &module_name;
                    modules.insert(line, module);
                } else {
                    return Err(Error::new(ErrorKind::Other, format!("not found module [{:?}]", module_name)));
                }
            }
        } else {
            break;
        }
    }
    Ok(modules)
}
