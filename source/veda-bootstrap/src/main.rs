#[macro_use]
extern crate log;

use chrono::prelude::*;
use env_logger::Builder;
use log::LevelFilter;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::io::{Error, ErrorKind};
use std::path::Path;
use std::process::{Child, Command};
use std::{io, thread, time};
use sysinfo::{ProcessExt, ProcessStatus, SystemExt};

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
    sys.refresh_processes();
    for (pid, proc) in sys.get_process_list() {
        if proc.name().starts_with("veda") && proc.name() != "veda-bootstrap" && proc.name() != "veda" {
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

    if started.is_err() {
        error!("veda not started, exit. err={:?}", started.err());
        return;
    }

    watch_started_modules(&modules, &mut started.unwrap());
    //info!("started {:?}", started);
}

fn is_ok_process(sys: &mut sysinfo::System, pid: u32) -> bool {
    if let Some(proc) = sys.get_process(pid as i32) {
        match proc.status() {
            ProcessStatus::Idle => true,
            ProcessStatus::Run => true,
            ProcessStatus::Sleep => true,
            _ => false,
        }
    } else {
        false
    }
}

fn watch_started_modules(modules: &HashMap<String, Module>, processes: &mut Vec<(String, Child)>) {
    loop {
        let mut sys = sysinfo::System::new();
        sys.refresh_processes();
        for (name, process) in processes.iter_mut() {
            if !is_ok_process(&mut sys, process.id()) {
                error!("found dead module {} {}", process.id(), name);

                if let Ok(_0) = process.kill() {
                    warn!("attempt stop module {} {}", process.id(), name);
                }

                if let Some(module) = modules.get(name) {
                    let child = start_module(module);
                    if child.is_err() {
                        error!("fail execute {}, err={:?}", module.exec_name, child.err());
                    } else {
                        let child = child.unwrap();
                        info!("{} restart module {}, {}, {}", child.id(), module.name, module.exec_name, module.args);
                        *process = child;
                    }
                } else {
                    error!("? internal error, not found module {}", name)
                }
            }
        }
        thread::sleep(time::Duration::from_millis(1000));
    }
}

fn start_module(module: &Module) -> io::Result<Child> {
    let datetime: DateTime<Local> = Local::now();
    let log_path = "logs/veda-".to_owned() + &module.name + "-" + &datetime.format("%Y-%m-%d %H:%M:%S.%f").to_string();
    let std_log_file = File::create(log_path.to_string());
    let err_log_file = File::create(log_path);

    let child = if module.args.is_empty() {
        Command::new(module.exec_name.to_string()).stdout(std_log_file.unwrap()).stderr(err_log_file.unwrap()).spawn()
    } else {
        Command::new(module.exec_name.to_string()).stdout(std_log_file.unwrap()).stderr(err_log_file.unwrap()).arg(&module.args).spawn()
    };

    match child {
        Ok(p) => {
            if let Ok(mut file) = File::create(".pids/__".to_owned() + &module.name + "-pid") {
                if let Err(e) = file.write_all(format!("{}", p.id()).as_bytes()) {
                    error!("can not create pid file for {} {}, err={:?}", &module.name, p.id(), e);
                }
            }
            Ok(p)
        }
        Err(e) => Err(e),
    }
}

fn start_modules(modules: Vec<&Module>) -> io::Result<Vec<(String, Child)>> {
    let mut started_processes = Vec::new();
    for module in modules {
        //info!("start {:?}", module);
        let child = start_module(module);
        if child.is_err() {
            return Err(Error::new(ErrorKind::Other, format!("fail execute {}, err={:?}", module.exec_name, child.err())));
        } else {
            let child = child.unwrap();
            info!("{} start module {}, {}, {}", child.id(), module.name, module.exec_name, module.args);
            started_processes.push((module.name.to_owned(), child));
        }
    }

    let mut sys = sysinfo::System::new();
    thread::sleep(time::Duration::from_millis(500));
    sys.refresh_processes();
    let mut success_started = 0;
    for (name, process) in started_processes.iter() {
        if is_ok_process(&mut sys, process.id()) {
            success_started += 1;
        } else {
            error!("fail start: {} {}", process.id(), name)
        }
    }

    if success_started < started_processes.len() {
        for (name, process) in started_processes.iter_mut() {
            if let Ok(_0) = process.kill() {
                warn!("stop process {} {}", process.id(), name);
            }
        }

        return Err(Error::new(ErrorKind::Other, "fail start"));
    }

    Ok(started_processes)
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
                            } else {
                                break;
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
