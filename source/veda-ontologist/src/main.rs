#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::time::Instant;
use v_api::app::ResultCode;
use v_api::IndvOp;
use v_module::info::ModuleInfo;
use v_module::module::*;
use v_onto::{individual::*, parser::*};
use v_queue::consumer::*;
use v_search::ft_client::FTQuery;

fn main() -> std::io::Result<()> {
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

    let onto_types = vec![
        "rdfs:Class",
        "owl:Class",
        "rdfs:Datatype",
        "owl:Ontology",
        "rdf:Property",
        "owl:DatatypeProperty",
        "owl:ObjectProperty",
        "owl:OntologyProperty",
        "owl:AnnotationProperty",
        "v-ui:PropertySpecification",
        "v-ui:DatatypePropertySpecification",
        "v-ui:ObjectPropertySpecification",
        "v-ui:TemplateSpecification",
        "v-ui:ClassModel",
    ];

    let mut query = String::new();

    for el in &onto_types {
        if !query.is_empty() {
            query.push_str(" || ");
        }
        query.push_str("'rdf:type' === '");
        query.push_str(el);
        query.push_str("'");
    }

    //    while !ft_client.connect() {
    //        thread::sleep(time::Duration::from_millis(3000));
    //    }

    let ontology_file_path = "public/ontology.json";

    wait_load_ontology();

    let module_info = ModuleInfo::new("./data", "ontologist", true);
    if module_info.is_err() {
        error!("{:?}", &module_info.err());
        return Ok(());
    }

    let mut ctx = Context {
        is_found_onto_changes: false,
        last_found_changes: Instant::now(),
        query,
        ontology_file_path: ontology_file_path.to_owned(),
        onto_types: onto_types.iter().map(|s| String::from(*s)).collect(),
    };

    let mut queue_consumer = Consumer::new("./data/queue", "ontologist", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_batch as fn(&mut Module, &mut Context, batch_size: u32) -> Option<u32>),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual) -> Result<bool, PrepareError>),
        &mut (after_batch as fn(&mut Module, &mut Context, prepared_batch_size: u32) -> bool),
    );
    Ok(())
}

pub struct Context {
    is_found_onto_changes: bool,
    last_found_changes: Instant,
    query: String,
    ontology_file_path: String,
    onto_types: Vec<String>,
}

fn before_batch(_module: &mut Module, _ctx: &mut Context, _size_batch: u32) -> Option<u32> {
    None
}

fn after_batch(_module: &mut Module, _ctx: &mut Context, _prepared_batch_size: u32) -> bool {
    true
}

fn prepare(module: &mut Module, _module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) -> Result<bool, PrepareError> {
    if !Path::new(&ctx.ontology_file_path).exists() {
        ctx.is_found_onto_changes = true;
    }

    let prev_found_changes = ctx.last_found_changes;

    if !ctx.is_found_onto_changes {
        ctx.is_found_onto_changes = is_changes(queue_element, &ctx.onto_types);

        if ctx.is_found_onto_changes {
            ctx.last_found_changes = Instant::now();
        }
    }

    if ctx.is_found_onto_changes && Instant::now().duration_since(prev_found_changes).as_secs() > 5 {
        if generate_file(module, &ctx.query, &ctx.ontology_file_path) {
            ctx.is_found_onto_changes = false;
        }
    }

    return Ok(true);
}

fn generate_file(module: &mut Module, query: &str, ontology_file_path: &str) -> bool {
    info!("prepare changes");
    info!("query={}", query);

    let res = module.fts.query(FTQuery::new_with_user("cfg:VedaSystem", query));
    if res.result_code == ResultCode::Ok && res.count > 0 {
        let mut indvs = Vec::new();

        for el in &res.result {
            let mut rindv: Individual = Individual::default();
            if module.storage.get_individual(&el, &mut rindv) {
                rindv.parse_all();
                indvs.push(rindv);
            } else {
                error!("fail read, uri={}", &el);
            }
        }

        let mut buf = String::new();

        buf.push('[');
        for el in indvs.iter() {
            if buf.len() > 1 {
                buf.push(',');
            }

            buf.push_str(&el.get_obj().as_json_str());
        }
        buf.push(']');

        if let Ok(mut file) = File::create(ontology_file_path) {
            if let Err(e) = file.write_all(buf.as_bytes()) {
                error!("fail write to file {:?}", e);
            } else {
                info!("stored: count:{}, bytes:{}", indvs.len(), buf.len());
                return true;
            }
        } else {
            error!("fail create file {}", ontology_file_path);
        }
    } else {
        error!("search return empty set, query: {}", &query);
    }

    false
}

fn is_changes(qel: &mut Individual, onto_types: &Vec<String>) -> bool {
    if parse_raw(qel).is_ok() {
        if let Some(new_state) = qel.get_first_binobj("new_state") {
            let mut indv: Individual = Individual::default();
            indv.set_raw(new_state.as_slice());

            if parse_raw(&mut indv).is_ok() {
                if indv.any_exists_v("rdf:type", &onto_types) {
                    info!("found onto changes from storage: uri={}", indv.get_id());
                    return true;
                }
            }
        } else {
            let wcmd = qel.get_first_integer("cmd");
            if wcmd.is_none() {
                return false;
            }
            let cmd = IndvOp::from_i64(wcmd.unwrap_or_default());

            if cmd != IndvOp::Remove {
                warn!("not found field [new_state], indv={}", qel.parse_all());
            }
        }
    }
    false
}
