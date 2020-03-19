#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use sled::{Db, Serialize};
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

    let path = "./data";

    let module_info = ModuleInfo::new(path, "ontologist", true);
    if module_info.is_err() {
        error!("{:?}", &module_info.err());
        return Ok(());
    }

    let mut ctx = Context {
        last_found_changes: Instant::now(),
        query,
        ontology_file_path: ontology_file_path.to_owned(),
        onto_types: onto_types.iter().map(|s| String::from(*s)).collect(),
        onto_index: sled::open(path.to_owned() + "/onto-index").unwrap(),
        is_need_generate: false,
    };

    if ctx.onto_index.len() == 0 {
        recover_index_from_ft(&mut ctx, &mut module);
    }

    let mut queue_consumer = Consumer::new("./data/queue", "ontologist", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_batch as fn(&mut Module, &mut Context, batch_size: u32) -> Option<u32>),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual) -> Result<bool, PrepareError>),
        &mut (after_batch as fn(&mut Module, &mut Context, prepared_batch_size: u32) -> bool),
        &mut (heartbeat as fn(&mut Module, &mut Context)),
    );
    Ok(())
}

pub struct Context {
    last_found_changes: Instant,
    query: String,
    ontology_file_path: String,
    onto_types: Vec<String>,
    onto_index: Db,
    is_need_generate: bool,
}

fn heartbeat(module: &mut Module, ctx: &mut Context) {
    if !Path::new(&ctx.ontology_file_path).exists() {
        generate_file(ctx, module);
    } else {
        if ctx.is_need_generate && Instant::now().duration_since(ctx.last_found_changes).as_secs() > 5 {
            if generate_file(ctx, module) {
                ctx.is_need_generate = false;
            }
        }
    }
}

fn before_batch(_module: &mut Module, _ctx: &mut Context, _size_batch: u32) -> Option<u32> {
    None
}

fn after_batch(_module: &mut Module, _ctx: &mut Context, _prepared_batch_size: u32) -> bool {
    true
}

fn prepare(_module: &mut Module, _module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) -> Result<bool, PrepareError> {
    if let Some((id, counter, is_deleted)) = test_on_onto(queue_element, &ctx.onto_types) {
        ctx.last_found_changes = Instant::now();
        ctx.is_need_generate = true;
        info!("update onto index, id={}, counter={}, is_deleted={}", id, counter, is_deleted);

        if is_deleted {
            if let Err(e) = ctx.onto_index.remove(id) {
                error!("fail remove from onto index, err={}", e);
            }
        } else {
            if let Err(e) = ctx.onto_index.insert(id, counter.serialize()) {
                error!("fail update onto index, err={}", e);
            }
        }
    }

    return Ok(true);
}

fn recover_index_from_ft(ctx: &mut Context, module: &mut Module) -> bool {
    info!("recover index from ft");
    info!("query={}", ctx.query);

    let res = module.fts.query(FTQuery::new_with_user("cfg:VedaSystem", &ctx.query));
    if res.result_code == ResultCode::Ok && res.count > 0 {
        if let Err(e) = ctx.onto_index.clear() {
            error!("fail clean index, err={}", e);
            return false;
        }

        for id in &res.result {
            if let Err(e) = ctx.onto_index.insert(id, "+") {
                error!("fail create onto index, err={}", e);
                break;
            }
        }
    } else {
        error!("search return empty set, query: {}", &ctx.query);
    }

    false
}

fn generate_file(ctx: &mut Context, module: &mut Module) -> bool {
    info!("generate file use onto index");

    let mut indvs_count = 0;
    let mut buf = String::new();

    buf.push('[');
    let key = [];

    for kv in ctx.onto_index.range(key..) {
        if let Ok((k, _v)) = kv {
            let id = String::from_utf8_lossy(k.as_ref());

            let mut rindv: Individual = Individual::default();
            if module.storage.get_individual(id.as_ref(), &mut rindv) {
                rindv.parse_all();

                if buf.len() > 1 {
                    buf.push(',');
                    buf.push('\n');
                }

                buf.push_str(&rindv.get_obj().as_json_str());
                indvs_count += 1;
            } else {
                error!("fail read, uri={}", id.as_ref());
            }
        }
    }

    buf.push(']');

    if let Ok(mut file) = File::create(&ctx.ontology_file_path) {
        if let Err(e) = file.write_all(buf.as_bytes()) {
            error!("fail write to file {:?}", e);
        } else {
            info!("stored: count:{}, bytes:{}", indvs_count, buf.len());
            return true;
        }
    } else {
        error!("fail create file {}", ctx.ontology_file_path);
    }

    false
}

fn test_on_onto(queue_element: &mut Individual, onto_types: &Vec<String>) -> Option<(String, i64, bool)> {
    if parse_raw(queue_element).is_ok() {
        let wcmd = queue_element.get_first_integer("cmd");
        if wcmd.is_none() {
            return None;
        }
        let cmd = IndvOp::from_i64(wcmd.unwrap_or_default());

        let mut prev_state = Individual::default();
        get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

        let mut new_state = Individual::default();
        get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

        if cmd != IndvOp::Remove {
            let is_deleted = new_state.is_exists_bool("v-s:deleted", true);
            if new_state.any_exists_v("rdf:type", &onto_types) {
                info!("found onto changes from storage: uri={}", new_state.get_id());
                return Some((new_state.get_id().to_owned(), new_state.get_first_integer("v-s:updateCounter").unwrap_or_default(), is_deleted));
            }
        } else {
            if prev_state.any_exists_v("rdf:type", &onto_types) {
                info!("found onto changes from storage: uri={}", prev_state.get_id());
                return Some((prev_state.get_id().to_owned(), prev_state.get_first_integer("v-s:updateCounter").unwrap_or_default(), true));
            }
        }
    }

    None
}
