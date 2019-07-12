#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use ini::Ini;
use log::LevelFilter;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::time::Instant;
use std::{thread, time};
use v_onto::{individual::*, parser::*};
use v_queue::{consumer::*, record::*};
use v_search::{FTClient, FTQuery};
use v_storage::storage::VStorage;

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

    let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");

    let section = conf.section(None::<String>).expect("fail parse veda.properties");

    let tarantool_addr = if let Some(p) = section.get("tarantool_url") {
        p.to_owned()
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    let ft_query_service_url = section.get("ft_query_service_url").expect("param [ft_query_service_url] not found in veda.properties").clone();

    info!("tarantool addr={:?}", &tarantool_addr);

    let mut storage = if !tarantool_addr.is_empty() {
        VStorage::new_tt(tarantool_addr, "veda6", "123456")
    } else {
        VStorage::new_lmdb("./data/lmdb-individuals/")
    };

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

    let mut ft_client = FTClient::new(ft_query_service_url.to_owned());

    while !ft_client.connect() {
        thread::sleep(time::Duration::from_millis(3000));
    }

    let mut queue_consumer = Consumer::new("./data/queue", "ontologist", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    let mut total_prepared_count: u64 = 0;

    let ontology_file_path = "public/ontology.json";
    ///////
    let mut is_found_onto_changes = false;
    let mut last_found_changes = Instant::now();

    loop {
        if !Path::new(ontology_file_path).exists() {
            is_found_onto_changes = true;
        }

        let mut size_batch = 0;

        // read queue current part info
        if let Err(e) = queue_consumer.queue.get_info_of_part(queue_consumer.id, true) {
            error!("{} get_info_of_part {}: {}", total_prepared_count, queue_consumer.id, e.as_str());
            continue;
        }

        if queue_consumer.queue.count_pushed - queue_consumer.count_popped == 0 {
            // if not new messages, read queue info
            queue_consumer.queue.get_info_queue();

            if queue_consumer.queue.id > queue_consumer.id {
                size_batch = 1;
            }
        } else if queue_consumer.queue.count_pushed - queue_consumer.count_popped > 0 {
            if queue_consumer.queue.id != queue_consumer.id {
                size_batch = 1;
            } else {
                size_batch = queue_consumer.queue.count_pushed - queue_consumer.count_popped;
            }
        }

        if size_batch > 0 {
            debug!("queue: batch size={}", size_batch);
        }

        for _it in 0..size_batch {
            // пробуем взять из очереди заголовок сообщения
            if !queue_consumer.pop_header() {
                break;
            }

            let mut raw = RawObj::new(vec![0; (queue_consumer.header.msg_length) as usize]);

            // заголовок взят успешно, занесем содержимое сообщения в структуру Individual
            if let Err(e) = queue_consumer.pop_body(&mut raw.data) {
                if e == ErrorQueue::FailReadTailMessage {
                    break;
                } else {
                    error!("{} get msg from queue: {}", total_prepared_count, e.as_str());
                    break;
                }
            }

            if !is_found_onto_changes {
                let mut indv = Individual::new();
                indv.raw = raw;
                is_found_onto_changes = is_changes(&mut indv, &onto_types);
                if is_found_onto_changes {
                    last_found_changes = Instant::now();
                    info!("found onto changes from storage: uri={}", indv.obj.uri);
                }
            }

            queue_consumer.commit_and_next();

            total_prepared_count += 1;

            if total_prepared_count % 1000 == 0 {
                info!("get from queue, count: {}", total_prepared_count);
            }
        }

        if is_found_onto_changes && size_batch == 0 && Instant::now().duration_since(last_found_changes).as_secs() > 5 {
            info!("prepare changes");

            if let Ok(mut file) = File::create(ontology_file_path) {
                let res = ft_client.query(FTQuery::new_with_user("cfg:VedaSystem", &query));
                if res.result_code == 200 && res.count > 0 {
                    let mut indvs = Vec::new();

                    for el in &res.result {
                        let mut rindv: Individual = Individual::new();
                        if storage.set_binobj(&el, &mut rindv) {
                            rindv.parse_all();
                            indvs.push(rindv);
                        }
                    }

                    let mut buf = String::new();

                    buf.push('[');
                    for el in indvs.iter() {
                        if buf.len() > 1 {
                            buf.push(',');
                        }

                        buf.push_str(&mut el.obj.as_json_str());
                    }
                    buf.push(']');

                    file.write_all(buf.as_bytes())?;
                    info!("count stored {}", res.count);
                    is_found_onto_changes = false;
                } else {
                    error!("search return empty set, query: {}", &query);
                }
            } else {
                error!("fail create file {}", ontology_file_path);
            }
        }

        thread::sleep(time::Duration::from_millis(3000));
    }
}

fn is_changes(iraw: &mut Individual, onto_types: &[&str]) -> bool {
    let mut msg: Individual = Individual::new();
    if let Ok(uri) = parse_raw(iraw) {
        msg.obj.uri = uri;
        if let Ok(new_state) = msg.get_first_binobj("new_state") {
            let mut l2: Individual = Individual::new();
            l2.raw = RawObj::new(new_state);

            if let Ok(uri) = parse_raw(&mut l2) {
                l2.obj.uri = uri;
                return iraw.any_exists("rdf:type", &onto_types);
            }
        }
    }
    false
}
