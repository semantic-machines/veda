#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use std::io::Write;
use std::{thread, time};
use v_api::IndvOp;
use v_exim::*;
use v_module::module::*;
use v_module::onto::*;
use v_onto::datatype::*;
use v_onto::individual::*;
use v_onto::individual2msgpack::*;
use v_onto::onto::*;
use v_onto::parser::*;
use v_queue::consumer::*;
use v_queue::queue::*;
use v_queue::record::*;

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

    let mut db_id = get_db_id(&mut module);

    if db_id.is_none() {
        db_id = create_db_id(&mut module);

        if db_id.is_none() {
            error!("fail create Database Identification");
            return Ok(());
        }
    }

    let mut onto = Onto::new();

    info!("load onto start");
    load_onto(&mut module.fts, &mut module.storage, &mut onto);
    info!("load onto end");

    //info!("onto: {}", onto);

    let mut queue_out = Queue::new("./data/out", "extract", Mode::ReadWrite).expect("!!!!!!!!! FAIL QUEUE");

    let mut queue_consumer = Consumer::new("./data/queue", "extract", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    let mut total_prepared_count: u64 = 0;

    let db_id = db_id.unwrap();

    loop {
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
            info!("queue: batch size={}", size_batch);
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

            if let Err(e) = prepare_queue_element(&mut onto, &mut Individual::new_raw(raw), &mut queue_out, &db_id) {
                error!("fail prepare queue element, err={}", e);
            }

            queue_consumer.commit_and_next();
            total_prepared_count += 1;

            if total_prepared_count % 1000 == 0 {
                info!("get from queue, count: {}", total_prepared_count);
            }
        }

        thread::sleep(time::Duration::from_millis(5000));
    }

    fn is_exportable(onto: &mut Onto, prev_state_indv: &mut Individual, new_state_indv: &mut Individual) -> bool {
        if let Ok(t) = new_state_indv.get_first_literal("rdf:type") {
            if onto.is_some_entered(&t, &["v-s:OrganizationUnit".to_owned()]) {
                if new_state_indv.get_first_literal("sys:source").is_err() {
                    return true;
                }
            }
            if t == "v-wf:DecisionForm" || t == "v-wf:takenDecision" {
                return true;
            }
        }
        return false;
    }

    fn prepare_queue_element(onto: &mut Onto, msg: &mut Individual, queue_out: &mut Queue, db_id: &str) -> Result<(), i32> {
        if let Ok(uri) = parse_raw(msg) {
            msg.obj.uri = uri;

            let wcmd = msg.get_first_integer("cmd");
            if wcmd.is_err() {
                return Err(-1);
            }

            let cmd = IndvOp::from_i64(wcmd.unwrap_or_default());

            let prev_state = msg.get_first_binobj("prev_state");
            let mut prev_state_indv = if !prev_state.is_err() {
                let mut indv = Individual::new_raw(RawObj::new(prev_state.unwrap_or_default()));
                if let Ok(uri) = parse_raw(&mut indv) {
                    indv.obj.uri = uri.clone();
                }
                indv
            } else {
                Individual::default()
            };

            let new_state = msg.get_first_binobj("new_state");
            if cmd != IndvOp::Remove && new_state.is_err() {
                return Err(-1);
            }

            let date = msg.get_first_integer("date");
            if date.is_err() {
                return Err(-1);
            }

            let mut new_state_indv = Individual::new_raw(RawObj::new(new_state.unwrap_or_default()));
            if let Ok(uri) = parse_raw(&mut new_state_indv) {
                if !is_exportable(onto, &mut prev_state_indv, &mut new_state_indv) {
                    return Ok(());
                }
                new_state_indv.parse_all();
                new_state_indv.obj.uri = uri.clone();

                let mut raw: Vec<u8> = Vec::new();
                if to_msgpack(&new_state_indv, &mut raw).is_ok() {
                    let mut new_indv = Individual::default();
                    new_indv.obj.uri = msg.obj.uri.clone();
                    new_indv.obj.add_binary("new_state", raw, 0);
                    new_indv.obj.add_integer("cmd", cmd as i64, 0);
                    new_indv.obj.add_integer("date", date.unwrap_or_default(), 0);
                    new_indv.obj.add_string("source_veda", db_id, Lang::NONE, 0);
                    new_indv.obj.add_string("target_veda", "*", Lang::NONE, 0);

                    let mut raw1: Vec<u8> = Vec::new();
                    if to_msgpack(&new_indv, &mut raw1).is_ok() {

                        /*
                        // test
                        let mut msg = Individual::new_raw(RawObj::new(raw1));
                        if let Ok(uri) = parse_raw(&mut msg) {
                            msg.obj.uri = uri;

                            msg.parse_all();

                            info!("! {}", msg);
                        }
                        */
                    }
                    if let Err(e) = queue_out.push(&raw1, MsgType::Object) {
                        error!("fail push into queue, err={:?}", e);
                        return Err(-1);
                    }
                }
                // info! ("{:?}", raw);
            }
        }
        Ok(())
    }
}