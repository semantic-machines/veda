#[macro_use]
extern crate log;

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

    let mut onto = Onto::default();

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

            if let Err(e) = prepare_queue_element(&mut module, &mut onto, &mut Individual::new_raw(raw), &mut queue_out, &db_id) {
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

    fn is_exportable(
        module: &mut Module,
        onto: &mut Onto,
        queue_out: &mut Queue,
        _prev_state_indv: &mut Individual,
        new_state_indv: &mut Individual,
        db_id: &str,
    ) -> Option<String> {
        if new_state_indv.get_first_literal("sys:source").is_some() {
            return None;
        }

        if let Some(types) = new_state_indv.get_literals("rdf:type") {
            for itype in types {
                // для всех потребителей выгружаем элемент орг струкутры не имеющий поля [sys:source]
                if onto.is_some_entered(&itype, &["v-s:OrganizationUnit".to_owned()]) {
                    return Some("*".to_owned());
                }

                // выгрузка person
                if itype == "v-s:Person" {
                    return Some("*".to_owned());
                }

                // выгрузка прав если они содержат ссылку на внешнего индивида
                if itype == "v-s:PermissionStatement" {
                    if let Some(src) = module.get_literal_of_link(new_state_indv, "v-s:permissionSubject", "sys:source", &mut Individual::default()) {
                        return Some(src);
                    }
                }

                // выгрузка формы решения у которого в поле [v-wf:to] находится индивид из другой системы
                // и в поле [v-wf:onDocument] должен находится документ типа gen:InternalDocument
                if itype == "v-wf:DecisionForm" {
                    if let Some(d) = new_state_indv.get_first_literal("v-wf:onDocument") {
                        let mut doc = Individual::default();

                        if !module.storage.get_individual(&d, &mut doc) {
                            error!("fail read {} (v-wf:onDocument)", d);
                            return None;
                        }

                        if let Some(t) = doc.get_first_literal("rdf:type") {
                            if t == "gen:InternalDocument" || t == "gen:Contract" {
                                if let Some(src) = module.get_literal_of_link(new_state_indv, "v-wf:to", "sys:source", &mut Individual::default()) {
                                    for predicate in doc.get_predicates_of_type(DataType::Uri) {
                                        if predicate == "v-s:lastEditor" || predicate == "v-s:creator" || predicate == "v-s:initiator" {
                                            continue;
                                        }
                                        let mut linked_doc = Individual::default();
                                        if let Some(p) = module.get_literal_of_link(&mut doc, &predicate, "v-s:parent", &mut linked_doc) {
                                            if p == doc.get_id() {
                                                if let Err(e) = add_to_queue(queue_out, IndvOp::Put, &mut linked_doc, "?", db_id, &src, 0) {
                                                    error!("fail add to queue, err={:?}", e);
                                                    return None;
                                                }
                                            }
                                        }
                                    }

                                    if let Err(e) = add_to_queue(queue_out, IndvOp::Put, &mut doc, "?", db_id, &src, 0) {
                                        error!("fail add to queue, err={:?}", e);
                                        return None;
                                    }

                                    return Some(src);
                                }
                            }
                        }
                    }
                }

                // выгрузка принятого решения у которого в поле [v-s:lastEditor] находится индивид из другой системы
                if onto.is_some_entered(&itype, &["v-wf:Decision".to_owned()]) {
                    if let Some(src) = module.get_literal_of_link(new_state_indv, "v-s:backwardTarget", "sys:source", &mut Individual::default()) {
                        return Some(src);
                    }
                }
            }
        }

        None
    }

    fn prepare_queue_element(module: &mut Module, onto: &mut Onto, msg: &mut Individual, queue_out: &mut Queue, db_id: &str) -> Result<(), i32> {
        if parse_raw(msg).is_ok() {

            let wcmd = msg.get_first_integer("cmd");
            if wcmd.is_none() {
                return Err(-1);
            }

            let cmd = IndvOp::from_i64(wcmd.unwrap_or_default());

            let prev_state = msg.get_first_binobj("prev_state");
            let mut prev_state_indv = if prev_state.is_some() {
                let mut indv = Individual::new_raw(RawObj::new(prev_state.unwrap_or_default()));
                if parse_raw(&mut indv).is_ok() {
                }
                indv
            } else {
                Individual::default()
            };

            let new_state = msg.get_first_binobj("new_state");
            if cmd != IndvOp::Remove && new_state.is_none() {
                return Err(-1);
            }

            let date = msg.get_first_integer("date");
            if date.is_none() {
                return Err(-1);
            }

            let mut new_state_indv = Individual::new_raw(RawObj::new(new_state.unwrap_or_default()));
            if parse_raw(&mut new_state_indv).is_ok() {
                let exportable = is_exportable(module, onto, queue_out, &mut prev_state_indv, &mut new_state_indv, db_id);

                if exportable.is_none() {
                    return Ok(());
                }
                let exportable = exportable.unwrap();

                if let Err(e) = add_to_queue(queue_out, cmd, &mut new_state_indv, &msg.get_id(), db_id, &exportable, date.unwrap_or_default()) {
                    error!("fail prepare message, err={:?}", e);
                    return Err(-1);
                }

                // info! ("{:?}", raw);
            }
        }
        Ok(())
    }

    fn add_to_queue(queue_out: &mut Queue, cmd: IndvOp, new_state_indv: &mut Individual, msg_id: &str, source: &str, target: &str, date: i64) -> Result<(), i32> {
        new_state_indv.parse_all();

        let mut raw: Vec<u8> = Vec::new();
        if to_msgpack(&new_state_indv, &mut raw).is_ok() {
            let mut new_indv = Individual::default();
            new_indv.set_id(msg_id);
            new_indv.obj.add_binary("new_state", raw);
            new_indv.obj.add_integer("cmd", cmd as i64);
            new_indv.obj.add_integer("date", date);
            new_indv.obj.add_string("source_veda", source, Lang::NONE);
            new_indv.obj.add_string("target_veda", target, Lang::NONE);

            info!("add to export queue: uri={}, source={}, target={}", new_state_indv.get_id(), &source, &target);

            let mut raw1: Vec<u8> = Vec::new();
            if let Err(e) = to_msgpack(&new_indv, &mut raw1) {
                error!("fail serialize, err={:?}", e);
                return Err(-2);
            }
            if let Err(e) = queue_out.push(&raw1, MsgType::Object) {
                error!("fail push into queue, err={:?}", e);
                return Err(-1);
            }
        }
        Ok(())
    }
}
