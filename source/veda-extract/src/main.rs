#[macro_use]
extern crate log;
extern crate env_logger;

use chrono::Local;
use env_logger::Builder;
//use ini::ini::Error;
use ini::Ini;
use log::LevelFilter;
use std::io::Write;
use std::{thread, time};
use v_module::onto::*;
use v_onto::datatype::*;
use v_onto::individual::*;
use v_onto::individual2msgpack::*;
use v_onto::onto::*;
use v_onto::parser::*;
use v_queue::consumer::*;
use v_queue::queue::*;
use v_queue::record::*;
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
    let ft_query_service_url = section.get("ft_query_service_url").expect("param [ft_query_service_url] not found in veda.properties").clone();

    let tarantool_addr = if let Some(p) = section.get("tarantool_url") {
        p.to_owned()
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    info!("tarantool addr={:?}", &tarantool_addr);

    let mut storage: VStorage;
    if tarantool_addr.len() > 0 {
        storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
    } else {
        storage = VStorage::new_lmdb("./data/lmdb-individuals/");
    }

    let mut ft_client = FTClient::new(ft_query_service_url);

    while ft_client.connect() != true {
        thread::sleep(time::Duration::from_millis(3000));
    }

    let mut onto = Onto::new();

    info!("load onto start");
    load_onto(&mut ft_client, &mut storage, &mut onto);
    info!("load onto stop");

    //info!("onto: {}", onto);

    let mut queue_out = Queue::new("./data/out", "extract", Mode::ReadWrite).expect("!!!!!!!!! FAIL QUEUE");

    let mut queue_consumer = Consumer::new("./data/queue", "extract", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    let mut total_prepared_count: u64 = 0;

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
            if queue_consumer.pop_header() == false {
                break;
            }

            let mut raw = RawObj::new(vec![0; (queue_consumer.header.msg_length) as usize]);
            //let mut msg = Individual::new(vec![0; (queue_consumer.header.msg_length) as usize]);

            // заголовок взят успешно, занесем содержимое сообщения в структуру Individual
            if let Err(e) = queue_consumer.pop_body(&mut raw.data) {
                if e == ErrorQueue::FailReadTailMessage {
                    break;
                } else {
                    error!("{} get msg from queue: {}", total_prepared_count, e.as_str());
                    break;
                }
            }

            prepare_queue_element(&mut Individual::new_raw(raw), &mut queue_out);

            queue_consumer.commit_and_next();
            total_prepared_count += 1;

            if total_prepared_count % 1000 == 0 {
                info!("get from queue, count: {}", total_prepared_count);
            }
        }

        thread::sleep(time::Duration::from_millis(5000));
    }

    fn prepare_queue_element(msg: &mut Individual, queue_out: &mut Queue) -> Result<(), i32> {
        if let Ok(uri) = parse_raw(msg) {
            msg.obj.uri = uri;

            let new_state = msg.get_first_binobj("new_state");
            if let Err(e) = new_state {
                return Err(-1);
            }

            let cmd = msg.get_first_integer("cmd");
            if let Err(e) = cmd {
                return Err(-1);
            }

            let date = msg.get_first_integer("date");
            if let Err(e) = date {
                return Err(-1);
            }

            let mut indv = Individual::new_raw(RawObj::new(new_state.unwrap_or_default()));
            if let Ok(uri) = parse_raw(&mut indv) {
                indv.parse_all();
                indv.obj.uri = uri.clone();

                let mut raw: Vec<u8> = Vec::new();
                to_msgpack(&indv, &mut raw);

                let mut new_indv = Individual::new();
                new_indv.obj.uri = uri.clone();
                new_indv.obj.add_uri("uri", &uri, 0);
                new_indv.obj.add_binary("new_state", raw, 0);
                new_indv.obj.add_integer("cmd", cmd.unwrap_or_default(), 0);
                new_indv.obj.add_integer("date", date.unwrap_or_default(), 0);
                new_indv.obj.add_string("source_veda", "*", Lang::NONE, 0);
                new_indv.obj.add_string("target_veda", "*", Lang::NONE, 0);

                let mut raw1: Vec<u8> = Vec::new();
                to_msgpack(&new_indv, &mut raw1);

                // test
                let mut msg = Individual::new_raw(RawObj::new(raw1));
                if let Ok(uri) = parse_raw(&mut msg) {
                    msg.obj.uri = uri;

                    msg.parse_all();

                    info!("! {}", msg);
                }

                // info! ("{:?}", raw);
                // queue_out.push(&raw1, MsgType::Object);
            }
        }
        Ok(())
    }
}
