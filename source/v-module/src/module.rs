use crate::info::ModuleInfo;
use crate::ticket::Ticket;
use chrono::Local;
use env_logger::Builder;
use ini::Ini;
use log::LevelFilter;
use nng::options::protocol::pubsub::Subscribe;
use nng::options::Options;
use nng::{Protocol, Socket};
use std::io::Write;
use v_api::*;
use v_onto::individual::*;
use v_onto::parser::*;
use v_queue::{consumer::*, record::*};
use v_search::*;
use v_storage::storage::*;

pub struct Module {
    pub storage: VStorage,
    pub fts: FTClient,
    pub api: APIClient,
    queue_prepared_count: i64,
    notify_channel_url: String,
}

impl Default for Module {
    fn default() -> Self {
        Module::new(StorageMode::ReadOnly)
    }
}

impl Module {
    pub fn new(storage_mode: StorageMode) -> Self {
        let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");

        let section = conf.section(None::<String>).expect("fail parse veda.properties");
        let ft_query_service_url = section.get("ft_query_service_url").expect("param [ft_query_service_url] not found in veda.properties").clone();

        let tarantool_addr = if let Some(p) = section.get("tarantool_url") {
            p.to_owned()
        } else {
            warn!("param [tarantool_url] not found in veda.properties");
            "".to_owned()
        };

        if !tarantool_addr.is_empty() {
            info!("tarantool addr={}", &tarantool_addr);
        }

        let notify_channel_url = if let Some(s) = section.get("notify_channel_url") {
            s.to_owned()
        } else {
            String::default()
        };

        let storage: VStorage;
        if !tarantool_addr.is_empty() {
            storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
        } else {
            storage = VStorage::new_lmdb("./data", storage_mode);
        }

        let ft_client = FTClient::new(ft_query_service_url);

        let param_name = "main_module_url";
        let api = if let Some(url) = Module::get_property(param_name) {
            APIClient::new(url)
        } else {
            error!("not found param {} in properties file", param_name);
            APIClient::new("".to_owned())
        };

        Module {
            storage,
            fts: ft_client,
            api,
            queue_prepared_count: 0,
            notify_channel_url,
        }
    }
}

impl Module {
    pub fn get_property(param: &str) -> Option<String> {
        let conf = Ini::load_from_file("veda.properties").expect("fail load veda.properties file");

        let section = conf.section(None::<String>).expect("fail parse veda.properties");
        if let Some(v) = section.get(param) {
            Some(v.to_string())
        } else {
            None
        }
    }

    pub fn get_sys_ticket_id_from_db(storage: &mut VStorage) -> Result<String, i32> {
        let mut indv = Individual::default();
        if storage.get_individual_from_db(StorageId::Tickets, "systicket", &mut indv) {
            if let Some(c) = indv.get_first_literal("v-s:resource") {
                return Ok(c);
            }
        }
        Err(-1)
    }

    pub fn get_sys_ticket_id(&mut self) -> Result<String, i32> {
        Module::get_sys_ticket_id_from_db(&mut self.storage)
    }

    pub fn get_literal_of_link(&mut self, indv: &mut Individual, link: &str, field: &str, to: &mut Individual) -> Option<String> {
        if let Some(v) = indv.get_literals(link) {
            for el in v {
                if self.storage.get_individual(&el, to) {
                    return to.get_first_literal(field);
                }
            }
        }
        None
    }

    pub fn get_literals_of_link(&mut self, indv: &mut Individual, link: &str, field: &str) -> Vec<String> {
        let mut res = Vec::new();
        if let Some(v) = indv.get_literals(link) {
            for el in v {
                let to = &mut Individual::default();
                if self.storage.get_individual(&el, to) {
                    if let Some(s) = to.get_first_literal(field) {
                        res.push(s);
                    }
                }
            }
        }
        res
    }

    pub fn get_datetime_of_link(&mut self, indv: &mut Individual, link: &str, field: &str, to: &mut Individual) -> Option<i64> {
        if let Some(v) = indv.get_literals(link) {
            for el in v {
                if self.storage.get_individual(&el, to) {
                    return to.get_first_datetime(field);
                }
            }
        }
        None
    }

    pub fn get_individual_h(&mut self, uri: &str) -> Option<Box<Individual>> {
        let mut iraw = Box::new(Individual::default());
        if !self.storage.get_individual(uri, &mut iraw) {
            return None;
        }
        Some(iraw)
    }

    pub fn get_individual<'a>(&mut self, uri: &str, iraw: &'a mut Individual) -> Option<&'a mut Individual> {
        if uri.is_empty() || !self.storage.get_individual(uri, iraw) {
            return None;
        }
        Some(iraw)
    }

    pub fn listen_queue<T>(
        &mut self,
        queue_consumer: &mut Consumer,
        module_info: &mut ModuleInfo,
        module_context: &mut T,
        before_bath: &mut fn(&mut Module, &mut T),
        prepare: &mut fn(&mut Module, &mut ModuleInfo, &mut T, &mut Individual),
        after_bath: &mut fn(&mut Module, &mut T),
    ) {
        let soc = Socket::new(Protocol::Sub0).unwrap();

        if !self.notify_channel_url.is_empty() {
            if let Err(e) = soc.dial(&self.notify_channel_url) {
                error!("fail connect to, {} err={}", self.notify_channel_url, e);
            }

            let all_topics = vec![];
            if let Err(e) = soc.set_opt::<Subscribe>(all_topics) {
                error!("fail subscribe, {} err={}", self.notify_channel_url, e);
            }
        }

        loop {
            let mut size_batch = 0;

            before_bath(self, module_context);

            // read queue current part info
            if let Err(e) = queue_consumer.queue.get_info_of_part(queue_consumer.id, true) {
                error!("{} get_info_of_part {}: {}", self.queue_prepared_count, queue_consumer.id, e.as_str());
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
                        error!("{} get msg from queue: {}", self.queue_prepared_count, e.as_str());
                        break;
                    }
                }

                let mut queue_element = Individual::new_raw(raw);
                if parse_raw(&mut queue_element).is_ok() {
                    prepare(self, module_info, module_context, &mut queue_element);
                }

                queue_consumer.commit_and_next();

                self.queue_prepared_count += 1;

                if self.queue_prepared_count % 1000 == 0 {
                    info!("get from queue, count: {}", self.queue_prepared_count);
                }
            }
            after_bath(self, module_context);

            let wmsg = soc.recv();
            if let Err(e) = wmsg {
                error!("fail recv from slave node, err={:?}", e);
            }
        }
    }
}

pub fn get_inner_binobj_as_individual<'a>(queue_element: &'a mut Individual, field_name: &str, new_indv: &'a mut Individual) -> bool {
    let binobj = queue_element.get_first_binobj(field_name);
    if binobj.is_some() {
        new_indv.set_raw(&binobj.unwrap_or_default());
        if parse_raw(new_indv).is_ok() {
            return true;
        }
    }
    false
}

pub fn get_cmd(queue_element: &mut Individual) -> Option<IndvOp> {
    let wcmd = queue_element.get_first_integer("cmd");
    wcmd?;

    Some(IndvOp::from_i64(wcmd.unwrap_or_default()))
}

pub fn init_log() {
    let env_var = "RUST_LOG";
    match std::env::var_os(env_var) {
        Some(val) => println!("use env var: {}: {:?}", env_var, val.to_str()),
        None => std::env::set_var(env_var, "info"),
    }

    Builder::new()
        .format(|buf, record| writeln!(buf, "{} [{}] - {}", Local::now().format("%Y-%m-%dT%H:%M:%S%.3f"), record.level(), record.args()))
        .filter(None, LevelFilter::Info)
        .init();
}

pub fn get_ticket_from_db(id: &str, dest: &mut Ticket, module: &mut Module) {
    let mut indv = Individual::default();
    if module.storage.get_individual_from_db(StorageId::Tickets, id, &mut indv) {
        dest.update_from_individual(&mut indv);
        dest.result = ResultCode::Ok;
    }
}
