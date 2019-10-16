use ini::Ini;
use std::{thread, time};
use v_api::APIClient;
use v_onto::individual::Individual;
use v_onto::individual::*;
use v_queue::{consumer::*, record::*};
use v_search::*;
use v_storage::storage::*;

pub struct Module {
    pub storage: VStorage,
    pub fts: FTClient,
    pub api: APIClient,
    queue_prepared_count: i64,
}

impl Default for Module {
    fn default() -> Self {
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

        let storage: VStorage;
        if !tarantool_addr.is_empty() {
            storage = VStorage::new_tt(tarantool_addr, "veda6", "123456");
        } else {
            storage = VStorage::new_lmdb("./data");
        }

        let mut ft_client = FTClient::new(ft_query_service_url);

        while !ft_client.connect() {
            thread::sleep(time::Duration::from_millis(3000));
        }

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

    pub fn get_sys_ticket_id(&mut self) -> Result<String, i32> {
        let mut indv = Individual::default();
        if self.storage.get_individual_from_db(StorageId::Tickets, "systicket", &mut indv) {
            if let Ok(c) = indv.get_first_literal("v-s:resource") {
                return Ok(c);
            }
        }
        Err(-1)
    }

    pub fn get_literal_of_link(&mut self, indv: &mut Individual, link: &str, field: &str, to: &mut Individual) -> Option<String> {
        if let Ok(v) = indv.get_literals(link) {
            for el in v {
                if self.storage.get_individual(&el, to) {
                    if let Ok(src) = to.get_first_literal(field) {
                        return Some(src);
                    }
                }
            }
        }
        None
    }

    pub fn get_datetime_of_link(&mut self, indv: &mut Individual, link: &str, field: &str, to: &mut Individual) -> Option<i64> {
        if let Ok(v) = indv.get_literals(link) {
            for el in v {
                if self.storage.get_individual(&el, to) {
                    if let Ok(src) = to.get_first_datetime(field) {
                        return Some(src);
                    }
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
        if !self.storage.get_individual(uri, iraw) {
            return None;
        }
        Some(iraw)
    }

    pub fn listen_queue<T>(
        &mut self,
        queue_consumer: &mut Consumer,
        module_context: &mut T,
        before_bath: &mut fn(&mut T),
        prepare: &mut fn(&mut T, &mut Individual),
        after_bath: &mut fn(&mut T),
    ) {
        loop {
            let mut size_batch = 0;

            before_bath(module_context);

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

                prepare(module_context, &mut Individual::new_raw(raw));

                queue_consumer.commit_and_next();

                self.queue_prepared_count += 1;

                if self.queue_prepared_count % 1000 == 0 {
                    info!("get from queue, count: {}", self.queue_prepared_count);
                }
            }
            after_bath(module_context);
        }
    }
}
