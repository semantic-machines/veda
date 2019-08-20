use ini::Ini;
use std::{thread, time};
use v_api::APIClient;
use v_onto::individual::Individual;
use v_search::*;
use v_storage::storage::*;

pub struct Module {
    pub storage: VStorage,
    pub fts: FTClient,
    pub api: APIClient,
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
        return Err(-1);
    }

    pub fn get_literal_of_link(&mut self, indv: &mut Individual, link: &str, field: &str) -> Option<String> {
        if let Ok(v) = indv.get_literals(link) {
            for el in v {
                let mut to = Individual::default();

                if self.storage.get_individual(&el, &mut to) {
                    if let Ok(src) = to.get_first_literal(field) {
                        return Some(src);
                    }
                }
            }
        }
        None
    }
}
