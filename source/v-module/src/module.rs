use v_storage::storage::VStorage;
use v_search::*;
use std::{thread, time};
use ini::Ini;

pub struct Module {
    pub storage : VStorage,
    pub fts: FTClient
}

impl Module {
    pub fn new () -> Self {

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

        Module {
            storage: storage,
            fts: ft_client
        }
    }
}