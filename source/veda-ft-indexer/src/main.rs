#[macro_use]
extern crate scan_fmt;

#[macro_use]
extern crate log;

#[macro_use]
extern crate maplit;

mod error;
mod index_schema;
mod index_workplace;
mod indexer;
mod ky2slot;

use crate::error::Result;

use std::process;

use v_module::info::ModuleInfo;
use v_module::module::*;
use v_module::onto::load_onto;

use v_onto::individual::*;
use v_onto::onto::Onto;

use crate::indexer::Indexer;
use v_queue::consumer::*;
use xapian_rusty::*;

const XAPIAN_DB_TYPE: i8 = BRASS;
//const BASE_PATH: &str = "data";

fn main() -> Result<(), i32> {
    init_log();

    if get_info_of_module("input-onto").unwrap_or((0, 0)).0 == 0 {
        wait_module("fulltext_indexer", wait_load_ontology());
    }

    let mut queue_consumer = Consumer::new("./data/queue", "index_ft", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");
    let module_info = ModuleInfo::new("./data", "index_ft", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        process::exit(101);
    }
    let mut module = Module::default();
    let mut onto = Onto::default();

    load_onto(&mut module.storage, &mut onto);

    let mut ctx = Indexer {
        onto,
        index_dbs: Default::default(),
        tg: TermGenerator::new()?,
        lang: "russian".to_string(),
        key2slot: Default::default(),
        db2path: hashmap! { "base".to_owned() => "data/xapian-search-base".to_owned(), "system".to_owned()=>"data/xapian-search-system".to_owned(), "deleted".to_owned()=>"data/xapian-search-deleted".to_owned(), "az".to_owned()=>"data/xapian-search-az".to_owned() },
        idx_prop: Default::default(),
        use_db: "".to_string(),
        counter: 0,
        prev_committed_counter: 0,
    };

    info!("Rusty search-index: start listening to queue");

    ctx.init("").expect("fail init");

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before as fn(&mut Module, &mut Indexer, u32) -> Option<u32>),
        &mut (process as fn(&mut Module, &mut ModuleInfo, &mut Indexer, &mut Individual, my_consumer: &Consumer) -> Result<bool, PrepareError>),
        &mut (after as fn(&mut Module, &mut Indexer, u32) -> bool),
        &mut (heartbeat as fn(&mut Module, &mut Indexer)),
    );

    Ok(())
}

fn heartbeat(_module: &mut Module, _ctx: &mut Indexer) {}

fn before(_module: &mut Module, _ctx: &mut Indexer, _batch_size: u32) -> Option<u32> {
    None
}

fn after(_module: &mut Module, _ctx: &mut Indexer, _processed_batch_size: u32) -> bool {
    true
}

fn process(module: &mut Module, _module_info: &mut ModuleInfo, ctx: &mut Indexer, queue_element: &mut Individual, _my_consumer: &Consumer) -> Result<bool, PrepareError> {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return Ok(true);
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let mut prev_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if let Err(e) = ctx.index_msg(&mut new_state, &mut prev_state, cmd.unwrap(), op_id, module) {
        error!("fail index msg, err={:?}", e);
    }

    Ok(false)
}
