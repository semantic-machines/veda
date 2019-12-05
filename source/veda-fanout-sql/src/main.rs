#[macro_use]
extern crate log;

use v_api::ResultCode;
use v_module::info::ModuleInfo;
use v_module::module::*;
use v_module::onto::load_onto;
use v_onto::individual::*;
use v_onto::onto::Onto;
use v_queue::consumer::*;
use mysql;

pub struct Context {
    onto: Onto,
}

fn main() -> Result<(), i32> {
    init_log();

    let mut queue_consumer = Consumer::new("./data/queue", "fanout_sql", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let module_info = ModuleInfo::new("./data", "fanout_sql", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    let mut module = Module::default();

    let mut ctx = Context {
        onto: Onto::default(),
    };

    connect_to_mysql(&mut ctx, &mut module);

    info!("load onto start");
    load_onto(&mut module.fts, &mut module.storage, &mut ctx.onto);
    info!("load onto end");

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (void as fn(&mut Module, &mut Context)),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual)),
        &mut (void as fn(&mut Module, &mut Context)),
    );
    Ok(())
}

fn void(_module: &mut Module, _ctx: &mut Context) {}

fn prepare(module: &mut Module, module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return;
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let mut prev_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if let Err(e) = module_info.put_info(op_id, op_id) {
        error!("fail write module_info, op_id={}, err={:?}", op_id, e)
    }

    if let Some(types) = new_state.get_literals("rdf:type") {
        for itype in types {
            if ctx.onto.is_some_entered(&itype, &["v-s:Exportable"]) {
                export(&mut new_state, module, ctx);
                break;
            }
        }
    }
}

fn export(individual: &mut Individual, _module: &mut Module, _ctx: &mut Context) -> ResultCode {
    info!("{} exported", individual.get_id());
    ResultCode::Ok

    /*let is_deleted = individual.is_exists("v-s:deleted");

    if is_deleted {
        info!("new_indv {} is deleted, ignore it", individual.get_id());
        return ResultCode::Ok;
    }

    let actual_version = individual.get_first_literal("v-s:actual_version").unwrap_or_default();

    if !actual_version.is_empty() && actual_version != individual.get_id() {
        info!("new {}.v-s:actual_version {} != {}, ignore", individual.get_id(), &actual_version, individual.get_id());
        return ResultCode::Ok;
    }

    ResultCode::InternalServerError
    */
}

fn connect_to_mysql(_ctx: &mut Context, module: &mut Module) -> bool {
    if let Some(node) = module.get_individual("cfg:standart_node", &mut Individual::default()) {
        if let Some(v) = node.get_literals("v-s:push_individual_by_event") {
            for el in v {
                let mut connection = Individual::default();
                if module.storage.get_individual(&el, &mut connection) && !connection.is_exists_bool("v-s:deleted", true) {
                    if let Some(transport) = connection.get_first_literal("v-s:transport") {
                        if transport == "mysql" {
                            info!("found connect to mysql {}", connection.get_id());

                            let host = connection.get_first_literal("v-s:host").unwrap_or_default();
                            if host.is_empty() {
                                error!("param [host] is empty");
                                return false;
                            }
                            let port = connection.get_first_integer("v-s:port").unwrap_or(3306) as u16;
                            let login = connection.get_first_literal("v-s:login").;
                            let pass = connection.get_first_literal("v-s:password");
                            let db = connection.get_first_literal("v-s:sql_database");

                            let mut builder = mysql::OptsBuilder::new();
                            builder.ip_or_hostname(Some(host))
                                .tcp_port(port)
                                .user(Some(login))
                                .pass(Some(pass))
                                .db_name(Some(db));


                            info!("connected to mysql {} {:#?} {:#?} {:#?} {:#?}", host, port, login, pass, db);
                            return true;
                        }
                    }
                }
            }
        }
    }
    error!("not found configuration for connection to mysql server");
    false
}
