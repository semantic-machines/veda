use crate::common::*;
use crate::session_cache::*;
use rusty_v8 as v8;
use rusty_v8::scope::Entered;
use rusty_v8::{Context, HandleScope, Local, OwnedIsolate};
use std::cell::RefCell;
use std::sync::Mutex;
use v_api::*;
use v_module::module::Module;
use v_onto::parser::parse_raw;
use v_search::ft_client::*;
use v_storage::inproc_indv_r_storage::get_individual;

lazy_static! {
    static ref FT_CLIENT: Mutex<RefCell<FTClient>> = Mutex::new(RefCell::new(FTClient::new(Module::get_property("ft_query_service_url").unwrap_or_default())));
    pub(crate) static ref G_VARS: Mutex<RefCell<CallbackSharedData>> = Mutex::new(RefCell::new(CallbackSharedData::default()));
    pub(crate) static ref G_TRANSACTION: Mutex<RefCell<Transaction>> = Mutex::new(RefCell::new(Transaction::default()));
}

pub fn fn_callback_get_individual(mut scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut rv: v8::ReturnValue) {
    //let ticket = get_string_arg(&mut scope, &args, 0, "callback_get_individual: ticket not found or invalid").unwrap_or_default();
    let id = get_string_arg(&mut scope, &args, 1, "callback_get_individual: id not found or invalid").unwrap_or_default();

    if id == "undefined" {
        return;
    } else if id == "$document" {
        let mut sh_g_vars = G_VARS.lock().unwrap();
        let g_vars = sh_g_vars.get_mut();

        if let Some(indv) = &mut g_vars.g_document {
            let j_indv = individual2v8obj(scope, indv.parse_all());
            rv.set(j_indv.into());
        }
        drop(sh_g_vars);

        return;
    } else if id == "$prev_state" {
        let mut sh_g_vars = G_VARS.lock().unwrap();
        let g_vars = sh_g_vars.get_mut();

        if let Some(indv) = &mut g_vars.g_prev_state {
            let j_indv = individual2v8obj(scope, indv.parse_all());
            rv.set(j_indv.into());
        }
        drop(sh_g_vars);

        return;
    } else if id == "$execute_script" {
        return;
    } else {
        let mut sh_tnx = G_TRANSACTION.lock().unwrap();
        let tnx = sh_tnx.get_mut();

        if let Some(indv) = tnx.get_indv(&id) {
            let j_indv = individual2v8obj(scope, indv);
            rv.set(j_indv.into());
        } else {
            if let Some(mut indv) = get_individual(&id) {
                if parse_raw(&mut indv).is_ok() {
                    let j_indv = individual2v8obj(scope, &mut indv.parse_all());
                    rv.set(j_indv.into());
                } else {
                    error!("callback_get_individual: fail parse binobj, id={}", id);
                }
            }
        }

        drop(sh_tnx);
    }
}

pub fn fn_callback_get_individuals(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut rv: v8::ReturnValue) {
    let j_res = v8::Array::new(scope, 0);
    let context = scope.get_current_context().unwrap();
    let arg1 = args.get(0);
    if arg1.is_array() {
        if let Some(r) = arg1.to_object(scope) {
            if let Some(arr_keys) = r.get_property_names(scope, context) {
                let mut sh_tnx = G_TRANSACTION.lock().unwrap();
                let tnx = sh_tnx.get_mut();

                for idx in 0..arr_keys.length() {
                    let j_idx = v8::Integer::new(scope, idx as i32);
                    let j_id = r.get(scope, context, j_idx.into()).unwrap().to_object(scope).unwrap();
                    let id = j_id.to_string(scope).unwrap().to_rust_string_lossy(scope);

                    if let Some(indv) = tnx.get_indv(&id) {
                        let j_indv = individual2v8obj(scope, indv);
                        j_res.set(context, j_idx.into(), j_indv.into());
                    } else {
                        if let Some(mut indv) = get_individual(&id) {
                            if parse_raw(&mut indv).is_ok() {
                                let j_indv = individual2v8obj(scope, &mut indv.parse_all());
                                j_res.set(context, j_idx.into(), j_indv.into());
                            } else {
                                error!("callback_get_individual: fail parse binobj, id={}", id);
                            }
                        }
                    }
                }
                drop(sh_tnx);
            }
        }
    } else {
        error!("callback_get_individuals: arg is not array");
    }
    rv.set(j_res.into());
}

pub fn fn_callback_print(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut _rv: v8::ReturnValue) {
    let mut str_out = String::new();

    for idx in 0..args.length() {
        let arg = args.get(idx);
        str_out.push_str(&arg.to_string(scope).unwrap().to_rust_string_lossy(scope));
        str_out.push_str(" ");
    }
    info!("{}", str_out);
}

pub fn fn_callback_log_trace(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut _rv: v8::ReturnValue) {
    let arg1 = args.get(0);
    info!("{}", arg1.to_string(scope).unwrap().to_rust_string_lossy(scope));
}

pub fn fn_callback_get_env_str_var(mut scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut rv: v8::ReturnValue) {
    if let Some(var_name) = get_string_arg(&mut scope, &args, 0, "fn_callback_get_env_str_var: arg not found or invalid") {
        let mut sh_g_vars = G_VARS.lock().unwrap();
        let g_vars = sh_g_vars.get_mut();

        debug!("fn_callback_get_env_str_var, var_name={:?}", var_name);

        if var_name == "$parent_script_id" {
            let j_res = str_2_v8(scope, &g_vars.g_parent_script_id);
            rv.set(j_res.into());
        } else if var_name == "$parent_document_id" {
            let j_res = str_2_v8(scope, &g_vars.g_parent_document_id);
            rv.set(j_res.into());
        } else if var_name == "$user" {
            let j_res = str_2_v8(scope, &g_vars.g_user);
            rv.set(j_res.into());
        } else if var_name == "$uri" {
            let j_res = str_2_v8(scope, &g_vars.g_uri);
            rv.set(j_res.into());
        } else if var_name == "$ticket" {
            let j_res = str_2_v8(scope, &g_vars.g_ticket);
            rv.set(j_res.into());
        } else if var_name == "$super_classes" {
            let j_res = str_2_v8(scope, &g_vars.g_super_classes);
            rv.set(j_res.into());
        }

        drop(sh_g_vars);
    }
}
pub fn fn_callback_get_env_num_var(mut scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut _rv: v8::ReturnValue) {
    if let Some(var_name) = get_string_arg(&mut scope, &args, 0, "fn_callback_get_env_str_var: arg not found or invalid") {
        //let mut sh_g_vars = G_VARS.lock().unwrap();
        //let _g_vars = sh_g_vars.get_mut();

        debug!("fn_callback_get_env_num_var, var_name={:?}", var_name);

        if var_name == "$queue_elements_count" {
            return;
        } else if var_name == "$queue_elements_processed" {
            return;
        }
    }
}

pub fn fn_callback_query(mut scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut rv: v8::ReturnValue) {
    let ticket = get_string_arg(&mut scope, &args, 0, "callback_query: arg0 [ticket] not found or invalid");
    if ticket.is_none() {
        return;
    }
    let mut ticket = ticket.unwrap();

    let query = get_string_arg(&mut scope, &args, 1, "callback_query: arg1 [query] not found or invalid");
    if query.is_none() {
        return;
    }

    let sort;
    let databases;
    let top;
    let limit;
    let from;

    if ticket.is_empty() {
        let mut sh_tnx = G_TRANSACTION.lock().unwrap();
        let tnx = sh_tnx.get_mut();
        ticket = tnx.sys_ticket.to_owned();
        drop(sh_tnx);
    }

    let mut query = FTQuery::new_with_ticket(&ticket, &query.unwrap());
    if args.length() > 2 {
        sort = get_string_arg(&mut scope, &args, 2, "callback_get_individual: arg2 [sort] not found or invalid");
        query.set_sort(sort.unwrap_or_default());
        if args.length() > 3 {
            databases = get_string_arg(&mut scope, &args, 3, "callback_get_individual: arg3 [databases] not found or invalid");
            query.set_databases(databases.unwrap_or_default());
            if args.length() > 4 {
                top = get_string_i32(&mut scope, &args, 4, "callback_get_individual: arg4 [top] not found or invalid");
                query.set_top(top.unwrap_or(100000));
                if args.length() > 5 {
                    limit = get_string_i32(&mut scope, &args, 5, "callback_get_individual: arg5 [limit] not found or invalid");
                    query.set_limit(limit.unwrap_or(100000));
                    if args.length() > 6 {
                        from = get_string_i32(&mut scope, &args, 6, "callback_get_individual: arg6 [from] not found or invalid");
                        query.set_from(from.unwrap_or_default());
                    }
                }
            }
        }
    }

    let _context = scope.get_current_context().unwrap();

    let mut sh_ft_client = FT_CLIENT.lock().unwrap();
    let ft_client = sh_ft_client.get_mut();

    let res = ft_client.query(query);

    drop(sh_ft_client);

    let j_res = query_result2v8obj(scope, &res);
    rv.set(j_res.into());
}

fn fn_callback_update(opt: IndvOp, mut scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, mut rv: v8::ReturnValue) {
    let context = scope.get_current_context().unwrap();

    let wticket = get_string_arg(&mut scope, &args, 0, "fn_callback_update: arg0 [ticket] not found or invalid");
    if wticket.is_none() {
        return;
    }
    let mut ticket = wticket.unwrap_or_default();

    let obj = args.get(1);
    if obj.is_object() {
        let js_obj = obj.to_object(scope).unwrap();
        let indv = v8obj2individual(scope, &context, js_obj);
        //let indv_id = indv.get_id().to_owned();

        let mut sh_tnx = G_TRANSACTION.lock().unwrap();
        let tnx = sh_tnx.get_mut();

        if ticket.is_empty() {
            ticket = tnx.sys_ticket.to_owned();
        }

        let res = tnx.add_to_transaction(opt.clone(), indv, ticket, "".to_string());

        //info!("ADD TO TRANSACTION {:?} {}, {:?}", &opt, indv_id, res);
        rv.set(v8::Integer::new(scope, res as i32).into());
    //drop(sh_tnx);
    } else {
        error!("callback {:?}, arg is not object", opt);
    }
}

pub fn fn_callback_put_individual(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, rv: v8::ReturnValue) {
    fn_callback_update(IndvOp::Put, scope, args, rv);
}

pub fn fn_callback_remove_individual(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, rv: v8::ReturnValue) {
    fn_callback_update(IndvOp::Remove, scope, args, rv);
}
pub fn fn_callback_add_to_individual(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, rv: v8::ReturnValue) {
    fn_callback_update(IndvOp::AddIn, scope, args, rv);
}
pub fn fn_callback_set_in_individual(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, rv: v8::ReturnValue) {
    fn_callback_update(IndvOp::SetIn, scope, args, rv);
}
pub fn fn_callback_remove_from_individual(scope: v8::FunctionCallbackScope, args: v8::FunctionCallbackArguments, rv: v8::ReturnValue) {
    fn_callback_update(IndvOp::RemoveFrom, scope, args, rv);
}

pub fn init_context_with_callback<'a>(scope: &mut Entered<'a, HandleScope, OwnedIsolate>) -> Local<'a, Context> {
    let object_templ = v8::ObjectTemplate::new(scope);
    object_templ.set(str_2_v8(scope, "print").into(), v8::FunctionTemplate::new(scope, fn_callback_print).into());
    object_templ.set(str_2_v8(scope, "get_individual").into(), v8::FunctionTemplate::new(scope, fn_callback_get_individual).into());
    object_templ.set(str_2_v8(scope, "get_individuals").into(), v8::FunctionTemplate::new(scope, fn_callback_get_individuals).into());
    object_templ.set(str_2_v8(scope, "put_individual").into(), v8::FunctionTemplate::new(scope, fn_callback_put_individual).into());
    object_templ.set(str_2_v8(scope, "get_env_str_var").into(), v8::FunctionTemplate::new(scope, fn_callback_get_env_str_var).into());
    object_templ.set(str_2_v8(scope, "get_env_num_var").into(), v8::FunctionTemplate::new(scope, fn_callback_get_env_num_var).into());
    object_templ.set(str_2_v8(scope, "query").into(), v8::FunctionTemplate::new(scope, fn_callback_query).into());
    object_templ.set(str_2_v8(scope, "remove_individual").into(), v8::FunctionTemplate::new(scope, fn_callback_remove_individual).into());
    object_templ.set(str_2_v8(scope, "add_to_individual").into(), v8::FunctionTemplate::new(scope, fn_callback_add_to_individual).into());
    object_templ.set(str_2_v8(scope, "set_in_individual").into(), v8::FunctionTemplate::new(scope, fn_callback_set_in_individual).into());
    object_templ.set(str_2_v8(scope, "remove_from_individual").into(), v8::FunctionTemplate::new(scope, fn_callback_remove_from_individual).into());
    object_templ.set(str_2_v8(scope, "log_trace").into(), v8::FunctionTemplate::new(scope, fn_callback_log_trace).into());

    v8::Context::new_from_template(scope, object_templ)
}

fn get_string_arg(scope: &mut v8::FunctionCallbackScope, args: &v8::FunctionCallbackArguments, idx: i32, err_msg: &str) -> Option<String> {
    let arg = args.get(idx).to_string(*scope);
    if arg.is_none() {
        error!("{}", err_msg);
        return None;
    }
    Some(arg.unwrap().to_rust_string_lossy(*scope))
}

fn get_string_i32(scope: &mut v8::FunctionCallbackScope, args: &v8::FunctionCallbackArguments, idx: i32, err_msg: &str) -> Option<i32> {
    let arg = args.get(idx).int32_value(*scope);
    if arg.is_none() {
        error!("{}", err_msg);
        return None;
    }
    arg
}
