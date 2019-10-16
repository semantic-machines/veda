use crate::common_winpak::*;
use futures::Future;
use tiberius::SqlConnection;
use tokio::runtime::current_thread;
use v_api::*;
use v_module::module::*;
use v_onto::datatype::Lang;
use v_onto::individual::*;

pub fn insert_to_winpak<'a>(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
    let (sync_res, info) = sync_data_to_winpak(module, conn_str, indv);
    if sync_res == ResultCode::ConnectError {
        return sync_res;
    }

    indv.obj.set_uri("v-s:lastEditor", "cfg:VedaSystem");

    if sync_res == ResultCode::Ok {
        indv.obj.set_uri("v-s:hasStatus", "v-s:StatusAccepted");
    } else {
        indv.obj.set_uri("v-s:hasStatus", "v-s:StatusRejected");
        indv.obj.add_string("v-s:errorMessage", info, Lang::RU);
    }
    indv.obj.clear("v-s:errorMessage");

    info!("update from {}, status={:?}, info={}", indv.obj.uri, sync_res, info);
    let res = module.api.update(systicket, IndvOp::Put, indv);
    if res.result != ResultCode::Ok {
        error!("fail update, uri={}, result_code={:?}", indv.obj.uri, res.result);
    } else {
        info!("success update, uri={}", indv.obj.uri);
    }
    sync_res
}

fn sync_data_to_winpak<'a>(module: &mut Module, conn_str: &str, indv: &mut Individual) -> (ResultCode, &'a str) {
    let module_label = indv.get_first_literal("v-s:moduleLabel");
    if module_label.is_err() || module_label.unwrap() != "winpak pe44 create" {
        return (ResultCode::NotFound, "исходные данные некорректны");
    }

    let backward_target = indv.get_first_literal("v-s:backwardTarget");
    if backward_target.is_err() {
        error!("not found [v-s:backwardTarget] in {}", indv.obj.uri);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let backward_target = backward_target.unwrap();

    let indv_b = module.get_individual_h(&backward_target);
    if indv_b.is_none() {
        error!("not found {}", &backward_target);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let mut indv_b = indv_b.unwrap();

    let has_kind_for_pass = indv_b.get_literals("mnd-s:hasPassKind");
    if has_kind_for_pass.is_err() {
        error!("not found [mnd-s:hasKindForPass] in {}", indv_b.obj.uri);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let has_kind_for_passes = has_kind_for_pass.unwrap();

    let wcard_number = indv_b.get_first_literal("mnd-s:cardNumber");
    if wcard_number.is_err() {
        error!("not found [mnd-s:cardNumber] in {}", indv_b.obj.uri);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let card_number = wcard_number.unwrap();

    for has_kind_for_pass in has_kind_for_passes {
        if has_kind_for_pass == "d:c94b6f98986d493cae4a3a37249101dc"
            || has_kind_for_pass == "d:5f5be080f1004af69742bc574c030609"
            || has_kind_for_pass == "d:1799f1e110054b5a9ef819754b0932ce"
        {}
        if has_kind_for_pass == "d:ece7e741557e406bb996809163810c6e"
            || has_kind_for_pass == "d:a149d268628b46ae8d40c6ea0ac7f3dd"
            || has_kind_for_pass == "d:228e15d5afe544c099c337ceafa47ea6"
        {}
    }

    let mut access_levels: Vec<String> = Vec::new();
    get_access_level(&mut indv_b, &mut access_levels);

    let future = SqlConnection::connect(conn_str)
        .and_then(|conn| conn.transaction())
        .and_then(|trans| clear_access_level(card_number.to_string(), trans))
        .and_then(|trans| update_access_level(0, access_levels, card_number.to_string(), trans))
        .and_then(|trans| trans.commit());
    match current_thread::block_on_all(future) {
        Ok(_) => {
            return (ResultCode::Ok, "данные обновлены");
        }
        Err(e) => {
            error!("fail execute query, err={:?}", e);
            return (ResultCode::DatabaseModifiedError, "ошибка обновления");
        }
    }
}
