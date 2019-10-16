use crate::common_winpak::*;
use futures::Future;
use tiberius::SqlConnection;
use tokio::runtime::current_thread;
use v_api::*;
use v_module::module::*;
use v_onto::datatype::Lang;
use v_onto::individual::*;

pub fn update_to_winpak<'a>(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
    let (sync_res, info) = sync_data_to_winpak_wo_update(module, conn_str, indv);
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

fn sync_data_to_winpak_wo_update<'a>(module: &mut Module, conn_str: &str, indv: &mut Individual) -> (ResultCode, &'a str) {
    let module_label = indv.get_first_literal("v-s:moduleLabel");
    if module_label.is_err() || module_label.unwrap() != "winpak pe44 update" {
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

    let source_data_request_pass = indv_b.get_first_literal("mnd-s:hasSourceDataRequestForPass");
    if source_data_request_pass.is_err() {
        error!("not found [mnd-s:hasSourceDataRequestForPass] in {}", indv_b.obj.uri);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }

    let has_change_kind_for_pass = indv_b.get_literals("mnd-s:hasChangeKindForPass");
    if has_change_kind_for_pass.is_err() {
        error!("not found [mnd-s:hasChangeKindForPass] in {}", indv_b.obj.uri);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let has_change_kind_for_passes = has_change_kind_for_pass.unwrap();

    let wcard_number = indv_b.get_first_literal("mnd-s:cardNumber");
    if wcard_number.is_err() {
        error!("not found [mnd-s:cardNumber] in {}", indv_b.obj.uri);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let card_number = wcard_number.unwrap();

    let columm_names = vec![
        "Note27".to_string(),
        "Note28".to_string(),
        "Note29".to_string(),
        "Note30".to_string(),
        "Note33".to_string(),
        "Note34".to_string(),
        "Note37".to_string(),
        "Note38".to_string(),
        "Note39".to_string(),
        "Note40".to_string(),
    ];

    let mut column_values = Vec::new();
    let mut date_from = Err(IndividualError::None);
    let mut date_to = Err(IndividualError::None);
    let mut access_levels: Vec<String> = Vec::new();

    for has_change_kind_for_pass in has_change_kind_for_passes {
        if has_change_kind_for_pass == "d:lt6pdbhy2qvwquzgnp22jj2r2w" {
            if let Ok(pass_equipment) = indv_b.get_first_literal("mnd-s:passEquipment") {
                split_str_for_winpak_db_columns(&pass_equipment, 64, &mut column_values);
            }
        } else if has_change_kind_for_pass == "d:j2dohw8s79d29mxqwoeut39q92" {
            date_from = indv_b.get_first_datetime("v-s:dateFrom");
            date_to = indv_b.get_first_datetime("v-s:dateTo");
        } else if has_change_kind_for_pass == "d:a5w44zg3l6lwdje9kw09je0wzki" {
            if let Ok(access_levels_uris) = indv_b.get_literals("mnd-s:hasAccessLevel") {
                for l in access_levels_uris {
                    if let Some(nl) = l.rsplit("_").next() {
                        access_levels.push(nl.to_string());
                    }
                }
            }
        }
    }

    let future = SqlConnection::connect(conn_str)
        .and_then(|conn| conn.transaction())
        .and_then(|trans| update_column(0, columm_names, column_values, card_number.to_string(), trans))
        .and_then(|trans| update_date(date_from, date_to, card_number.to_string(), trans))
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
