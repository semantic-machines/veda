use crate::common_winpak::*;
use chrono::Utc;
use futures::Future;
use tiberius::SqlConnection;
use tokio::runtime::current_thread;
use v_api::app::ResultCode;
use v_api::*;
use v_module::module::*;
use v_onto::datatype::Lang;
use v_onto::individual::*;

pub fn update_to_winpak<'a>(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
    let (sync_res, info) = sync_data_to_winpak(module, conn_str, indv);
    if sync_res == ResultCode::ConnectError {
        return sync_res;
    }

    indv.set_uri("v-s:lastEditor", "cfg:VedaSystemAppointment");

    if sync_res == ResultCode::Ok {
        indv.set_uri("v-s:hasStatus", "v-s:StatusAccepted");
    } else {
        indv.set_uri("v-s:hasStatus", "v-s:StatusRejected");
        indv.add_string("v-s:errorMessage", info, Lang::RU);
    }
    indv.clear("v-s:errorMessage");

    info!("update from {}, status={:?}, info={}", indv.get_id(), sync_res, info);
    let res = module.api.update(systicket, IndvOp::Put, indv);
    if res.result != ResultCode::Ok {
        error!("fail update, uri={}, result_code={:?}", indv.get_id(), res.result);
    } else {
        info!("success update, uri={}", indv.get_id());
    }
    sync_res
}

fn sync_data_to_winpak<'a>(module: &mut Module, conn_str: &str, indv: &mut Individual) -> (ResultCode, &'a str) {
    let backward_target = indv.get_first_literal("v-s:backwardTarget");
    if backward_target.is_none() {
        error!("not found [v-s:backwardTarget] in {}", indv.get_id());
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let backward_target = backward_target.unwrap();

    let indv_b = module.get_individual_h(&backward_target);
    if indv_b.is_none() {
        error!("not found {}", &backward_target);
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let mut indv_b = indv_b.unwrap();

    let btype = indv_b.get_first_literal("rdf:type").unwrap_or_default();

    let has_change_kind_for_pass = indv_b.get_literals("mnd-s:hasChangeKindForPass");
    if btype != "mnd-s:Pass" && has_change_kind_for_pass.is_none() {
        error!("not found [mnd-s:hasChangeKindForPass] in {}", indv_b.get_id());
        return (ResultCode::NotFound, "исходные данные некорректны");
    }

    let has_change_kind_for_passes = has_change_kind_for_pass.unwrap_or_default();

    let wcard_number = indv_b.get_first_literal("mnd-s:cardNumber");
    if wcard_number.is_none() {
        error!("not found [mnd-s:cardNumber] in {}", indv_b.get_id());
        return (ResultCode::NotFound, "исходные данные некорректны");
    }
    let card_number = wcard_number.unwrap();

    let mut equipment_list = Vec::new();
    let mut date_from = None;
    let mut date_to = None;
    let mut access_levels: Vec<i64> = Vec::new();
    let mut is_update_access_levels = false;
    let mut is_update_access_levels_without_clean = false;
    let mut is_update_equipment = false;
    let mut is_need_block_card = false;
    let mut cardholder_family: Option<String> = None;
    let mut ts_number: Option<String> = None;

    if btype == "mnd-s:Pass" {
        is_update_equipment = true;
        get_equipment_list(&mut indv_b, &mut equipment_list);
        date_from = indv_b.get_first_datetime("v-s:dateFromFact");
        date_to = indv_b.get_first_datetime("v-s:dateToFact");

        is_update_access_levels = true;
    //get_access_level(&mut indv_b, &mut access_levels);
    } else {
        if has_change_kind_for_passes.is_empty() {
            is_update_access_levels = true;
        }

        for has_change_kind_for_pass in has_change_kind_for_passes {
            if has_change_kind_for_pass == "d:lt6pdbhy2qvwquzgnp22jj2r2w" {
                is_update_equipment = true;
                get_equipment_list(&mut indv_b, &mut equipment_list);
            } else if has_change_kind_for_pass == "d:j2dohw8s79d29mxqwoeut39q92" {
                date_from = indv_b.get_first_datetime("v-s:dateFromFact");
                date_to = indv_b.get_first_datetime("v-s:dateToFact");
            } else if has_change_kind_for_pass == "d:a5w44zg3l6lwdje9kw09je0wzki" {
                is_update_access_levels = true;
            } else if has_change_kind_for_pass == "d:e8j2tpz9r613hxq4g4rbbxtfqe" {
                date_from = indv_b.get_first_datetime("v-s:dateFromFact");
                is_need_block_card = true;
            } else if has_change_kind_for_pass == "d:a8kf3r1ryfotqg695yckpm2isih" {
                cardholder_family = indv_b.get_first_literal_with_lang("mnd-s:passLastName", &[Lang::RU, Lang::NONE]);
            } else if has_change_kind_for_pass == "d:a5y91zferr8t41abib4ecdlggn0" {
                ts_number = indv_b.get_first_literal("mnd-s:passVehicleRegistrationNumber");
            } else if has_change_kind_for_pass == "d:efbibmgvxpr46t1qksmtkkautw" {
                is_update_access_levels = false;
                is_update_access_levels_without_clean = true;
                get_access_level(&mut indv_b, "mnd-s:hasTemporaryAccessLevel", &mut access_levels);
            }
        }
    }

    if is_update_access_levels {
        get_access_level(&mut indv_b, "mnd-s:hasAccessLevel", &mut access_levels);
    }

    let now = Utc::now().naive_utc();

    let future = SqlConnection::connect(conn_str)
        .and_then(|conn| conn.transaction())
        .and_then(|trans| update_equipment(is_update_equipment, equipment_list, card_number.to_string(), trans))
        .and_then(|trans| update_card_date(Some(get_now_00_00_00().timestamp()), date_to, card_number.to_string(), trans))
        .and_then(|trans| block_card(is_need_block_card, date_from, now, card_number.to_string(), trans))
        .and_then(|trans| clear_access_level(is_update_access_levels, card_number.to_string(), trans))
        .and_then(|trans| insert_access_levels(is_update_access_levels | is_update_access_levels_without_clean, now, 0, access_levels, card_number.to_string(), trans))
        .and_then(|trans| create_winpak_change_card_event(is_update_access_levels, card_number.to_string(), trans))
        .and_then(|trans| update_cardholder_family(cardholder_family, card_number.to_string(), trans))
        .and_then(|trans| update_ts_number(ts_number, card_number.to_string(), trans))
        .and_then(|trans| deactivate_card(is_update_access_levels_without_clean, Some(get_now_00_00_00().timestamp()), card_number.to_string(), trans))
        .and_then(|trans| trans.commit());
    return match current_thread::block_on_all(future) {
        Ok(_) => (ResultCode::Ok, "данные обновлены"),
        Err(e) => {
            error!("fail execute query, err={:?}", e);
            (ResultCode::DatabaseModifiedError, "ошибка обновления")
        }
    };
}
