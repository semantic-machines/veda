use chrono::{NaiveDateTime, Utc};
use futures::Future;
use std::ops::Add;
use tiberius::{BoxableIo, Error, SqlConnection, Transaction};
use time::Duration;
use tokio::runtime::current_thread;
use v_api::*;
use v_module::module::*;
use v_onto::datatype::Lang;
use v_onto::individual::*;
use voca_rs::*;

const WINPAK_TIMEZONE: i64 = 3;

const LPART_UPD_COLUMN_QUERY: &str = "\
=@P1
FROM [WIN-PAK PRO].[dbo].[CardHolder] t1
JOIN [WIN-PAK PRO].[dbo].[Card] t2 ON [t2].[CardHolderID]=[t1].[RecordId]
WHERE LTRIM([t2].[CardNumber])=@P2 and [t2].[CardHolderID]<>0 and [t1].[deleted]=0 and [t2].[deleted]=0";

const UPD_DATE_QUERY: &str = "\
UPDATE [WIN-PAK PRO].[dbo].[Card]
   SET [ActivationDate]=@P1, [ExpirationDate]=@P2
   WHERE LTRIM([CardNumber])=@P3 and [deleted]=0";

const CLEAR_ACCESS_LEVEL: &str = "\
UPDATE t1
   SET [t1].[Deleted]=1
FROM [WIN-PAK PRO].[dbo].[CardAccessLevels] t1
    JOIN [WIN-PAK PRO].[dbo].[Card] t2 ON [t2].[RecordID]=[t1].[CardID]
WHERE LTRIM([t2].[CardNumber])=@P1 and [t2].[CardHolderID]<>0 and [t1].[deleted]=0 and [t2].[deleted]=0";

const INSERT_ACCESS_LEVEL: &str = "\
INSERT INTO [WIN-PAK PRO].[dbo].[CardAccessLevels]  (AccountID,TimeStamp,UserID,NodeID,Deleted,UserPriority,CardID,AccessLevelID,SpareW1,SpareW2,SpareW3,SpareW4,SpareDW1,SpareDW2,SpareDW3,SpareDW4)
VALUES (0,@P1,0,0,0,0,
    (SELECT RecordID FROM [WIN-PAK PRO].[dbo].[Card] WHERE LTRIM([CardNumber])=@P2 and [Deleted]=0),
    @P3,0,0,0,0,0,0,0,0)";


pub fn sync_data_to_winpak<'a>(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
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

fn update_date<I: BoxableIo + 'static>(
    date_from: Result<i64, IndividualError>,
    date_to: Result<i64, IndividualError>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if date_to.is_ok() && date_from.is_ok() {
        Box::new(
            transaction
                .exec(
                    UPD_DATE_QUERY,
                    &[
                        &NaiveDateTime::from_timestamp(date_from.unwrap(), 0).add(Duration::hours(WINPAK_TIMEZONE)),
                        &NaiveDateTime::from_timestamp(date_to.unwrap(), 0).add(Duration::hours(WINPAK_TIMEZONE)),
                        &card_number.as_str(),
                    ],
                )
                .and_then(|(_result, trans)| Ok(trans)),
        )
    } else {
        Box::new(transaction.simple_exec("").and_then(|(_, trans)| Ok(trans)))
    }
}

fn clear_access_level<I: BoxableIo + 'static>(card_number: String, transaction: Transaction<I>) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    Box::new(transaction.exec(CLEAR_ACCESS_LEVEL, &[&card_number.as_str()]).and_then(|(_result, trans)| Ok(trans)))
}

fn update_access_level<I: BoxableIo + 'static>(
    idx: usize,
    levels: Vec<String>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if idx < levels.len() {
        Box::new(
            transaction
                .exec(INSERT_ACCESS_LEVEL, &[&Utc::now().naive_utc(), &card_number.as_str(), &levels.get(idx).unwrap().as_str()])
                .and_then(|(_result, trans)| Ok(trans))
                .and_then(move |trans| update_access_level(idx + 1, levels, card_number, trans)),
        )
    } else {
        Box::new(transaction.simple_exec("").and_then(|(_, trans)| Ok(trans)))
    }
}

fn update_column<I: BoxableIo + 'static>(
    idx: usize,
    names: Vec<String>,
    values: Vec<String>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if idx < values.len() && idx < names.len() {
        let column_name = names.get(idx).unwrap();
        let query = "UPDATE t1 SET [t1].[".to_string() + column_name + "]" + LPART_UPD_COLUMN_QUERY;
        let column_val = if let Some(v) = values.get(idx) {
            v.as_str()
        } else {
            ""
        };
        //info!("card [{}], update column [{}]=[{}]", card_number.to_owned(), column_name, column_val);
        //info!("query= {}", query);

        Box::new(
            transaction
                .exec(query, &[&column_val, &card_number.as_str()])
                .and_then(|(_result, trans)| Ok(trans))
                .and_then(move |trans| update_column(idx + 1, names, values, card_number, trans)),
        )
    } else {
        Box::new(transaction.simple_exec("").and_then(|(_, trans)| Ok(trans)))
    }
}

fn split_str_for_winpak_db_columns(src: &str, len: usize, res: &mut Vec<String>) {
    for el in src.split('\n') {
        let mut start = 0;
        let mut end = len;
        loop {
            if end >= el.len() {
                end = el.len();
            }

            let ss = chop::substring(el, start, end);
            if !ss.is_empty() {
                res.push(chop::substring(el, start, end));
            } else {
                break;
            }

            if end >= el.len() {
                break;
            }
            start = end;
            end += len;
        }
    }
}
