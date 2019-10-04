use chrono::{NaiveDateTime, Utc};
use futures::Future;
use tiberius::SqlConnection;
use tokio::runtime::current_thread;
use v_api::*;
use v_module::module::*;
use v_onto::individual::*;
use voca_rs::*;

const COLUMNS_OF_DEVICES: &'static [&'static str] = &["Note27", "Note28", "Note29", "Note30", "Note33", "Note34", "Note37", "Note38", "Note39", "Note40"];

const UPD_COLUMN_QUERY: &str = "\
UPDATE t1
SET [t1].[@P1]=@P2
FROM [WIN-PAK PRO].[dbo].[CardHolder] t1
JOIN [WIN-PAK PRO].[dbo].[Card] t2 ON [t2].[CardHolderID]=[t1].[RecordId]
WHERE LTRIM([t2].[CardNumber])=@P3 and [t2].[CardHolderID]<>0 and [t1].[deleted]=0 and [t2].[deleted]=0";

const UPD_DATE_QUERY: &str = "\
UPDATE [WIN-PAK PRO].[dbo].[Card]
   SET [ActivationDate]=@P1, [ExpirationDate]=@P2
   WHERE LTRIM([CardNumber])=@P3 and [deleted]=0";

const CLEAR_ACCESS_LEVEL: &str = "\
UPDATE [WIN-PAK PRO].[dbo].[CardAccessLevels]
   SET [Deleted]=1
   WHERE [RecordID]=@P1 and [Deleted]=0";

const INSERT_ACCESS_LEVEL: &str = "\
INSERT INTO [WIN-PAK PRO].[dbo].[CardAccessLevels]
 (AccountID,TimeStamp,UserID,NodeID,Deleted,UserPriority,CardID,AccessLevelID,SpareW1,SpareW2,SpareW3,SpareW4,SpareDW1,SpareDW2,SpareDW3,SpareDW4)
VALUES (0,@P1,0,0,0,0,@P2,@P3,0,0,0,0,0,0,0,0)";

pub fn sync_data_to_winpak(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
    let module_label = indv.get_first_literal("v-s:moduleLabel");

    if module_label.is_err() || module_label.unwrap() != "winpak pe44 update" {
        return ResultCode::NotFound;
    }

    let backward_target = indv.get_first_literal("v-s:backwardTarget");
    if backward_target.is_err() {
        error!("not found [v-s:backwardTarget] in {}", indv.obj.uri);
        return ResultCode::NotFound;
    }
    let backward_target = backward_target.unwrap();

    let indv_b = module.get_individual(&backward_target);
    if indv_b.is_none() {
        error!("not found {}", &backward_target);
        return ResultCode::NotFound;
    }
    let mut indv_b = indv_b.unwrap();

    let has_change_kind_for_pass = indv_b.get_first_literal("mnd-s:hasChangeKindForPass");
    if has_change_kind_for_pass.is_err() {
        error!("not found [mnd-s:hasChangeKindForPass] in {}", indv_b.obj.uri);
        return ResultCode::NotFound;
    }

    let source_data_request_pass = indv_b.get_first_literal("mnd-s:hasSourceDataRequestForPass");
    if source_data_request_pass.is_err() {
        error!("not found [mnd-s:hasSourceDataRequestForPass] in {}", indv_b.obj.uri);
        return ResultCode::NotFound;
    }
    let source_data_request_pass = source_data_request_pass.unwrap();

    let indv_c = module.get_individual(&source_data_request_pass.as_str());
    if indv_c.is_none() {
        error!("not found {}", source_data_request_pass);
        return ResultCode::NotFound;
    }
    let mut indv_c = indv_c.unwrap();

    let has_change_kind_for_pass = has_change_kind_for_pass.unwrap();

    let card_number = indv_c.get_first_literal("mnd-s:cardNumber");
    if card_number.is_err() {
        error!("not found [mnd-s:cardNumber] in {}", indv_c.obj.uri);
        return ResultCode::NotFound;
    }
    let card_number = card_number.unwrap();

    if has_change_kind_for_pass == "d:lt6pdbhy2qvwquzgnp22jj2r2w" {
        if let Ok(pass_equipment) = indv_b.get_first_literal("mnd-s:passEquipment") {
            let mut idx = 0;
            for el in split_str_for_winpak_db_columns(&pass_equipment, 64) {
                let column = COLUMNS_OF_DEVICES[idx];
                idx += 1;

                let future = SqlConnection::connect(conn_str).and_then(|conn| conn.exec(UPD_COLUMN_QUERY, &[&column, &el, &card_number.as_str()]));
                if let Err(e) = current_thread::block_on_all(future) {
                    error!("fail execute sql query, err={:?}", e);
                    return ResultCode::NotFound;
                }
            }
        }
    } else if has_change_kind_for_pass == "d:j2dohw8s79d29mxqwoeut39q92" {
        let date_from = indv_b.get_first_datetime("v-s:dateFrom");
        let date_to = indv_b.get_first_datetime("v-s:dateTo");

        if date_to.is_ok() && date_from.is_ok() {
            let future = SqlConnection::connect(conn_str).and_then(|conn| {
                conn.exec(
                    UPD_DATE_QUERY,
                    &[&NaiveDateTime::from_timestamp(date_from.unwrap(), 0), &NaiveDateTime::from_timestamp(date_to.unwrap(), 0), &card_number.as_str()],
                )
            });
            if let Err(e) = current_thread::block_on_all(future) {
                error!("fail execute sql query, err={:?}", e);
                return ResultCode::DatabaseModifiedError;
            }
        }
    } else if has_change_kind_for_pass == "d:a5w44zg3l6lwdje9kw09je0wzki" {
        if let Ok(access_levels) = indv_b.get_literals("mnd-s:hasAccessLevel") {
            let winpak_card_record_id = indv_c.get_first_literal("mnd-s:winpakCardRecordId");
            if winpak_card_record_id.is_err() {
                error!("not found {}", source_data_request_pass);
                return ResultCode::NotFound;
            }
            let winpak_card_record_id = winpak_card_record_id.unwrap();

            if !access_levels.is_empty() {
                let future = SqlConnection::connect(conn_str).and_then(|conn| conn.exec(CLEAR_ACCESS_LEVEL, &[&card_number.as_str()]));
                if let Err(e) = current_thread::block_on_all(future) {
                    error!("fail execute sql query, err={:?}", e);
                    return ResultCode::DatabaseModifiedError;
                } else {
                    for alv in access_levels {
                        if let Some(alvcts) = alv.rsplit("_").next() {
                            let future = SqlConnection::connect(conn_str)
                                .and_then(|conn| conn.exec(INSERT_ACCESS_LEVEL, &[&Utc::now().naive_utc(), &winpak_card_record_id.as_str(), &alvcts]));
                            if let Err(e) = current_thread::block_on_all(future) {
                                error!("fail execute sql query, err={:?}", e);
                                return ResultCode::DatabaseModifiedError;
                            }
                        }
                    }
                }
            }
        }
    }

    ResultCode::Ok
}

fn split_str_for_winpak_db_columns(src: &str, len: usize) -> Vec<&str> {
    let res: Vec<&str> = Vec::new();
    if src.contains('\n') {
        return src.split('\n').collect();
    } else {
        let mut start = 0;
        let mut end = len;
        let mut res = Vec::new();
        loop {
            if end >= src.len() {
                break;
            }
            res.push(chop::substring(src, start, end));
            start = end;
            end += len;
        }
    }

    res
}
