use chrono::{NaiveDateTime, Utc};
use futures::Future;
use tiberius::{BoxableIo, Error, SqlConnection, Transaction};
use tokio::runtime::current_thread;
use v_api::*;
use v_module::module::*;
use v_onto::individual::*;
use voca_rs::*;

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

    let indv_b = module.get_individual_h(&backward_target);
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

    let indv_c = module.get_individual_h(&source_data_request_pass.as_str());
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
    if has_change_kind_for_pass == "d:lt6pdbhy2qvwquzgnp22jj2r2w" {
        if let Ok(pass_equipment) = indv_b.get_first_literal("mnd-s:passEquipment") {
            let column_values = split_str_for_winpak_db_columns(&pass_equipment, 64);

            let future = SqlConnection::connect(conn_str)
                .and_then(|conn| conn.transaction())
                .and_then(|trans| update_column(0, columm_names, column_values, card_number, trans))
                .and_then(|trans| trans.commit());
            current_thread::block_on_all(future).unwrap();
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
                let future = SqlConnection::connect(conn_str)
                    .and_then(|conn| conn.transaction())
                    .and_then(|trans| clear_access_level(card_number, trans))
                    .and_then(|trans| update_access_level(0, access_levels, winpak_card_record_id, trans))
                    .and_then(|trans| trans.commit());
                current_thread::block_on_all(future).unwrap();
            }
        }
    }

    ResultCode::NotFound
}

fn clear_access_level<I: BoxableIo + 'static>(card_number: String, transaction: Transaction<I>) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    Box::new(transaction.exec(CLEAR_ACCESS_LEVEL, &[&card_number.as_str()]).and_then(|(result, trans)| {
        assert_eq!(result, 1);
        Ok(trans)
    }))
}

fn update_access_level<I: BoxableIo + 'static>(
    idx: usize,
    levels: Vec<String>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    Box::new(
        transaction
            .exec(INSERT_ACCESS_LEVEL, &[&Utc::now().naive_utc(), &card_number.as_str(), &levels.get(idx).unwrap().as_str()])
            .and_then(|(result, trans)| {
                assert_eq!(result, 1);
                Ok(trans)
            })
            .and_then(move |trans| update_access_level(idx + 1, levels, card_number, trans)),
    )
}

fn update_column<I: BoxableIo + 'static>(
    idx: usize,
    names: Vec<String>,
    values: Vec<String>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if idx < names.len() {
        let query = "UPDATE t1 SET [t1].".to_string() + names.get(idx).unwrap() + LPART_UPD_COLUMN_QUERY;
        Box::new(
            transaction
                .exec(query, &[&values.get(idx).unwrap().as_str(), &card_number.as_str()])
                .and_then(|(result, trans)| {
                    assert_eq!(result, 1);
                    Ok(trans)
                })
                .and_then(move |trans| update_column(idx + 1, names, values, card_number, trans)),
        )
    } else {
        Box::new(transaction.simple_exec("").and_then(|(_, trans)| Ok(trans)))
    }
}

fn split_str_for_winpak_db_columns(src: &str, len: usize) -> Vec<String> {
    let mut res: Vec<String> = Vec::new();
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
    res
}
