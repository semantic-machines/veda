use futures::Future;
use tiberius::SqlConnection;
use tokio::runtime::current_thread;
use v_api::*;
use v_module::module::*;
use v_onto::individual::*;
use voca_rs::*;

const COLUMNS_OF_DEVICES: &'static [&'static str] = &["Note27", "Note28", "Note29", "Note30", "Note33", "Note34", "Note37", "Note38", "Note39", "Note40"];

const UPD_QUERY: &str = "\
UPDATE t1
SET [t1].[@P1]=@P2
FROM [WIN-PAK PRO].[dbo].[CardHolder] t1
JOIN [WIN-PAK PRO].[dbo].[Card] t2 ON [t2].[CardHolderID]=[t1].[RecordId]
WHERE LTRIM([t2].[CardNumber])=@P3 and [t2].[CardHolderID]<>0 and [t1].[deleted]=0 and [t2].[deleted]=0
";

pub fn sync_data_to_winpak(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
    let module_label = indv.get_first_literal("v-s:moduleLabel");

    if module_label.is_err() || module_label.unwrap() != "winpak pe44 update" {
        return ResultCode::Ok;
    }

    let bt = indv.get_first_literal("v-s:backwardTarget");
    if bt.is_err() {
        error!("not found [v-s:backwardTarget] in {}", indv.obj.uri);
        return ResultCode::Ok;
    }

    if let Some(mut indv1) = module.get_individual(&bt.unwrap()) {
        if let Ok(s) = indv1.get_first_literal("mnd-s:hasChangeKindForPass") {
            if s == "d:lt6pdbhy2qvwquzgnp22jj2r2w" {
                let card_number = if let Ok(source_data_request_pass) = indv1.get_first_literal("mnd-s:hasSourceDataRequestForPass") {
                    if let Some(mut indv2) = module.get_individual(&source_data_request_pass) {
                        indv2.get_first_literal("mnd-s:cardNumber")
                    } else {
                        Err(IndividualError::None)
                    }
                } else {
                    Err(IndividualError::None)
                };

                if card_number.is_ok() {
                    let card_number = card_number.unwrap();
                    if let Ok(pass_equipment) = indv1.get_first_literal("mnd-s:passEquipment") {
                        let mut idx = 0;
                        for el in split_str_for_winpak_db_columns(&pass_equipment, 64) {
                            let column = COLUMNS_OF_DEVICES[idx];
                            idx += 1;

                            let future = SqlConnection::connect(conn_str).and_then(|conn| conn.exec(UPD_QUERY, &[&column, &el, &card_number.as_str()]));
                            if let Err(e) = current_thread::block_on_all(future) {
                                error!("fail execute sql query, err={:?}", e);
                            }
                        }
                    }
                }
            } else if s == "d:j2dohw8s79d29mxqwoeut39q92" {
            } else if s == "d:a5w44zg3l6lwdje9kw09je0wzki" {
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
