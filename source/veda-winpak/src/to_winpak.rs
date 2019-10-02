use chrono::prelude::*;
//use chrono::{Local, NaiveDateTime};
use v_api::*;
use v_module::module::*;
use v_onto::individual::*;
//use v_search::FTQuery;
use futures::Future;
use futures_state_stream::StateStream;
use std::ops::Sub;
use tiberius::SqlConnection;
use time::Duration;
use tokio::runtime::current_thread;
use v_onto::datatype::Lang;

const COLUMNS_OF_DEVICES: &'static [&'static str] = &["Note27", "Note28", "Note29", "Note30", "Note33", "Note34", "Note37", "Note38", "Note39", "Note40"];

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

    if let Some(indv1) = module.storage.get_individual1(&bt.unwrap(), &mut Individual::default()) {
        if let Ok(s) = indv1.get_first_literal("mnd-s:hasChangeKindForPass") {
            if s == "d:lt6pdbhy2qvwquzgnp22jj2r2w" {
                if let Ok(pass_equipment) = indv1.get_first_literal("mnd-s:passEquipment") {}
            } else if s == "d:j2dohw8s79d29mxqwoeut39q92" {
            } else if s == "d:a5w44zg3l6lwdje9kw09je0wzki" {
            }
        }
    }

    //mnd-s:hasChangeKindForPass

    ResultCode::Ok
}

trait StringUtils {
    fn substring(&self, start: usize, len: usize) -> Self;
    fn partition(&self, len: usize) -> Vec<String>;
}

/*
fn split_str_for_winpak_db_columns () {
    let elements: Vec<String> = if pass_equipment.contains('\n') {
        let elements: Vec<&str> = pass_equipment.split('\n').collect();
        pass_equipment.partition(64)
    } else {
//                        pass_equipment.split('\n')
        pass_equipment.partition(64)
    };


    fn partition(&self, len: usize) -> Vec<String> {
        let mut start = 0;
        let mut end = len;
        let mut res = Vec::new();
        loop {
            if end >= self.len() {
                break;
            }
            res.push(self.substring(start, end));
            start = end;
            end += len;
        }
        res
    }
}
*/
