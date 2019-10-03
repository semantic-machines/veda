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

const WINPAK_TIMEZONE: i64 = 3;
const CARD_NUMBER_FIELD_NAME: &str = "mnd-s:cardNumber";
const CARD_DATA_QUERY: &str = "\
SELECT [t1].[ActivationDate], [t1].[ExpirationDate], [t1].[RecordID],
concat([t2].[LastName],' ',[t2].[FirstName],' ',[t2].[Note1]) as Description,
[t2].[Note2] as TabNumber,
[t2].[Note17] as Birthday,
concat( [t2].[Note4]+' ',
CASE WHEN [t2].[Note6]='0' THEN null ELSE [t2].[Note6]+' ' END,
CASE WHEN [t2].[Note7]='0' THEN null ELSE [t2].[Note7]+' ' END,
CASE WHEN [t2].[Note8]='0' THEN null ELSE [t2].[Note8] END) as Comment,
concat( CASE WHEN LTRIM([t2].[Note27])='' THEN null ELSE LTRIM([t2].[Note27]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note28])='' THEN null ELSE LTRIM([t2].[Note28]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note29])='' THEN null ELSE LTRIM([t2].[Note29]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note30])='' THEN null ELSE LTRIM([t2].[Note30]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note33])='' THEN null ELSE LTRIM([t2].[Note33]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note34])='' THEN null ELSE LTRIM([t2].[Note34]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note37])='' THEN null ELSE LTRIM([t2].[Note34]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note38])='' THEN null ELSE LTRIM([t2].[Note34]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note39])='' THEN null ELSE LTRIM([t2].[Note34]+CHAR(13)+CHAR(10)) END,
  CASE WHEN LTRIM([t2].[Note40])='' THEN null ELSE LTRIM([t2].[Note34]+CHAR(13)+CHAR(10)) END) as Equipment
FROM [WIN-PAK PRO].[dbo].[Card] t1
JOIN [WIN-PAK PRO].[dbo].[CardHolder] t2 ON [t2].[RecordID]=[t1].[CardHolderID]
WHERE LTRIM([t1].[CardNumber])=@P1 and [t1].[deleted]=0 and [t2].[deleted]=0";

const ACCESS_LEVEL_QUERY: &str = "\
SELECT [t2].[AccessLevelID]
FROM [WIN-PAK PRO].[dbo].[Card] t1
JOIN [WIN-PAK PRO].[dbo].[CardAccessLevels] t2 ON [t2].[CardID]=[t1].[RecordID]
WHERE LTRIM([t1].[CardNumber])=@P1 and [t1].[deleted]=0 and [t2].[deleted]=0";

pub fn sync_data_from_winpak(module: &mut Module, systicket: &str, conn_str: &str, indv: &mut Individual) -> ResultCode {
    let card_number = indv.get_first_literal(CARD_NUMBER_FIELD_NAME);
    if card_number.is_err() {
        error!("fail read {} {:?}", CARD_NUMBER_FIELD_NAME, card_number.err());
        return ResultCode::UnprocessableEntity;
    }
    let param1 = card_number.unwrap_or_default();

    let mut card_data = (false, 0i64, 0i64, 0i32, "".to_string(), "".to_string(), "".to_string(), "".to_string(), "".to_string());
    let mut access_levels = Vec::new();

    let future = SqlConnection::connect(conn_str)
        .and_then(|conn| {
            conn.query(CARD_DATA_QUERY, &[&param1.as_str()]).for_each(|row| {
                card_data = (
                    true,
                    row.get::<_, NaiveDateTime>(0).sub(Duration::hours(WINPAK_TIMEZONE)).timestamp(),
                    row.get::<_, NaiveDateTime>(1).sub(Duration::hours(WINPAK_TIMEZONE)).timestamp(),
                    row.get::<_, i32>(2).to_owned(),
                    row.get::<_, &str>(3).to_owned(),
                    row.get::<_, &str>(4).to_owned(),
                    row.get::<_, &str>(5).to_owned(),
                    row.get::<_, &str>(6).to_owned(),
                    row.get::<_, &str>(7).to_owned(),
                );
                Ok(())
            })
        })
        .and_then(|conn| {
            conn.query(ACCESS_LEVEL_QUERY, &[&param1.as_str()]).for_each(|row| {
                access_levels.push(row.get::<_, i32>(0).to_owned());
                Ok(())
            })
        });

    if let Err(e) = current_thread::block_on_all(future) {
        error!("fail execute sql query, err={:?}", e);
        match e {
            tiberius::Error::Server(_) => {
                return ResultCode::ConnectError;
            }
            tiberius::Error::Io(_) => {
                return ResultCode::ConnectError;
            }
            _ => {}
        }
    }

    if card_data.0 {
        info!("card_data={:?}", card_data);

        indv.obj.clear("v-s:errorMessage");
        indv.obj.set_datetime("v-s:dateFrom", card_data.1);
        indv.obj.set_datetime("v-s:dateTo", card_data.2);
        indv.obj.set_integer("mnd-s:winpakCardRecordId", card_data.3.into());
        indv.obj.set_string("v-s:description", card_data.4.as_str(), Lang::NONE);
        indv.obj.set_string("v-s:tabNumber", card_data.5.as_str(), Lang::NONE);
        indv.obj.set_string("v-s:birthday", card_data.6.as_str(), Lang::NONE);
        indv.obj.set_string("rdfs:comment", card_data.7.as_str(), Lang::NONE);
        indv.obj.set_string("mnd-s:passEquipment", card_data.8.as_str(), Lang::NONE);

        let mut access_level_uris = Vec::new();
        for level in access_levels {
            let mut indv = Individual::default();
            if module.storage.get_individual(&("d:winpak_accesslevel_".to_string() + &level.to_string()), &mut indv) {
                if let Ok(v) = indv.get_first_literal("v-s:registrationNumber") {
                    if level.to_string() == v {
                        access_level_uris.push(indv.obj.uri);
                    }
                }
            }
        }
        indv.obj.set_uris("mnd-s:hasAccessLevel", access_level_uris);
    } else {
        error!("card [{}] not found in winpak database", param1);

        indv.obj.clear("v-s:dateFrom");
        indv.obj.clear("v-s:dateTo");
        indv.obj.clear("mnd-s:winpakCardRecordId");
        indv.obj.clear("v-s:description");
        indv.obj.clear("v-s:tabNumber");
        indv.obj.clear("v-s:birthday");
        indv.obj.clear("rdfs:comment");
        indv.obj.clear("mnd-s:passEquipment");
        indv.obj.clear("mnd-s:hasAccessLevel");

        indv.obj.clear("v-s:errorMessage");
        indv.obj.add_string("v-s:errorMessage", "Карта не найдена", Lang::RU);
        indv.obj.add_string("v-s:errorMessage", "Card not found", Lang::EN);
    }
    indv.obj.set_uri("v-s:lastEditor", "cfg:VedaSystem");

    let res = module.api.update(systicket, IndvOp::Put, indv);
    if res.result != ResultCode::Ok {
        error!("fail update, uri={}, result_code={:?}", indv.obj.uri, res.result);
        return ResultCode::DatabaseModifiedError;
    } else {
        info!("success update, uri={}", indv.obj.uri);
        return ResultCode::Ok;
    }
}
