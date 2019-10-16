use chrono::{NaiveDateTime, Utc};
use futures::Future;
use std::ops::Add;
use tiberius::{BoxableIo, Error, Transaction};
use time::Duration;
use v_onto::individual::{Individual, IndividualError};
use voca_rs::chop;

pub const WINPAK_TIMEZONE: i64 = 3;
pub const CARD_NUMBER_FIELD_NAME: &str = "mnd-s:cardNumber";

pub const CARD_DATA_QUERY: &str = "\
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

pub const ACCESS_LEVEL_QUERY: &str = "\
SELECT [t2].[AccessLevelID]
FROM [WIN-PAK PRO].[dbo].[Card] t1
    JOIN [WIN-PAK PRO].[dbo].[CardAccessLevels] t2 ON [t2].[CardID]=[t1].[RecordID]
WHERE LTRIM([t1].[CardNumber])=@P1 and [t1].[deleted]=0 and [t2].[deleted]=0";

// CLEAR CARD

pub fn clear_card<I: BoxableIo + 'static>(card_number: String, transaction: Transaction<I>) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    Box::new(transaction.exec(CLEAR_ACCESS_LEVEL, &[&card_number.as_str()]).and_then(|(_result, trans)| Ok(trans)))
}

// INSERT CARD
const INSERT_CARD: &str = "\
INSERT INTO [WIN-PAK PRO].[dbo].[Card]
(
	AccountID,
	TimeStamp,
	UserID,
	NodeId,
	Deleted,
	UserPriority,
	CardNumber,
	Issue,
	CardHolderID,
	AccessLevelID,
	ActivationDate,
	ExpirationDate,
	NoOfUsesLeft,
	CMDFileID,
	CardStatus,
	Display,
	BackDrop1ID,
	BackDrop2ID,
	ActionGroupID,
	LastReaderHID,
	PrintStatus,
	SpareW1,
	SpareW2,
	SpareW3,
	SpareW4,
	SpareDW1,
	SpareDW2,
	SpareDW3,
	SpareDW4
)
VALUES (1,@P1,0,0,0,0,@P2,0,SCOPE_IDENTITY(),-1,@P3,@P4,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)";

pub fn insert_card<I: BoxableIo + 'static>(
    is_vehicle: bool,
    date_from: Result<i64, IndividualError>,
    date_to: Result<i64, IndividualError>,
    card_number: String,
    vehicle_reg_num: Result<String, IndividualError>,
    vehicle_model: Option<String>,
    suppl_taxid: Option<String>,
    suppl_shlabel: Option<String>,
    comment: String,
    parent_regdat: Option<i64>,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if is_vehicle {
        let label = vehicle_reg_num.unwrap_or_default() + " " + vehicle_model.unwrap_or_default().as_str();

        Box::new(
            transaction
                .exec(
                    INSERT_CARD,
                    &[
                        &Utc::now().naive_utc(),
                        &label.as_str(),
                        &suppl_taxid.unwrap_or_default().as_str(),
                        &suppl_shlabel.unwrap_or_default().as_str(),
                        &label.as_str(),
                        &label.as_str(),
                        &comment.as_str(),
                        &NaiveDateTime::from_timestamp(date_from.unwrap_or_default(), 0).add(Duration::hours(WINPAK_TIMEZONE)),
                        &NaiveDateTime::from_timestamp(date_to.unwrap_or_default(), 0).add(Duration::hours(WINPAK_TIMEZONE)),
                        &parent_regdat.unwrap_or_default(),
                        &card_number.as_str(),
                    ],
                )
                .and_then(|(_result, trans)| Ok(trans)),
        )
    } else {
        Box::new(transaction.simple_exec("").and_then(|(_, trans)| Ok(trans)))
    }
}

// UPDATE CARD DATE

pub const UPDATE_CARD_DATE: &str = "\
UPDATE [WIN-PAK PRO].[dbo].[Card]
    SET [ActivationDate]=@P1, [ExpirationDate]=@P2
    WHERE LTRIM([CardNumber])=@P3 and [deleted]=0";

pub fn update_card_date<I: BoxableIo + 'static>(
    date_from: Result<i64, IndividualError>,
    date_to: Result<i64, IndividualError>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if date_to.is_ok() && date_from.is_ok() {
        Box::new(
            transaction
                .exec(
                    UPDATE_CARD_DATE,
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

// CLEAR ACCESS LEVEL

pub const CLEAR_ACCESS_LEVEL: &str = "\
UPDATE t1
   SET [t1].[Deleted]=1
FROM [WIN-PAK PRO].[dbo].[CardAccessLevels] t1
    JOIN [WIN-PAK PRO].[dbo].[Card] t2 ON [t2].[RecordID]=[t1].[CardID]
WHERE LTRIM([t2].[CardNumber])=@P1 and [t2].[CardHolderID]<>0 and [t1].[deleted]=0 and [t2].[deleted]=0";

pub fn clear_access_level<I: BoxableIo + 'static>(card_number: String, transaction: Transaction<I>) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    Box::new(transaction.exec(CLEAR_ACCESS_LEVEL, &[&card_number.as_str()]).and_then(|(_result, trans)| Ok(trans)))
}

// INSERT ACCESS LEVEL

pub const INSERT_ACCESS_LEVEL: &str = "\
INSERT INTO [WIN-PAK PRO].[dbo].[CardAccessLevels]  (AccountID,TimeStamp,UserID,NodeID,Deleted,UserPriority,CardID,AccessLevelID,SpareW1,SpareW2,SpareW3,SpareW4,SpareDW1,SpareDW2,SpareDW3,SpareDW4)
VALUES (0,@P1,0,0,0,0,
    (SELECT RecordID FROM [WIN-PAK PRO].[dbo].[Card] WHERE LTRIM([CardNumber])=@P2 and [Deleted]=0),
    @P3,0,0,0,0,0,0,0,0)";

pub fn update_access_level<I: BoxableIo + 'static>(
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

// UPDATE EQUIPMENT

pub const LPART_UPD_EQUIPMENT_QUERY: &str = "\
=@P1
FROM [WIN-PAK PRO].[dbo].[CardHolder] t1
JOIN [WIN-PAK PRO].[dbo].[Card] t2 ON [t2].[CardHolderID]=[t1].[RecordId]
WHERE LTRIM([t2].[CardNumber])=@P2 and [t2].[CardHolderID]<>0 and [t1].[deleted]=0 and [t2].[deleted]=0";

pub fn update_equipment<I: BoxableIo + 'static>(
    idx: usize,
    names: Vec<String>,
    values: Vec<String>,
    card_number: String,
    transaction: Transaction<I>,
) -> Box<dyn Future<Item = Transaction<I>, Error = Error>> {
    if idx < values.len() && idx < names.len() {
        let column_name = names.get(idx).unwrap();
        let query = "UPDATE t1 SET [t1].[".to_string() + column_name + "]" + LPART_UPD_EQUIPMENT_QUERY;
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
                .and_then(move |trans| update_equipment(idx + 1, names, values, card_number, trans)),
        )
    } else {
        Box::new(transaction.simple_exec("").and_then(|(_, trans)| Ok(trans)))
    }
}

pub fn split_str_for_winpak_db_columns(src: &str, len: usize, res: &mut Vec<String>) {
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

pub fn get_access_level(indv: &mut Individual, access_levels: &mut Vec<String>) {
    if let Ok(access_levels_uris) = indv.get_literals("mnd-s:hasAccessLevel") {
        for l in access_levels_uris {
            if let Some(nl) = l.rsplit("_").next() {
                access_levels.push(nl.to_string());
            }
        }
    }
}

pub fn get_equipment_list(indv: &mut Individual, list: &mut Vec<String>) {
    if let Ok(pass_equipment) = indv.get_first_literal("mnd-s:passEquipment") {
        split_str_for_winpak_db_columns(&pass_equipment, 64, list);
    }
}

pub fn get_equipment_field_names() -> Vec<String> {
    vec![
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
    ]
}
