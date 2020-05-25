use crate::context::to_lower_and_replace_delimeters;
use crate::error::Result;
use crate::Context;
use chrono::{TimeZone, Utc};
use v_onto::datatype::Lang;
use v_onto::resource::Resource;
use xapian_rusty::Document;

pub struct IndexDocWorkplace {
    pub(crate) doc: Document,
    pub(crate) all_text: String,
    p_text_ru: String,
    p_text_en: String,
}

impl IndexDocWorkplace {
    pub(crate) fn new(doc: Document) -> Self {
        IndexDocWorkplace {
            doc,
            all_text: "".to_string(),
            p_text_ru: "".to_string(),
            p_text_en: "".to_string(),
        }
    }

    pub(crate) fn index_double(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(predicate);

        let l_data = oo.get_float();
        self.doc.add_double(slot_l1, l_data)?;

        let prefix = format!("X{}D", slot_l1);
        ctx.indexer.index_double(l_data, &prefix)?;

        Ok(())
    }

    pub(crate) fn index_boolean(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(predicate);

        let l_data = oo.get_bool();

        let prefix = format!("X{}D", slot_l1);

        if l_data {
            ctx.indexer.index_text_with_prefix("T", &prefix)?;
            self.doc.add_string(slot_l1, "T")?;
        } else {
            ctx.indexer.index_text_with_prefix("F", &prefix)?;
            self.doc.add_string(slot_l1, "F")?;
        }
        Ok(())
    }

    pub(crate) fn index_integer(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(predicate);

        let l_data = oo.get_int() as i32;
        self.doc.add_int(slot_l1, l_data)?;

        let prefix = format!("X{}D", slot_l1);
        ctx.indexer.index_int(l_data, &prefix)?;

        Ok(())
    }

    pub(crate) fn index_date(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(predicate);

        let l_data = oo.get_datetime();
        let data = Utc.timestamp(l_data, 0).to_string();
        self.doc.add_string(slot_l1, &data)?;

        let prefix = format!("X{}D", slot_l1);
        ctx.indexer.index_text_with_prefix(&data, &prefix)?;

        Ok(())
    }

    pub(crate) fn index_uri(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(predicate);

        let data = to_lower_and_replace_delimeters(oo.get_str());

        let prefix = format!("X{}X", slot_l1);
        ctx.indexer.index_text_with_prefix(&data, &prefix)?;

        //all_text.write(data);
        //all_text.write('|');

        Ok(())
    }

    pub(crate) fn index_string(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(predicate);

        let data = oo.get_str();

        if oo.get_lang() == Lang::RU {
            self.p_text_ru.push_str(oo.get_str());
        } else if oo.get_lang() == Lang::EN {
            self.p_text_en.push_str(oo.get_str());
        }

        let prefix = format!("X{}X", slot_l1);
        ctx.indexer.index_text_with_prefix(data, &prefix)?;
        self.doc.add_string(slot_l1, data)?;

        self.all_text.push_str(&data);
        self.all_text.push('|');

        Ok(())
    }

    fn index_string_for_first_wildcard(&mut self, ctx: &mut Context, predicate: &str, oo: &Resource) -> Result<()> {
        let slot_l1 = ctx.get_slot_and_set_if_not_found(&(predicate.to_owned() + "#F"));

        let data: String = oo.get_str().chars().rev().collect();

        let prefix = format!("X{}X", slot_l1);
        ctx.indexer.index_text_with_prefix(&data, &prefix)?;

        Ok(())
    }

    pub(crate) fn doc_add_text_value(&mut self, l_slot: u32, data: &str) -> Result<()> {
        if data.len() > 16 {
            self.doc.add_string(l_slot, &data[..16])?;
        } else {
            self.doc.add_string(l_slot, &data)?;
        }
        Ok(())
    }
}
