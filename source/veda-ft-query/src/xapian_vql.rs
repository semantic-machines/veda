use crate::vql::{Decor, TTA};
use chrono::NaiveDateTime;
use regex::Regex;
use std::collections::HashSet;
use std::io::{Error, ErrorKind};
use v_ft_xapian::key2slot::Key2Slot;
use v_ft_xapian::to_lower_and_replace_delimiters;
use v_ft_xapian::xerror::{Result, XError};
use v_onto::onto::Onto;
use v_search::common::QueryResult;
use xapian_rusty::{Enquire, FeatureFlag, Query, QueryParser, XapianOp};

#[derive(Debug, PartialEq)]
enum TokenType {
    TEXT,
    NUMBER,
    DATE,
    BOOLEAN,
}

pub enum OptAuthorize {
    NO,
    YES,
}

pub fn exec_xapian_query_and_queue_authorize(
    user_uri: &str,
    xapian_enquire: Enquire,
    from: i32,
    top: i32,
    limit: i32,
    add_out_element: fn(uri: &str),
    op_auth: OptAuthorize,
) -> QueryResult {
    Default::default()
}

pub fn transform_vql_to_xapian(
    tta: &mut TTA,
    p_op: &str,
    l_token: Option<&mut String>,
    op: Option<&mut String>,
    query: &mut Query,
    key2slot: &Key2Slot,
    _rd: &mut f64,
    level: i32,
    qp: &mut QueryParser,
    onto: &Onto,
) -> Result<String> {
    let mut query_r = Query::new()?;
    let mut query_l = Query::new()?;
    let mut rd = 0.0;
    let mut ld = 0.0;

    if tta.op == ">" || tta.op == "<" {
        if tta.l.is_none() || tta.r.is_none() {
            return Err(XError::from(Error::new(ErrorKind::Other, format!("transform_vql_to_xapian, invalid tta=[{}]", tta))));
        }

        let mut ls = String::new();
        if let Some(l) = &mut tta.l {
            ls = transform_vql_to_xapian(l, &tta.op, None, None, &mut query_l, key2slot, &mut ld, level + 1, qp, onto)?;
        }

        let mut rs = String::new();
        if let Some(r) = &mut tta.r {
            rs = transform_vql_to_xapian(r, &tta.op, None, None, &mut query_r, key2slot, &mut rd, level + 1, qp, onto)?;
        }

        let (rs_type, value) = get_token_type(&rs)?;
        if rs_type == TokenType::DATE || rs_type == TokenType::NUMBER {
            if let Some(l_out) = l_token {
                *l_out = ls;
            }
            if let Some(op_out) = op {
                *op_out = tta.op.clone();
            }

            *_rd = value;
            return Ok(rs);
        }
    } else if tta.op == "==" || tta.op == "!=" || tta.op == "===" {
        let mut is_strict_equality = false;
        if tta.op == "===" {
            is_strict_equality = true;
            tta.op = "==".to_string();
        }

        if tta.l.is_none() || tta.r.is_none() {
            return Err(XError::from(Error::new(ErrorKind::Other, format!("transform_vql_to_xapian, invalid tta=[{}]", tta))));
        }

        let mut ls = String::new();
        if let Some(l) = &mut tta.l {
            ls = transform_vql_to_xapian(l, &tta.op, None, None, &mut query_l, key2slot, &mut ld, level + 1, qp, onto)?;
        }

        let mut rs = String::new();
        if let Some(r) = &mut tta.r {
            rs = transform_vql_to_xapian(r, &tta.op, None, None, &mut query_r, key2slot, &mut rd, level + 1, qp, onto)?;
        }

        if !is_strict_equality {
            if let Some(s) = add_subclasses_to_query(&rs, &onto) {
                rs = s;
            }
        }

        if query_l.is_empty() && query_r.is_empty() {
            if ls != "*" {
                if ls == "@" {
                    let mut flags = FeatureFlag::FLAG_DEFAULT as i16 | FeatureFlag::FLAG_WILDCARD as i16;
                    let mut query_str = format!("uid_{}", to_lower_and_replace_delimiters(&rs));

                    if tta.op == "!=" {
                        flags = flags | FeatureFlag::FLAG_PURE_NOT as i16;
                        query_str = "NOT ".to_owned() + &query_str;
                    }

                    *query = parse_query(qp, &query_str, flags)?;
                } else {
                    let rs_first_byte = rs.as_bytes()[0];
                    let rs_last_byte = rs.as_bytes()[rs.len() - 1];

                    let wslot = if !rs.is_empty() && rs_first_byte == b'*' && is_good_token(&rs) {
                        key2slot.get_slot(&(ls + "#F"))
                    } else {
                        key2slot.get_slot(&ls)
                    };

                    if let Some(slot) = wslot {
                        let (rs_type, value) = get_token_type(&rs)?;
                        let xtr = format!("X{}D", slot);

                        if rs_type == TokenType::BOOLEAN {
                        } else {
                            if let Some(r) = &tta.r {
                                if r.token_decor == Decor::QUOTED || (rs.find('*').is_some() && is_good_token(&rs)) {
                                    if rs.find('*').is_some() && rs_first_byte == b'+' && !is_good_token(&rs) {
                                        rs = Regex::new(r"[*]").unwrap().replace_all(&rs, "").to_string();
                                    }

                                    let mut query_str = rs;
                                    if rs_first_byte == b'*' {
                                        query_str = query_str.chars().rev().collect();
                                    }

                                    if Regex::new(r"^[a-z0-9_-]+:[a-z0-9_-]*$").unwrap().is_match(&query_str) {
                                        query_str = to_lower_and_replace_delimiters(&query_str);
                                    }

                                    let xtr = format!("X{}X", slot);
                                    let mut flags = FeatureFlag::FLAG_DEFAULT as i16
                                        | FeatureFlag::FLAG_WILDCARD as i16
                                        | FeatureFlag::FLAG_PHRASE as i16
                                        | FeatureFlag::FLAG_LOVEHATE as i16;

                                    if tta.op == "!=" {
                                        flags = flags | FeatureFlag::FLAG_PURE_NOT as i16;
                                        query_str = "NOT ".to_owned() + &query_str;
                                    }

                                    *query = parse_query(qp, &query_str, flags)?;
                                } else if r.token_decor == Decor::RANGE {
                                    let vals: Vec<&str> = rs.split(',').collect();
                                    if vals.len() == 2 {
                                        let (tt, c_from) = get_token_type(vals.get(0).unwrap())?;
                                        if tt == TokenType::DATE || tt == TokenType::NUMBER {
                                            let (tt, c_to) = get_token_type(vals.get(1).unwrap())?;
                                            if tt == TokenType::DATE || tt == TokenType::NUMBER {
                                                *query = Query::new_range(XapianOp::OP_VALUE_RANGE, slot, c_from, c_to)?;
                                            }
                                        }
                                    } else if vals.len() == 1 {
                                        let mut el = rs.clone();
                                        if rs_first_byte == b'\'' && el.len() > 2 && rs_last_byte == b'\'' {
                                            if let Some(s) = &rs.get(1..rs.len() - 1) {
                                                el = s.to_string();
                                            }
                                        }

                                        let mut query_str = el;
                                        let xtr = format!("X{}X", slot);

                                        if !is_strict_equality {
                                            if let Some(s) = add_subclasses_to_query(&rs, &onto) {
                                                query_str = s;
                                            }
                                        }

                                        let mut flags = FeatureFlag::FLAG_DEFAULT as i16
                                            | FeatureFlag::FLAG_WILDCARD as i16
                                            | FeatureFlag::FLAG_PHRASE as i16
                                            | FeatureFlag::FLAG_LOVEHATE as i16;

                                        *query = parse_query(qp, &query_str, flags)?;
                                    } else {
                                        if let Ok(d) = rs.parse::<f64>() {
                                            *query = Query::new_double_with_prefix(&format!("X{}X", slot), d)?;
                                        } else {
                                            return Err(XError::from(Error::new(ErrorKind::Other, format!("transform_vql_to_xapian, invalid tta=[{}]", tta))));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                let mut query_str = rs;
                if Regex::new(r"^[a-z0-9_-]+:[a-z0-9_-]*$").unwrap().is_match(&query_str) {
                    query_str = to_lower_and_replace_delimiters(&query_str);
                }

                if query_str.find('*').is_some() && is_good_token(&query_str) {
                    let mut flags = FeatureFlag::FLAG_DEFAULT as i16 | FeatureFlag::FLAG_WILDCARD as i16 | FeatureFlag::FLAG_LOVEHATE as i16;

                    if tta.op == "!=" {
                        flags = flags | FeatureFlag::FLAG_PURE_NOT as i16;
                        query_str = "NOT ".to_owned() + &query_str;
                    }

                    *query = parse_query(qp, &query_str, flags)?;
                } else {
                    let mut flags = FeatureFlag::FLAG_DEFAULT as i16;

                    *query = parse_query(qp, &query_str, flags)?;
                }
            }
        } else {
        }
    } else if tta.op == "&&" {
    } else if tta.op == "||" {
    } else {
    }

    Ok(Default::default())
}

fn add_subclasses_to_query(rs: &str, onto: &Onto) -> Option<String> {
    if rs.find(':').is_some() {
        let mut new_rs = rs.to_string();
        let mut subclasses = HashSet::new();
        onto.get_subs(&new_rs, &mut subclasses);

        for cs in subclasses.iter() {
            new_rs.push_str(" OR ");
            new_rs.push_str(cs);
        }

        return Some(to_lower_and_replace_delimiters(&new_rs));
    }
    None
}

fn parse_query(qp: &mut QueryParser, query_str: &str, flags: i16) -> Result<Query> {
    if let Ok(q) = qp.parse_query(query_str, flags) {
        Ok(q)
    } else {
        warn!("The search query has been changed: [{}]->[\"{}\"]", query_str, query_str);
        Ok(qp.parse_query(&(format!("\"{}\"", query_str)), flags)?)
    }
}

fn get_token_type(token_in: &str) -> Result<(TokenType, f64)> {
    let res = TokenType::TEXT;

    debug!("token=[{}]", token_in);

    let token = token_in.trim().as_bytes();

    if token == b"true" {
        return Ok((TokenType::BOOLEAN, 1.0));
    } else if token == b"false" {
        return Ok((TokenType::BOOLEAN, 0.0));
    } else if token.len() == 19 && token[4] == b'-' && token[7] == b'-' && token[10] == b'T' && token[13] == b':' && token[16] == b':' {
        if let Ok(nv) = NaiveDateTime::parse_from_str(token_in, "%Y-%m-%dT%H:%M:%S") {
            return Ok((TokenType::DATE, nv.timestamp() as f64));
        }
    } else if token.len() == 24 && token[4] == b'-' && token[7] == b'-' && token[10] == b'T' && token[13] == b':' && token[16] == b':' && token[19] == b'.' {
        if let Ok(nv) = NaiveDateTime::parse_from_str(token_in, "%Y-%m-%dT%H:%M:%S.sss") {
            return Ok((TokenType::DATE, nv.timestamp() as f64));
        }
    }

    if let Ok(v) = token_in.parse::<f64>() {
        return Ok((res, v));
    }

    return Err(XError::from(Error::new(ErrorKind::Other, format!("transform_vql_to_xapian, invalid token=[{}]", token_in))));
}

fn is_good_token(str: &str) -> bool {
    let mut count_alpha = 0;
    let mut count_number = 0;

    for dd in str.chars() {
        if dd.is_alphabetic() {
            count_alpha += 1;
        }
        if dd.is_numeric() {
            count_number += 1;
        }
    }

    debug!("get_count_alpha, str=[{}], count_alpha=[{}]", str, count_alpha);

    if count_alpha + count_number < 3 {
        return false;
    }

    if count_alpha + count_number < 4 && count_number == 3 {
        return false;
    }

    return true;
}
