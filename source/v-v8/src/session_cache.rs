use std::collections::{HashMap, HashSet};
use std::string::ToString;
use v_api::app::ResultCode;
use v_api::{APIClient, IndvOp};
use v_module::module::indv_apply_cmd;
use v_onto::individual::Individual;
use v_onto::onto::Onto;
use v_onto::parser::parse_raw;
use v_storage::remote_indv_r_storage::get_individual;

pub struct CallbackSharedData {
    pub g_key2indv: HashMap<String, Individual>,
    pub g_key2attr: HashMap<String, String>,
}

impl Default for CallbackSharedData {
    fn default() -> Self {
        Self {
            g_key2indv: Default::default(),
            g_key2attr: Default::default(),
        }
    }
}

impl CallbackSharedData {
    pub fn set_g_parent_script_id_etc(&mut self, event_id: &str) {
        let mut event_id = event_id;

        if !event_id.is_empty() {
            let mut aa: Vec<&str> = event_id.split(';').collect();
            if !aa.is_empty() {
                event_id = aa.get(0).unwrap();
            }

            aa = event_id.split('+').collect();

            if aa.len() >= 2 {
                self.g_key2attr.insert("$parent_script_id".to_owned(), aa.get(1).unwrap().to_string());
                self.g_key2attr.insert("$parent_document_id".to_owned(), aa.get(0).unwrap().to_string());
            } else {
                self.g_key2attr.insert("$parent_script_id".to_owned(), String::default());
                self.g_key2attr.insert("$parent_document_id".to_owned(), String::default());
            }
        } else {
            self.g_key2attr.insert("$parent_script_id".to_owned(), String::default());
            self.g_key2attr.insert("$parent_document_id".to_owned(), String::default());
        }
    }

    pub fn set_g_super_classes(&mut self, indv_types: &[String], onto: &Onto) {
        let mut super_classes = HashSet::new();

        for indv_type in indv_types.iter() {
            onto.get_supers(indv_type, &mut super_classes);
        }

        let mut g_super_classes = String::new();

        g_super_classes.push('[');
        for s in super_classes.iter() {
            if g_super_classes.len() > 2 {
                g_super_classes.push(',');
            }
            g_super_classes.push('"');
            g_super_classes.push_str(s);
            g_super_classes.push('"');
        }
        g_super_classes.push(']');

        self.g_key2attr.insert("$super_classes".to_owned(), g_super_classes);
    }
}

pub struct TransactionItem {
    uri: String,
    pub cmd: IndvOp,
    pub indv: Individual,
    ticket_id: String,
    pub rc: ResultCode,
}

pub struct Transaction {
    pub sys_ticket: String,
    pub id: i64,
    pub event_id: String,
    buff: HashMap<String, usize>,
    pub queue: Vec<TransactionItem>,
    pub src: String,
}

impl Default for Transaction {
    fn default() -> Self {
        Self {
            sys_ticket: "".to_string(),
            id: 0,
            event_id: "".to_string(),
            buff: Default::default(),
            queue: vec![],
            src: "".to_string(),
        }
    }
}

impl Transaction {
    fn add_item(&mut self, item: TransactionItem) {
        self.buff.insert(item.uri.clone(), self.queue.len());
        self.queue.push(item);
    }

    pub(crate) fn get_indv(&mut self, id: &str) -> Option<&mut Individual> {
        if let Some(idx) = self.buff.get(id) {
            if let Some(ti) = self.queue.get_mut(*idx) {
                return Some(&mut ti.indv);
            }
        }

        None
    }

    pub(crate) fn add_to_transaction(&mut self, cmd: IndvOp, new_indv: Individual, ticket_id: String, _user_id: String) -> ResultCode {
        let mut ti = TransactionItem {
            uri: "".to_string(),
            cmd,
            indv: new_indv,
            ticket_id,
            rc: ResultCode::Ok,
        };

        if ti.cmd == IndvOp::Remove {
        } else {
            ti.uri = ti.indv.get_id().to_string();

            if ti.cmd == IndvOp::AddTo || ti.cmd == IndvOp::SetIn || ti.cmd == IndvOp::RemoveFrom {
                if let Some(mut prev_indv) = self.get_indv(ti.indv.get_id()) {
                    debug!("{:?} BEFORE: {}", ti.cmd, &prev_indv);
                    debug!("{:?} APPLY: {}", ti.cmd, &ti.indv);
                    indv_apply_cmd(&ti.cmd, &mut prev_indv, &mut ti.indv);
                    debug!("{:?} AFTER: {}", ti.cmd, &prev_indv);
                    ti.indv = Individual::new_from_obj(prev_indv.get_obj());
                } else if let Some(mut prev_indv) = get_individual(ti.indv.get_id()) {
                    if parse_raw(&mut prev_indv).is_ok() {
                        prev_indv.parse_all();
                        debug!("{:?} BEFORE: {}", ti.cmd, &prev_indv);
                        debug!("{:?} APPLY: {}", ti.cmd, &ti.indv);
                        indv_apply_cmd(&ti.cmd, &mut prev_indv, &mut ti.indv);
                        debug!("{:?} AFTER: {}", ti.cmd, &prev_indv);
                        ti.indv = prev_indv;
                    } else {
                        ti.rc = ResultCode::UnprocessableEntity;
                    }
                } else {
                    ti.rc = ResultCode::UnprocessableEntity;
                }

                ti.cmd = IndvOp::Put;
            }
        }

        if ti.rc == ResultCode::Ok {
            self.add_item(ti);
            ResultCode::Ok
        } else {
            ti.rc
        }
    }
}

pub fn commit(tnx: &Transaction, api_client: &mut APIClient) -> ResultCode {
    for ti in tnx.queue.iter() {
        if ti.cmd == IndvOp::Remove && ti.indv.get_id().is_empty() {
            continue;
        }

        if ti.rc != ResultCode::Ok {
            return ti.rc;
        }

        if ti.indv.get_id().is_empty() || ti.indv.get_id().len() < 2 {
            warn!("skip individual with invalid id: {}", ti.indv.to_string());
            continue;
        }

        debug!("commit {}", &ti.indv);
        let res = api_client.update_with_event(&ti.ticket_id, &tnx.event_id, &tnx.src, ti.cmd.clone(), &ti.indv);
        if res.result != ResultCode::Ok {
            error!("commit: op_id={}, code={:?}", res.op_id, res.result);
            if res.result != ResultCode::NotReady {
                return res.result;
            }
        }
    }

    ResultCode::Ok
}
