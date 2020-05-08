use std::collections::{HashMap, HashSet};
use std::string::ToString;
use v_api::app::ResultCode;
use v_api::IndvOp;
use v_onto::individual::Individual;
use v_onto::onto::Onto;

pub(crate) struct CallbackSharedData {
    pub g_super_classes: String,
    pub g_parent_script_id: String,
    pub g_parent_document_id: String,
    pub g_prev_state: Option<Individual>,
    pub g_user: String,
    //_Buff   g_execute_script;
    pub g_document: Option<Individual>,
    pub g_uri: String,
    pub g_ticket: String,

    pub tnx: Transaction,
}

impl Default for CallbackSharedData {
    fn default() -> Self {
        Self {
            g_super_classes: "".to_string(),
            g_parent_script_id: "".to_string(),
            g_parent_document_id: "".to_string(),
            g_prev_state: None,
            g_user: "".to_string(),
            g_document: None,
            g_uri: "".to_string(),
            g_ticket: "".to_string(),
            tnx: Default::default(),
        }
    }
}

impl CallbackSharedData {
    pub(crate) fn add_to_transaction(tnx: &mut Transaction, cmd: IndvOp, new_indv: &mut Individual, ticket_id: &str, event_id: &str) {}

    pub(crate) fn set_g_parent_script_id_etc(&mut self, event_id: &str) {
        let mut event_id = event_id;

        if !event_id.is_empty() {
            let mut aa: Vec<&str> = event_id.split(";").collect();
            if aa.len() > 0 {
                event_id = aa.get(0).unwrap();
            }

            aa = event_id.split("+").collect();

            if aa.len() >= 2 {
                self.g_parent_script_id = aa.get(1).unwrap().to_string();
                self.g_parent_document_id = aa.get(0).unwrap().to_string();
            } else {
                self.g_parent_script_id = String::default();
                self.g_parent_document_id = String::default();
            }
        } else {
            self.g_parent_script_id = String::default();
            self.g_parent_document_id = String::default();
        }
    }

    pub(crate) fn set_g_super_classes(&mut self, indv_types: &Vec<String>, onto: &Onto) {
        let mut super_classes = HashSet::new();

        for indv_type in indv_types.iter() {
            onto.get_subs(indv_type, &mut super_classes);
        }

        self.g_super_classes = String::new();

        for s in super_classes.iter() {
            if !self.g_super_classes.is_empty() {
                self.g_super_classes.push(',');
            }
            self.g_super_classes.push_str(s);
        }
    }
}

pub(crate) struct TransactionItem {
    op_id: u64,
    uri: String,
    cmd: IndvOp,
    indv: Individual,
    ticket_id: String,
    event_id: String,
    user_uri: String,
    rc: ResultCode,
}

pub(crate) struct Transaction {
    buff: HashMap<String, u64>,
    queue: Vec<u64>,
    data: HashMap<u64, TransactionItem>,
}

impl Default for Transaction {
    fn default() -> Self {
        Self {
            buff: Default::default(),
            queue: vec![],
            data: Default::default(),
        }
    }
}
