use crate::common::MyError;
use indextree::{Arena, NodeId};
use quick_xml::events::{BytesStart, Event};
use quick_xml::Reader;
use std::collections::HashMap;
use std::error::Error;
use std::{fs, io, str};
use v_onto::individual::Individual;

#[derive(Default, Debug)]
pub struct XNode {
    id: String,
    tag: String,
    attributes: HashMap<String, String>,
}

#[derive(Default, Debug)]
pub struct IndexedNodeTree {
    pub(crate) id: String,
    arena: Arena<XNode>,
    id2node_id: HashMap<String, NodeId>,
}

impl IndexedNodeTree {
    pub(crate) fn get_id_of_idx(&self, idx: NodeId) -> Result<&str, Box<dyn Error>> {
        if let Some(n) = self.arena.get(idx) {
            return Ok(&n.get().id);
        }
        Err(Box::new(MyError("Not found".into())))
    }

    pub(crate) fn get_idx_of_id(&self, id: &str) -> Result<NodeId, Box<dyn Error>> {
        if let Some(n) = self.id2node_id.get(id) {
            return Ok(*n);
        }
        Err(Box::new(MyError("Not found".into())))
    }

    pub(crate) fn get_type_of_idx(&self, idx: NodeId) -> Result<&str, Box<dyn Error>> {
        if let Some(n) = self.arena.get(idx) {
            return Ok(&n.get().tag);
        }
        Err(Box::new(MyError("Not found".into())))
    }

    pub(crate) fn get_attribute_of_idx(&self, idx: NodeId, key: &str) -> Result<&str, Box<dyn Error>> {
        if let Some(n) = self.arena.get(idx) {
            if let Some(v) = n.get().attributes.get(key) {
                return Ok(&*v);
            }
        }
        Err(Box::new(MyError(format!("[{}] not found", key).into())))
    }

    pub(crate) fn get_idx_of_type(&self, type_: &str) -> Vec<NodeId> {
        let mut res = vec![];
        for el in self.id2node_id.values() {
            if let Some(n) = self.arena.get(*el) {
                if n.get().tag == type_ {
                    res.push(*el);
                }
            }
        }
        res
    }

    pub(crate) fn get_idxs_of_path(&self, in_node: &NodeId, path: &[&str]) -> Vec<NodeId> {
        let mut res = vec![];
        if path.is_empty() {
            return res;
        }

        let mut cur_node = *in_node;
        for p in path {
            for c in cur_node.children(&self.arena) {
                if let Some(n) = self.arena.get(c) {
                    let node = n.get();
                    debug!("{}, {}", node.tag, p);
                    if &node.tag == p {
                        cur_node = c;
                        if p == path.last().unwrap() {
                            res.push(c);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        res
    }

    pub(crate) fn get_values_of_tag(&self, node: &NodeId, tag: &str) -> Vec<String> {
        let mut res = vec![];
        for c in node.children(&self.arena) {
            if let Some(n) = self.arena.get(c) {
                let node = n.get();
                if node.tag == tag {
                    if let Some(v) = node.attributes.get("value") {
                        res.push(v.to_owned());
                    }
                }
            }
        }
        res
    }
}

pub(crate) fn get_process_source(process: &mut Individual) -> Result<IndexedNodeTree, Box<dyn Error>> {
    if let Some(bpmn_xml) = process.get_first_literal("bpmn:processDefinition") {
        //info!("{}", bpmn_xml);
        return parse_bpmn(&process.get_id(), &bpmn_xml);
    }
    Err(Box::new(MyError("Not found".into())))
}

fn parse_bpmn(id: &str, src: &str) -> Result<IndexedNodeTree, Box<dyn Error>> {
    let mut buf = Vec::new();
    let file_buf;

    let mut reader = if src.ends_with(".bpmn") {
        file_buf = fs::read_to_string(format!("data/files/{}", src))?;
        Reader::from_str(&file_buf)
    } else {
        Reader::from_str(src)
    };

    reader.trim_text(true);

    let mut nt = IndexedNodeTree {
        id: "".to_string(),
        arena: Default::default(),
        id2node_id: Default::default(),
    };

    let root = nt.arena.new_node(XNode {
        id: "root".to_string(),
        tag: "".to_string(),
        attributes: Default::default(),
    });

    let mut cur_node = root;

    loop {
        match reader.read_event(&mut buf) {
            Ok(Event::Start(ref e)) => {
                let (id, node_id) = start_create_node(e, &mut nt.arena, &mut cur_node)?;
                nt.id2node_id.insert(id, node_id);
            }
            Ok(Event::End(ref _e)) => {
                end_create_node(&mut nt.arena, &mut cur_node)?;
            }
            Ok(Event::Empty(ref e)) => {
                let (id, node_id) = start_create_node(e, &mut nt.arena, &mut cur_node)?;
                nt.id2node_id.insert(id, node_id);
                end_create_node(&mut nt.arena, &mut cur_node)?;
            }
            Ok(Event::Text(e)) => {
                if let Ok(txt) = e.unescape_and_decode(&reader) {
                    add_attribute_to_node(&mut nt.arena, &mut cur_node, "value", &txt)?;
                }
            }
            Ok(Event::Eof) => break, // exits the loop when reaching end of file
            Err(e) => panic!("Error at position {}: {:?}", reader.buffer_position(), e),
            _ => (),
        }

        buf.clear();
    }

    nt.id = id.to_owned();

    Ok(nt)
}

fn add_attribute_to_node(arena: &mut Arena<XNode>, cur_node: &mut NodeId, key: &str, val: &str) -> Result<(), std::str::Utf8Error> {
    if let Some(p) = arena.get_mut(*cur_node) {
        let cc = p.get_mut();
        cc.attributes.insert(key.to_owned(), val.to_owned());
    }
    Ok(())
}

fn end_create_node(arena: &mut Arena<XNode>, cur_node: &mut NodeId) -> Result<(), io::Error> {
    if let Some(p) = arena.get_mut(*cur_node) {
        if let Some(n) = p.parent() {
            *cur_node = n;
        }
    }
    Ok(())
}

fn start_create_node(e: &BytesStart, arena: &mut Arena<XNode>, cur_node: &mut NodeId) -> Result<(String, NodeId), Box<dyn Error>> {
    let tag = str::from_utf8(e.name()).unwrap();

    let node_id = arena.new_node(XNode {
        id: Default::default(),
        tag: tag.to_string(),
        attributes: Default::default(),
    });
    cur_node.append(node_id, arena);
    *cur_node = node_id;

    let mut id = Default::default();

    if let Some(p) = arena.get_mut(node_id) {
        let cc = p.get_mut();

        for att in e.attributes() {
            if let Ok(a) = att {
                if a.key == b"id" {
                    id = std::str::from_utf8(&a.value)?.to_string();
                    cc.id = id.to_string();
                } else {
                    let attr_name = str::from_utf8(a.key)?;
                    cc.attributes.insert(attr_name.to_owned(), std::str::from_utf8(&a.value)?.to_string());
                }
            }
        }
    }

    Ok((id, node_id))
}
