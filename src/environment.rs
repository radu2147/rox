use std::collections::BTreeMap;

use crate::interpreter::{RunTimeError, Value};
use std::cell::RefCell;
use std::rc::Rc;

struct EnvNode {
    map: BTreeMap<String, Value>,
    parent: Option<Rc<RefCell<EnvNode>>>,
}

impl EnvNode {
    fn new() -> Self {
        EnvNode {
            map: BTreeMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Rc<RefCell<EnvNode>>) -> Self {
        EnvNode {
            map: BTreeMap::new(),
            parent: Some(parent),
        }
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.map.get(key).map(Clone::clone)
    }

    fn set(&mut self, key: &str, value: Value) {
        self.map.insert(key.to_owned(), value);
    }
}

#[derive(Clone)]
pub struct Environment {
    node: Rc<RefCell<EnvNode>>,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        let ptr = &*self.node as *const _;
        let other_ptr = &*other.node as *const _;
        ptr == other_ptr
    }
}

impl Environment {
    pub fn new() -> Self {
        let node = Rc::new(RefCell::new(EnvNode::new()));
        Environment { node }
    }

    pub fn define(&mut self, var: String, value: Value) {
        self.node.borrow_mut().set(&var, value)
    }

    pub fn assign(&mut self, name: String, value: Value) -> Result<(), RunTimeError> {
        let mut node_it = self.node.clone();
        loop {
            if node_it.borrow().map.contains_key(&name) {
                node_it.borrow_mut().map.insert(name, value);
                return Ok(());
            }
            if node_it.borrow().parent.is_some() {
                let cel = node_it.borrow_mut().parent.clone().unwrap();
                node_it = cel;
            } else {
                break;
            }
        }

        Err(RunTimeError {
            message: format!("Undefined variable {name}").to_string(),
            return_error: false,
        })
    }

    pub fn get(&self, name: &String) -> Result<Value, RunTimeError> {
        let mut node_it = self.node.clone();
        loop {
            if node_it.borrow().map.contains_key(name) {
                return Ok(node_it.borrow().map.get(name).unwrap().clone());
            }
            if node_it.borrow().parent.is_some() {
                node_it = {
                    let c = node_it.borrow().parent.clone().unwrap();
                    c
                };
            } else {
                break;
            }
        }

        Err(RunTimeError {
            message: format!("Undefined variable {name}").to_string(),
            return_error: false,
        })
    }

    pub fn extend(&self) -> Self {
        let node = Rc::new(RefCell::new(EnvNode::with_parent(self.node.clone())));
        Environment { node }
    }
}
