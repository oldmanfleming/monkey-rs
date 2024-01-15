use crate::{builtins::Builtins, object::Object};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: Self::get_hashmap_with_builtins(),
            outer: None,
        }))
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: Self::get_hashmap_with_builtins(),
            outer: Some(outer),
        }))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: &str, value: Object) {
        self.store.insert(name.to_string(), value);
    }

    fn get_hashmap_with_builtins() -> HashMap<String, Object> {
        let builtins = Builtins::get();
        let mut store = HashMap::new();
        for (key, value) in builtins {
            store.insert(key.to_string(), value.clone());
        }
        store
    }
}
