use core::fmt;
use std::{
    cell::RefCell,
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use anyhow::Result;

use crate::{ast::Statement, environment::Environment};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<String>,
        body: Box<Statement>,
        env: Rc<RefCell<Environment>>,
    },
    BuiltInFunction(fn(Vec<Object>) -> Result<Object>),
}

impl Object {
    pub fn hashable(&self) -> bool {
        match self {
            Object::Integer(_) | Object::Boolean(_) | Object::String(_) => true,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(value) => value.hash(state),
            Object::String(value) => value.hash(state),
            Object::Boolean(value) => value.hash(state),
            _ => panic!("Cannot hash object {:?}", self),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Array(values) => {
                let values: Vec<String> = values.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", values.join(", "))
            }
            Object::ReturnValue(value) => write!(f, "return {}", value),
            Object::Function {
                parameters,
                body,
                env: _,
            } => write!(f, "fn({}) {{ {} }}", parameters.join(", "), body),
            Object::BuiltInFunction(function) => {
                write!(f, "builtin function {:?}", function)
            }
            Object::Hash(hash) => {
                let pairs = hash
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{}}}", pairs)
            }
        }
    }
}
