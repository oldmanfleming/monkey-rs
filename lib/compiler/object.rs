use core::fmt;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use anyhow::Result;

use super::code::Instructions;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    CompiledFunction {
        instructions: Instructions,
        num_locals: usize,
        num_parameters: usize,
    },
    BuiltInFunction(fn(Vec<Object>) -> Result<Object>),
    Closure {
        function: Box<Object>,
        free: Vec<Object>,
    },
}

impl Object {
    pub fn hashable(&self) -> bool {
        match self {
            Object::Integer(_) | Object::Boolean(_) | Object::String(_) => true,
            _ => false,
        }
    }
}

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
            Object::Hash(hash) => {
                let pairs = hash
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{}}}", pairs)
            }
            Object::CompiledFunction { .. } => {
                write!(f, "fn(){{...}}")
            }
            Object::BuiltInFunction(function) => {
                write!(f, "builtin function {:?}", function)
            }
            Object::Closure { function, .. } => {
                write!(f, "closure({})", function)
            }
        }
    }
}
