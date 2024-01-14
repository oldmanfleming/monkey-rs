use core::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::{ast::Statement, environment::Environment};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
    Array(Vec<Object>),
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<String>,
        body: Statement,
        env: Rc<RefCell<Environment>>,
    },
    BuiltInFunction(fn(Vec<Object>) -> Result<Object, String>),
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
            } => write!(f, "fn({}) {{\n{}\n}}", parameters.join(", "), body),
            Object::BuiltInFunction(function) => {
                write!(f, "builtin function {:?}", function)
            }
        }
    }
}
