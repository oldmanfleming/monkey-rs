use core::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::{ast::Statement, environment::Environment};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<String>,
        body: Statement,
        env: Rc<RefCell<Environment>>,
    },
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(value) => write!(f, "return {}", value),
            Object::Function {
                parameters,
                body,
                env: _,
            } => write!(f, "fn({}) {{\n{}\n}}", parameters.join(", "), body),
        }
    }
}
