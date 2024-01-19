use anyhow::{anyhow, bail, Result};

use super::object::Object;

pub struct Builtins;

impl Builtins {
    pub fn get() -> Vec<(String, Object)> {
        vec![
            ("len".to_string(), Object::BuiltInFunction(Self::len_fn)),
            ("print".to_string(), Object::BuiltInFunction(Self::print_fn)),
            ("first".to_string(), Object::BuiltInFunction(Self::first_fn)),
            ("last".to_string(), Object::BuiltInFunction(Self::last_fn)),
            ("rest".to_string(), Object::BuiltInFunction(Self::rest_fn)),
            ("push".to_string(), Object::BuiltInFunction(Self::push_fn)),
        ]
    }

    fn print_fn(args: Vec<Object>) -> Result<Object> {
        for arg in args {
            println!("{}", arg);
        }
        Ok(Object::Null)
    }

    fn len_fn(args: Vec<Object>) -> Result<Object> {
        if args.len() != 1 {
            bail!(
                "wrong number of arguments for len: want=1, got={}",
                args.len()
            );
        }

        match args.first() {
            Some(Object::String(value)) => Ok(Object::Integer(value.len() as i64)),
            Some(Object::Array(value)) => Ok(Object::Integer(value.len() as i64)),
            Some(Object::Hash(value)) => Ok(Object::Integer(value.len() as i64)),
            Some(other) => Err(anyhow!("argument to `len` not supported, got {}", other)),
            None => Err(anyhow!("argument to `len` not provided")),
        }
    }

    fn first_fn(args: Vec<Object>) -> Result<Object> {
        if args.len() != 1 {
            bail!(
                "wrong number of arguments for first: want=1, got={}",
                args.len()
            );
        }

        match args.first() {
            Some(Object::Array(value)) => match value.first() {
                Some(value) => Ok(value.clone()),
                None => Ok(Object::Null),
            },
            Some(other) => Err(anyhow!("argument to `first` not supported, got {}", other)),
            None => Err(anyhow!("argument to `first` not provided")),
        }
    }

    fn last_fn(args: Vec<Object>) -> Result<Object> {
        if args.len() != 1 {
            bail!(
                "wrong number of arguments for last: want=1, got={}",
                args.len()
            );
        }

        match args.first() {
            Some(Object::Array(value)) => match value.last() {
                Some(value) => Ok(value.clone()),
                None => Ok(Object::Null),
            },
            Some(other) => Err(anyhow!("argument to `last` not supported, got {}", other)),
            None => Err(anyhow!("argument to `last` not provided")),
        }
    }

    fn rest_fn(args: Vec<Object>) -> Result<Object> {
        if args.len() != 1 {
            bail!(
                "wrong number of arguments for rest: want=1, got={}",
                args.len()
            );
        }

        match args.first() {
            Some(Object::Array(value)) => {
                if value.len() == 0 {
                    return Ok(Object::Null);
                }
                Ok(Object::Array(value[1..].to_vec()))
            }
            Some(other) => Err(anyhow!("argument to `rest` not supported, got {}", other)),
            None => Err(anyhow!("argument to `rest` not provided")),
        }
    }

    fn push_fn(args: Vec<Object>) -> Result<Object> {
        if args.len() != 2 {
            bail!(
                "wrong number of arguments for push: want=2, got={}",
                args.len()
            );
        }

        match (args.get(0), args.get(1)) {
            (Some(Object::Array(arr)), Some(value)) => {
                let mut new_array = arr.clone();
                new_array.push(value.clone());
                Ok(Object::Array(new_array))
            }
            (other, value) => Err(anyhow!(
                "argument to `push` not supported, got {:?} and {:?}",
                other,
                value
            )),
        }
    }
}
