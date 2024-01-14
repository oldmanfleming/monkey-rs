use crate::object::Object;

pub struct Builtins;

impl Builtins {
    pub fn get() -> Vec<(String, Object)> {
        vec![
            ("len".to_string(), Object::BuiltInFunction(Self::len_fn)),
            ("print".to_string(), Object::BuiltInFunction(Self::print_fn)),
        ]
    }

    fn len_fn(args: Vec<Object>) -> Result<Object, String> {
        if args.len() != 1 {
            return Err(format!(
                "wrong number of arguments for len: want=1, got={}",
                args.len()
            ));
        }

        match args.first() {
            Some(Object::String(value)) => Ok(Object::Integer(value.len() as i64)),
            // Some(Object::Array(value)) => Ok(Object::Integer(value.len() as i64)),
            // Some(Object::Hash(value)) => Ok(Object::Integer(value.len() as i64)),
            Some(other) => Err(format!("argument to `len` not supported, got {}", other)),
            None => Err("argument to `len` not provided".to_string()),
        }
    }

    fn print_fn(args: Vec<Object>) -> Result<Object, String> {
        for arg in args {
            println!("{}", arg);
        }
        Ok(Object::Null)
    }
}
