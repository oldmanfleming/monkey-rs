use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    GlobalScope,
    LocalScope,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    store: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let scope = match &self.outer {
            Some(_) => SymbolScope::LocalScope,
            None => SymbolScope::GlobalScope,
        };
        let symbol = Symbol {
            name: name.clone(),
            index: self.num_definitions,
            scope,
        };

        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        match self.store.get(name) {
            Some(symbol) => Some(symbol),
            None => match &self.outer {
                Some(outer) => outer.resolve(name),
                None => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let mut global = SymbolTable::new();
        let expected = vec![("a".to_string(), 0), ("b".to_string(), 1)];

        for (name, expected_index) in expected.iter() {
            let symbol = global.define(name.to_string());
            assert_eq!(symbol.index, *expected_index);
        }

        let mut first_local = SymbolTable::new_enclosed(global);
        let expected = vec![("c".to_string(), 0), ("d".to_string(), 1)];

        for (name, expected_index) in expected.iter() {
            let symbol = first_local.define(name.to_string());
            assert_eq!(symbol.index, *expected_index);
        }

        let mut second_local = SymbolTable::new_enclosed(first_local);
        let expected = vec![("e".to_string(), 0), ("f".to_string(), 1)];

        for (name, expected_index) in expected.iter() {
            let symbol = second_local.define(name.to_string());
            assert_eq!(symbol.index, *expected_index);
        }
    }

    #[test]
    fn test_resolve() {
        let mut global = SymbolTable::new();
        let expected = vec![("a".to_string(), 0), ("b".to_string(), 1)];

        for (name, _) in expected.iter() {
            global.define(name.to_string());
        }

        for (name, expected_index) in expected.iter() {
            let symbol = global.resolve(name);
            assert_eq!(symbol.unwrap().index, *expected_index);
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let mut local = SymbolTable::new_enclosed(global);
        local.define("c".to_string());
        local.define("d".to_string());

        let tests = vec![
            ("a", SymbolScope::GlobalScope, 0),
            ("b", SymbolScope::GlobalScope, 1),
            ("c", SymbolScope::LocalScope, 0),
            ("d", SymbolScope::LocalScope, 1),
        ];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = local.resolve(name);
            assert_eq!(symbol.unwrap().scope, *expected_scope);
            assert_eq!(symbol.unwrap().index, *expected_index);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());
        first_local.define("d".to_string());

        let tests = vec![
            ("a", SymbolScope::GlobalScope, 0),
            ("b", SymbolScope::GlobalScope, 1),
            ("c", SymbolScope::LocalScope, 0),
            ("d", SymbolScope::LocalScope, 1),
        ];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = first_local.resolve(name);
            assert_eq!(symbol.unwrap().scope, *expected_scope);
            assert_eq!(symbol.unwrap().index, *expected_index);
        }

        let mut second_local = SymbolTable::new_enclosed(first_local);
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        let tests = vec![
            ("a", SymbolScope::GlobalScope, 0),
            ("b", SymbolScope::GlobalScope, 1),
            ("e", SymbolScope::LocalScope, 0),
            ("f", SymbolScope::LocalScope, 1),
        ];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = second_local.resolve(name);
            assert_eq!(symbol.unwrap().scope, *expected_scope);
            assert_eq!(symbol.unwrap().index, *expected_index);
        }
    }
}
