use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    GlobalScope,
    LocalScope,
    BuiltinScope,
    FreeScope,
    FunctionScope,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    store: HashMap<String, Symbol>,
    pub num_definitions: usize,
    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
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

    fn define_free(&mut self, original: &Symbol) -> Symbol {
        self.free_symbols.push(original.clone());
        let symbol = Symbol {
            name: original.name.clone(),
            index: self.free_symbols.len() - 1,
            scope: SymbolScope::FreeScope,
        };

        self.store.insert(original.name.clone(), symbol.clone());
        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        match self.store.get(name) {
            Some(symbol) => Some(symbol.clone()),
            None => match &mut self.outer {
                Some(outer) => match outer.resolve(name) {
                    Some(symbol) => {
                        if symbol.scope == SymbolScope::GlobalScope
                            || symbol.scope == SymbolScope::BuiltinScope
                        {
                            Some(symbol)
                        } else {
                            let free = self.define_free(&symbol);
                            Some(free)
                        }
                    }
                    None => None,
                },
                None => None,
            },
        }
    }

    pub fn define_builtin(&mut self, index: usize, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            index,
            scope: SymbolScope::BuiltinScope,
        };

        self.store.insert(name, symbol.clone());

        symbol
    }

    pub fn define_function_name(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            index: 0,
            scope: SymbolScope::FunctionScope,
        };

        self.store.insert(name, symbol.clone());

        symbol
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
            let symbol = local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
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
            let symbol = first_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
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
            let symbol = second_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let mut global = SymbolTable::new();

        let tests = vec![
            ("a", SymbolScope::BuiltinScope, 0),
            ("c", SymbolScope::BuiltinScope, 1),
            ("e", SymbolScope::BuiltinScope, 2),
            ("f", SymbolScope::BuiltinScope, 3),
        ];

        for (index, (name, ..)) in tests.iter().enumerate() {
            global.define_builtin(index, name.to_string());
        }

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = global.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }

        let mut first_local = SymbolTable::new_enclosed(global);

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = first_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }

        let mut second_local = SymbolTable::new_enclosed(first_local);

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = second_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }
    }

    #[test]
    fn test_resolve_free() {
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

        assert_eq!(first_local.free_symbols.len(), 0);
        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = first_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }

        let mut second_local = SymbolTable::new_enclosed(first_local);
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        let tests = vec![
            ("a", SymbolScope::GlobalScope, 0),
            ("b", SymbolScope::GlobalScope, 1),
            ("c", SymbolScope::FreeScope, 0),
            ("d", SymbolScope::FreeScope, 1),
            ("e", SymbolScope::LocalScope, 0),
            ("f", SymbolScope::LocalScope, 1),
        ];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = second_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }
        assert_eq!(second_local.free_symbols.len(), 2);
        assert_eq!(second_local.free_symbols[0].scope, SymbolScope::LocalScope);
        assert_eq!(second_local.free_symbols[0].index, 0);
        assert_eq!(second_local.free_symbols[0].name, "c");
        assert_eq!(second_local.free_symbols[1].scope, SymbolScope::LocalScope);
        assert_eq!(second_local.free_symbols[1].index, 1);
        assert_eq!(second_local.free_symbols[1].name, "d");
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());

        let mut second_local = SymbolTable::new_enclosed(first_local);
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        let tests = vec![
            ("a", SymbolScope::GlobalScope, 0),
            ("c", SymbolScope::FreeScope, 0),
            ("e", SymbolScope::LocalScope, 0),
            ("f", SymbolScope::LocalScope, 1),
        ];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = second_local.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }

        let tests = vec!["b", "d"];
        for name in tests.iter() {
            let symbol = second_local.resolve(name);
            assert_eq!(symbol, None);
        }
    }

    #[test]
    fn define_and_resolve_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a".to_string());

        let tests = vec![("a", SymbolScope::FunctionScope, 0)];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = global.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }
    }

    #[test]
    fn define_shadowing_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a".to_string());
        global.define("a".to_string());

        let tests = vec![("a", SymbolScope::GlobalScope, 0)];

        for (name, expected_scope, expected_index) in tests.iter() {
            let symbol = global.resolve(name).unwrap();
            assert_eq!(symbol.scope, *expected_scope);
            assert_eq!(symbol.index, *expected_index);
        }
    }
}
