use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    GlobalScope,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: SymbolScope::GlobalScope,
            index: self.num_definitions,
        };

        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
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
}
