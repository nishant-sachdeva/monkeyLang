use lazy_static::lazy_static;
use std::collections::HashMap;

pub type SymbolScope = String;

lazy_static! {
    static ref GLOBAL_SCOPE: SymbolScope = String::from("GLOBAL");
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: i32
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub store: HashMap<String, Symbol>,
    pub number_of_definitions: i32
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            number_of_definitions: 0
        }
    }

    pub fn define(&mut self, symbol_name: String) -> Result<Symbol, String> {
        let symbol = Symbol {
            name: symbol_name.clone(),
            scope: GLOBAL_SCOPE.clone(),
            index: self.number_of_definitions
        };

        self.store.insert(symbol_name, symbol.clone());
        self.number_of_definitions += 1;
        
        return Ok(symbol)
    }

    pub fn resolve(&mut self, symbol_name: String) -> Result<Symbol, String> {
        match self.store.get(&symbol_name) {
            Some(symbol) => Ok(symbol.clone()),
            None => Err(format!("Symbol {} not found", symbol_name))
        }
    }
}

mod test {

    #[test]
    fn test_define_symbols() {
        let expected = super::HashMap::from(
            [
                ("a" , super::Symbol{name: "a".to_string(), scope: super::GLOBAL_SCOPE.clone(), index: 0}),
                ("b" , super::Symbol{name: "b".to_string(), scope: super::GLOBAL_SCOPE.clone(), index: 1}),
            ]
        );

        let mut global = super::SymbolTable::new();

        let a = global.define("a".to_string()).unwrap();

        assert!(a == expected["a"]);

        let b = global.define("b".to_string()).unwrap();

        assert!(b == expected["b"]);
    }

    #[test]
    fn test_resolve_symbol() {
        let mut global = super::SymbolTable::new();
        global.define("a".to_string()).unwrap();
        global.define("b".to_string()).unwrap();

        let expected = vec![
            super::Symbol{name: "a".to_string(), scope: super::GLOBAL_SCOPE.clone(), index: 0},
            super::Symbol{name: "b".to_string(), scope: super::GLOBAL_SCOPE.clone(), index: 1},
        ];

        for sym in expected {
            let result = global.resolve(sym.name.clone()).unwrap();
            assert!(result == sym);
        }
    }
}