mod builtins;
mod environment;
mod evaluator;
mod object;

use anyhow::Result;
pub use evaluator::Evaluator;

use crate::{Engine, Lexer, Parser};

pub fn new_engine() -> Box<dyn Engine> {
    Box::new(EngineImpl {
        evaluator: Evaluator::new(),
    })
}

struct EngineImpl {
    evaluator: Evaluator,
}

impl Engine for EngineImpl {
    fn run(&mut self, input: &str) -> Result<String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;

        let result = self.evaluator.eval(program)?;

        Ok(result.to_string())
    }
}
