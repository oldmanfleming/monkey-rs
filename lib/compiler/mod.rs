mod code;
mod compiler;
mod object;
mod vm;

use anyhow::Result;
pub use compiler::Compiler;
pub use vm::VirtualMachine;

use crate::{Engine, Lexer, Parser};

pub fn new_engine() -> Box<dyn Engine> {
    Box::new(EngineImpl {
        vm: VirtualMachine::new(),
    })
}

struct EngineImpl {
    vm: VirtualMachine,
}

impl Engine for EngineImpl {
    fn run(&mut self, input: &str) -> Result<String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program()?;

        let mut compiler = Compiler::new();

        compiler.compile(program)?;

        let bytecode = compiler.bytecode();

        let result = self.vm.run(bytecode)?;

        Ok(result.to_string())
    }
}
