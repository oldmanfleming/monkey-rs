use std::io::Cursor;

use anyhow::{anyhow, bail, Result};
use byteorder::ReadBytesExt;

use super::{code::Opcode, compiler::Bytecode, object::Object};

const STACK_SIZE: usize = 2048;
const NULL: Object = Object::Null;

pub struct VirtualMachine {
    stack: [Object; STACK_SIZE],
    stack_pointer: usize,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            stack: [NULL; STACK_SIZE],
            stack_pointer: 0,
        }
    }

    pub fn run(&mut self, bytecode: Bytecode) -> Result<Object> {
        let mut instructions = Cursor::new(bytecode.instructions.inner());

        while !instructions.is_empty() {
            let opcode = Opcode::from(instructions.read_u8()?);
            match opcode {
                Opcode::Constant => {
                    let constant_index = instructions.read_u16::<byteorder::BigEndian>()? as usize;
                    let constant = bytecode.constants[constant_index].clone();
                    self.push(constant)?;
                }
                Opcode::Add => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    match (left, right) {
                        (Object::Integer(left), Object::Integer(right)) => {
                            self.push(Object::Integer(left + right))?
                        }
                        _ => todo!(),
                    }
                }
            }
        }

        Ok(self.stack_top().ok_or(anyhow!("no stack result"))?.clone())
    }

    fn push(&mut self, object: Object) -> Result<()> {
        if self.stack_pointer >= STACK_SIZE {
            bail!("stack overflow");
        }

        self.stack[self.stack_pointer] = object;
        self.stack_pointer += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        if self.stack_pointer == 0 {
            bail!("stack underflow");
        }

        self.stack_pointer -= 1;
        Ok(self.stack[self.stack_pointer].clone())
    }

    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.get(self.stack_pointer - 1)
    }
}

#[cfg(test)]
mod tests {
    use crate::{compiler::compiler::Compiler, Lexer, Parser};

    use super::*;

    #[test]
    fn test() {
        let tests = vec![
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(3)),
        ];

        for (input, expected_stack) in tests {
            run_vm_tests(input, expected_stack);
        }
    }

    fn run_vm_tests(input: &str, expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let mut compiler = Compiler::new();
        compiler.compile(program).unwrap();
        let bytecode = compiler.bytecode();

        let mut vm = VirtualMachine::new();
        vm.run(bytecode).unwrap();

        test_expected_object(&expected, vm.stack_top().unwrap());
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        assert_eq!(expected, actual);
    }
}
