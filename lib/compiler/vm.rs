use std::io::Cursor;

use anyhow::{anyhow, bail, Result};
use byteorder::ReadBytesExt;

use super::{code::Opcode, compiler::Bytecode, object::Object};

const STACK_SIZE: usize = 2048;
const NULL: Object = Object::Null;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

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
            let opcode = Opcode::try_from(instructions.read_u8()?)?;
            match opcode {
                Opcode::Constant => {
                    let constant_index = instructions.read_u16::<byteorder::BigEndian>()? as usize;
                    let constant = bytecode.constants[constant_index].clone();
                    self.push(constant)?;
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(opcode)?;
                }
                Opcode::Pop => {
                    self.pop()?;
                }
                Opcode::True => {
                    self.push(TRUE)?;
                }
                Opcode::False => {
                    self.push(FALSE)?;
                }
            }
        }

        Ok(self
            .last_popped_elem()
            .ok_or(anyhow!("no stack result"))?
            .clone())
    }

    fn execute_binary_operation(&mut self, opcode: Opcode) -> Result<(), anyhow::Error> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                let int = match opcode {
                    Opcode::Add => left + right,
                    Opcode::Sub => left - right,
                    Opcode::Mul => left * right,
                    Opcode::Div => {
                        if right == 0 {
                            bail!("division by zero");
                        }

                        left / right
                    }
                    _ => bail!("unknown integer operator: {:?}", opcode),
                };
                self.push(Object::Integer(int))?
            }
            (left, right) => {
                bail!(
                    "unsupported types for {:?}: {:?} + {:?}",
                    opcode,
                    left,
                    right
                )
            }
        };
        Ok(())
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

    pub fn last_popped_elem(&self) -> Option<&Object> {
        self.stack.get(self.stack_pointer)
    }
}

#[cfg(test)]
mod tests {
    use crate::{compiler::compiler::Compiler, Lexer, Parser};

    use super::*;

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            ("1 + 2", Object::Integer(3)),
            ("1 - 2", Object::Integer(-1)),
            ("1 * 2", Object::Integer(2)),
            ("4 / 2", Object::Integer(2)),
            ("50 / 2 * 2 + 10 - 5", Object::Integer(55)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("5 * (2 + 10)", Object::Integer(60)),
            // ("-(5 + 2)", Object::Integer(-7)),
        ];

        for (input, expected_stack) in tests {
            run_vm_tests(input, expected_stack);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            // ("1 < 2", Object::Boolean(true)),
            // ("1 > 2", Object::Boolean(false)),
            // ("1 < 1", Object::Boolean(false)),
            // ("1 > 1", Object::Boolean(false)),
            // ("1 == 1", Object::Boolean(true)),
            // ("1 != 1", Object::Boolean(false)),
            // ("1 == 2", Object::Boolean(false)),
            // ("1 != 2", Object::Boolean(true)),
            // ("true == true", Object::Boolean(true)),
            // ("false == false", Object::Boolean(true)),
            // ("true == false", Object::Boolean(false)),
            // ("true != false", Object::Boolean(true)),
            // ("false != true", Object::Boolean(true)),
            // ("(1 < 2) == true", Object::Boolean(true)),
            // ("(1 < 2) == false", Object::Boolean(false)),
            // ("(1 > 2) == true", Object::Boolean(false)),
            // ("(1 > 2) == false", Object::Boolean(true)),
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

        test_expected_object(&expected, vm.last_popped_elem().unwrap());
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        assert_eq!(expected, actual);
    }
}
