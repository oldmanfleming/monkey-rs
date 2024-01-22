use std::io::Cursor;

use anyhow::{anyhow, bail, Result};
use byteorder::ReadBytesExt;

use super::{code::Opcode, compiler::Bytecode, object::Object};

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

pub struct VirtualMachine {
    stack: Vec<Object>,
    stack_pointer: usize,

    globals: Vec<Object>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            stack: vec![NULL; STACK_SIZE],
            stack_pointer: 0,

            globals: vec![NULL; GLOBALS_SIZE],
        }
    }

    pub fn last_popped_elem(&self) -> Option<&Object> {
        self.stack.get(self.stack_pointer)
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
                Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::Equal
                | Opcode::NotEqual
                | Opcode::GreaterThan => {
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
                Opcode::Minus => {
                    self.execute_minus_operator()?;
                }
                Opcode::Bang => {
                    self.execute_bang_operator()?;
                }
                Opcode::Jump => {
                    let position = instructions.read_u16::<byteorder::BigEndian>()? as u64;
                    instructions.set_position(position);
                }
                Opcode::JumpNotTruthy => {
                    let position = instructions.read_u16::<byteorder::BigEndian>()? as u64;
                    let condition = self.pop()?;
                    if !self.is_truthy(condition) {
                        instructions.set_position(position);
                    }
                }
                Opcode::Null => {
                    self.push(NULL)?;
                }
                Opcode::SetGlobal => {
                    let global_index = instructions.read_u16::<byteorder::BigEndian>()? as usize;
                    self.globals[global_index] = self.pop()?;
                }
                Opcode::GetGlobal => {
                    let global_index = instructions.read_u16::<byteorder::BigEndian>()? as usize;
                    let global = self.globals[global_index].clone();
                    self.push(global)?;
                }
                Opcode::Array => {
                    let num_elements = instructions.read_u16::<byteorder::BigEndian>()? as usize;
                    let mut elements = Vec::with_capacity(num_elements);
                    for _ in 0..num_elements {
                        elements.push(self.pop()?);
                    }
                    elements.reverse();
                    self.push(Object::Array(elements))?;
                }
                _ => bail!("unknown opcode: {:?}", opcode),
            }
        }

        Ok(self
            .last_popped_elem()
            .ok_or(anyhow!("no stack result"))?
            .clone())
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

    fn execute_binary_operation(&mut self, opcode: Opcode) -> Result<(), anyhow::Error> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                let result = match opcode {
                    Opcode::Add => Object::Integer(left + right),
                    Opcode::Sub => Object::Integer(left - right),
                    Opcode::Mul => Object::Integer(left * right),
                    Opcode::Div => {
                        if right == 0 {
                            bail!("division by zero");
                        }

                        Object::Integer(left / right)
                    }
                    Opcode::Equal => self.native_boolean_to_boolean_object(left == right),
                    Opcode::NotEqual => self.native_boolean_to_boolean_object(left != right),
                    Opcode::GreaterThan => self.native_boolean_to_boolean_object(left > right),
                    _ => bail!("unknown integer operator: {:?}", opcode),
                };
                self.push(result)?
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                let result = match opcode {
                    Opcode::Equal => self.native_boolean_to_boolean_object(left == right),
                    Opcode::NotEqual => self.native_boolean_to_boolean_object(left != right),
                    _ => bail!("unknown boolean operator: {:?}", opcode),
                };
                self.push(result)?
            }
            (Object::String(left), Object::String(right)) => {
                let result = match opcode {
                    Opcode::Add => Object::String(format!("{}{}", left, right)),
                    Opcode::Equal => self.native_boolean_to_boolean_object(left == right),
                    Opcode::NotEqual => self.native_boolean_to_boolean_object(left != right),
                    _ => bail!("unknown string operator: {:?}", opcode),
                };
                self.push(result)?
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

    fn native_boolean_to_boolean_object(&self, input: bool) -> Object {
        if input {
            TRUE
        } else {
            FALSE
        }
    }

    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(value) => value,
            _ => true,
        }
    }

    fn execute_bang_operator(&mut self) -> Result<()> {
        let operand = self.pop()?;
        match operand {
            TRUE => self.push(FALSE)?,
            FALSE => self.push(TRUE)?,
            NULL => self.push(TRUE)?,
            _ => self.push(FALSE)?,
        };
        Ok(())
    }

    fn execute_minus_operator(&mut self) -> Result<(), anyhow::Error> {
        let operand = self.pop()?;
        Ok(match operand {
            Object::Integer(value) => {
                self.push(Object::Integer(-value))?;
            }
            _ => bail!("unsupported type for negation: {:?}", operand),
        })
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
            ("-(5 + 2)", Object::Integer(-7)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
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
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!5", Object::Boolean(false)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
        ];

        for (input, expected_stack) in tests {
            run_vm_tests(input, expected_stack);
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (true) { 10 } else { 20 }", Object::Integer(10)),
            ("if (false) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (false) { 10 }", Object::Null),
            (
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                Object::Integer(20),
            ),
            ("!if (false) { 10 }", Object::Boolean(true)),
        ];

        for (input, expected_stack) in tests {
            run_vm_tests(input, expected_stack);
        }
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            ("let one = 1; one", Object::Integer(1)),
            ("let one = 1; let two = 2; one + two", Object::Integer(3)),
            (
                "let one = 1; let two = one + one; one + two",
                Object::Integer(3),
            ),
        ];

        for (input, expected_stack) in tests {
            run_vm_tests(input, expected_stack);
        }
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            ("\"monkey\"", Object::String(String::from("monkey"))),
            ("\"mon\" + \"key\"", Object::String(String::from("monkey"))),
            (
                "\"mon\" + \"key\" + \"banana\"",
                Object::String(String::from("monkeybanana")),
            ),
        ];

        for (input, expected_stack) in tests {
            run_vm_tests(input, expected_stack);
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            ("[]", Object::Array(vec![])),
            (
                "[1, 2, 3]",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(12),
                    Object::Integer(11),
                ]),
            ),
        ];
        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    fn run_vm_tests(input: &str, expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(program).unwrap();

        let mut vm = VirtualMachine::new();
        vm.run(bytecode).unwrap();

        test_expected_object(&expected, vm.last_popped_elem().unwrap());
    }

    fn test_expected_object(expected: &Object, actual: &Object) {
        assert_eq!(expected, actual);
    }
}
