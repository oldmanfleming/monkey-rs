use std::{collections::HashMap, io::Cursor};

use anyhow::{anyhow, bail, Result};
use byteorder::ReadBytesExt;

use super::{
    code::{Instructions, Opcode},
    compiler::Bytecode,
    object::Object,
};

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;
const FRAMES_SIZE: usize = 1024;

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

#[derive(Debug, Clone)]
struct Frame {
    instructions: Cursor<Vec<u8>>,
    base_pointer: usize,
}

impl Frame {
    fn new(instructions: &Instructions, base_pointer: usize) -> Self {
        Self {
            instructions: Cursor::new(instructions.inner().clone()),
            base_pointer,
        }
    }

    fn instructions(&mut self) -> &mut Cursor<Vec<u8>> {
        &mut self.instructions
    }
}

pub struct VirtualMachine {
    constants: Vec<Object>,
    globals: Vec<Object>,

    stack: Vec<Object>,
    frames: Vec<Frame>,
    last_popped_elem: Option<Object>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            constants: vec![],
            globals: vec![NULL; GLOBALS_SIZE],
            stack: Vec::with_capacity(STACK_SIZE),
            frames: vec![],
            last_popped_elem: None,
        }
    }

    pub fn run(&mut self, bytecode: Bytecode) -> Result<Object> {
        let main_frame = Frame::new(&bytecode.instructions, 0);
        self.frames = Vec::with_capacity(FRAMES_SIZE);
        self.frames.push(main_frame);
        self.constants = bytecode.constants;

        while !self.current_instructions()?.is_empty() {
            let opcode = Opcode::try_from(self.current_instructions()?.read_u8()?)?;
            match opcode {
                Opcode::Constant => {
                    let constant_index =
                        self.current_instructions()?
                            .read_u16::<byteorder::BigEndian>()? as usize;
                    let constant = self.constants[constant_index].clone();
                    self.push(constant);
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
                    self.push(TRUE);
                }
                Opcode::False => {
                    self.push(FALSE);
                }
                Opcode::Minus => {
                    self.execute_minus_operator()?;
                }
                Opcode::Bang => {
                    self.execute_bang_operator()?;
                }
                Opcode::Jump => {
                    let instructions = self.current_instructions()?;
                    let position = instructions.read_u16::<byteorder::BigEndian>()? as u64;
                    instructions.set_position(position);
                }
                Opcode::JumpNotTruthy => {
                    let position =
                        self.current_instructions()?
                            .read_u16::<byteorder::BigEndian>()? as u64;
                    let condition = self.pop()?;
                    if !self.is_truthy(condition) {
                        self.current_instructions()?.set_position(position);
                    }
                }
                Opcode::Null => {
                    self.push(NULL);
                }
                Opcode::SetGlobal => {
                    let global_index =
                        self.current_instructions()?
                            .read_u16::<byteorder::BigEndian>()? as usize;
                    self.globals[global_index] = self.pop()?;
                }
                Opcode::GetGlobal => {
                    let global_index =
                        self.current_instructions()?
                            .read_u16::<byteorder::BigEndian>()? as usize;
                    let global = self.globals[global_index].clone();
                    self.push(global);
                }
                Opcode::Array => {
                    let num_elements =
                        self.current_instructions()?
                            .read_u16::<byteorder::BigEndian>()? as usize;
                    let mut elements = Vec::with_capacity(num_elements);
                    for _ in 0..num_elements {
                        elements.push(self.pop()?);
                    }
                    elements.reverse();
                    self.push(Object::Array(elements));
                }
                Opcode::Hash => {
                    let num_elements =
                        self.current_instructions()?
                            .read_u16::<byteorder::BigEndian>()? as usize;
                    let mut hash = HashMap::with_capacity(num_elements * 2);
                    for _ in 0..num_elements {
                        let value = self.pop()?;
                        let key = self.pop()?;
                        if !key.hashable() {
                            bail!("unusable as hash key: {}", key);
                        }
                        hash.insert(key, value);
                    }
                    self.push(Object::Hash(hash));
                }
                Opcode::Index => {
                    let index = self.pop()?;
                    let left = self.pop()?;
                    self.execute_index_expression(left, index)?;
                }
                Opcode::Call => {
                    // let num_args = self.current_instructions()?.read_u8()? as usize;
                    // We don't pop from the stack here, poping the function will be the job of the return opcodes
                    match self.stack.last().ok_or(anyhow!("no function found"))? {
                        Object::CompiledFunction {
                            instructions,
                            num_locals,
                        } => {
                            let num_locals = num_locals.clone();
                            let frame = Frame::new(instructions, self.stack.len() - 1);
                            self.push_frame(frame);
                            for _ in 0..num_locals {
                                self.push(NULL);
                            }
                        }
                        object => bail!("calling non-function: {:?}", object),
                    }
                }
                Opcode::ReturnValue => {
                    let return_value = self.pop()?;
                    let frame = self.pop_frame()?;
                    self.stack.drain(frame.base_pointer..);
                    self.push(return_value);
                }
                Opcode::Return => {
                    let frame = self.pop_frame()?;
                    self.stack.drain(frame.base_pointer..);
                    self.push(NULL);
                }
                Opcode::SetLocal => {
                    let local_index = self.current_instructions()?.read_u8()? as usize;
                    let frame = self.frames.last().ok_or(anyhow!("no frame found"))?;
                    let local = frame.base_pointer + local_index;
                    let value = self.pop()?;
                    self.stack[local] = value;
                }
                Opcode::GetLocal => {
                    let local_index = self.current_instructions()?.read_u8()? as usize;
                    let frame = self.frames.last().ok_or(anyhow!("no frame found"))?;
                    let local = frame.base_pointer + local_index;
                    let value = self.stack[local].clone();
                    self.push(value);
                }
                _ => bail!("unknown opcode: {:?}", opcode),
            }
        }

        Ok(self
            .last_popped_elem()
            .ok_or(anyhow!("no stack result"))?
            .clone())
    }

    fn current_instructions(&mut self) -> Result<&mut Cursor<Vec<u8>>> {
        Ok(self
            .frames
            .last_mut()
            .ok_or(anyhow!("no frame found"))?
            .instructions())
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame)
    }

    fn pop_frame(&mut self) -> Result<Frame> {
        self.frames.pop().ok_or(anyhow!("no frame found"))
    }

    fn push(&mut self, object: Object) {
        self.stack.push(object);
    }

    fn pop(&mut self) -> Result<Object> {
        match self.stack.pop() {
            Some(object) => {
                self.last_popped_elem = Some(object.clone());
                Ok(object)
            }
            None => bail!("stack is empty"),
        }
    }

    fn last_popped_elem(&self) -> Option<&Object> {
        self.last_popped_elem.as_ref()
    }

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<()> {
        match (left, index) {
            (Object::Array(elements), Object::Integer(index)) => {
                let element = elements.get(index as usize);
                match element {
                    Some(value) => self.push(value.clone()),
                    None => self.push(NULL),
                }
            }
            (Object::Hash(pairs), index) => {
                if !index.hashable() {
                    bail!("unusable as hash key: {}", index);
                }
                match pairs.get(&index) {
                    Some(value) => self.push(value.clone()),
                    None => self.push(NULL),
                }
            }
            (left, _) => bail!("index operator not supported: {:?}", left),
        }
        Ok(())
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
                self.push(result)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                let result = match opcode {
                    Opcode::Equal => self.native_boolean_to_boolean_object(left == right),
                    Opcode::NotEqual => self.native_boolean_to_boolean_object(left != right),
                    _ => bail!("unknown boolean operator: {:?}", opcode),
                };
                self.push(result)
            }
            (Object::String(left), Object::String(right)) => {
                let result = match opcode {
                    Opcode::Add => Object::String(format!("{}{}", left, right)),
                    Opcode::Equal => self.native_boolean_to_boolean_object(left == right),
                    Opcode::NotEqual => self.native_boolean_to_boolean_object(left != right),
                    _ => bail!("unknown string operator: {:?}", opcode),
                };
                self.push(result)
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

    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(value) => value,
            _ => true,
        }
    }

    fn native_boolean_to_boolean_object(&self, input: bool) -> Object {
        if input {
            TRUE
        } else {
            FALSE
        }
    }

    fn execute_bang_operator(&mut self) -> Result<()> {
        let operand = self.pop()?;
        match operand {
            TRUE => self.push(FALSE),
            FALSE => self.push(TRUE),
            NULL => self.push(TRUE),
            _ => self.push(FALSE),
        };
        Ok(())
    }

    fn execute_minus_operator(&mut self) -> Result<(), anyhow::Error> {
        let operand = self.pop()?;
        Ok(match operand {
            Object::Integer(value) => {
                self.push(Object::Integer(-value));
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

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            ("{}", Object::Hash(vec![].into_iter().collect())),
            (
                "{1: 2, 2: 3}",
                Object::Hash(
                    vec![
                        (Object::Integer(1), Object::Integer(2)),
                        (Object::Integer(2), Object::Integer(3)),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Object::Hash(
                    vec![
                        (Object::Integer(2), Object::Integer(4)),
                        (Object::Integer(6), Object::Integer(16)),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
        ];

        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][0 + 2]", Object::Integer(3)),
            ("[[1, 2, 3]][0][0]", Object::Integer(1)),
            ("[][0]", Object::Null),
            ("[1, 2, 3][99]", Object::Null),
            ("[1][-1]", Object::Null),
            ("{1: 1, 2: 2}[1]", Object::Integer(1)),
            ("{1: 1, 2: 2}[2]", Object::Integer(2)),
            ("{1: 1}[0]", Object::Null),
            ("{}[0]", Object::Null),
        ];

        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        let tests = vec![
            (
                "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();",
                Object::Integer(15),
            ),
            (
                "let one = fn() { 1; }; let two = fn() { 2; }; one() + two()",
                Object::Integer(3),
            ),
            (
                "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c()",
                Object::Integer(3),
            ),
        ];

        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    #[test]
    fn test_functions_with_return_statement() {
        let tests = vec![
            (
                "let earlyExit = fn() { return 99; 100; }; earlyExit();",
                Object::Integer(99),
            ),
            (
                "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
                Object::Integer(99),
            ),
        ];

        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![
            ("let noReturn = fn() { }; noReturn();", Object::Null),
            ("let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); noReturnTwo();", Object::Null),
        ];

        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    #[test]
    fn test_first_class_functions() {
        let tests = vec![
            (
                "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();",
                Object::Integer(1),
            ),
            (
                "let returnsOneReturner = fn() { let returnsOne = fn() { 1; }; returnsOne; }; returnsOneReturner()();",
                Object::Integer(1),
            ),
        ];

        for (input, expected) in tests {
            run_vm_tests(input, expected);
        }
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = vec![
            (
                "let one = fn() { let one = 1; one }; one();",
                Object::Integer(1),
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
                Object::Integer(3),
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; let threeAndFour = fn() { let three = 3; let four = 4; three + four; }; oneAndTwo() + threeAndFour();",
                Object::Integer(10),
            ),
            (
                "let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();",
                Object::Integer(150),
            ),
            (
                "let globalSeed = 50; let minusOne = fn() { let num = 1; globalSeed - num; }; let minusTwo = fn() { let num = 2; globalSeed - num; }; minusOne() + minusTwo();",
                Object::Integer(97),
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
