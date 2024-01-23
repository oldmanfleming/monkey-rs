use std::{fmt, io::Cursor};

use anyhow::{bail, Context, Result};
use byteorder::{ReadBytesExt, WriteBytesExt};

#[derive(Debug, Clone, PartialEq)]
pub struct Instructions(Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Self(vec![])
    }

    #[cfg(test)]
    pub fn from(instructions: Vec<Vec<u8>>) -> Self {
        Self(instructions.into_iter().flatten().collect::<Vec<u8>>())
    }

    pub fn inner(&self) -> &Vec<u8> {
        &self.0
    }

    pub fn extend(&mut self, instruction: Vec<u8>) -> usize {
        let instr_start_pos = self.0.len();
        self.0.extend(instruction);
        instr_start_pos
    }

    pub fn drain_at(&mut self, pos: usize) {
        self.0.drain(pos..);
    }

    pub fn change_u16_operand(&mut self, pos: usize, operand: usize) -> Result<()> {
        let mut instructions = Cursor::new(&mut self.0);
        instructions.set_position((pos + 1) as u64);
        instructions.write_u16::<byteorder::BigEndian>(operand.try_into()?)?;
        Ok(())
    }

    /// Convert an opcode and a vector of operands into a byte vector instruction
    pub fn make(opcode: Opcode, operands: Vec<usize>) -> Result<Vec<u8>> {
        let widths = opcode.operand_width();
        let name = opcode.name();

        let mut instruction = vec![opcode.into()];
        for (i, operand) in operands.into_iter().enumerate() {
            match widths.get(i) {
                Some(2) => {
                    instruction.write_u16::<byteorder::BigEndian>(operand.try_into()?)?;
                }
                Some(1) => {
                    instruction.write_u8(operand.try_into()?)?;
                }
                Some(width) => bail!("unknown operand width: {}", width),
                None => bail!("no operand width for opcode: {}", name),
            }
        }
        Ok(instruction)
    }
}

impl fmt::Display for Instructions {
    /// Print the instructions in a human-readable format
    /// TODO: handle errors instead of unwraping
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut instructions = Cursor::new(&self.0);
        while !instructions.is_empty() {
            write!(f, "{:04} ", instructions.position())?;
            let instruction = Opcode::try_from(instructions.read_u8().unwrap()).unwrap();
            write!(f, "{}", instruction.name())?;

            for width in instruction.operand_width().iter() {
                match width {
                    2 => {
                        let operand = instructions
                            .read_u16::<byteorder::BigEndian>()
                            .context("could not read 2-byte operand")
                            .unwrap();
                        write!(f, " {}", operand)?;
                    }
                    1 => {
                        let operand = instructions
                            .read_u8()
                            .context("could not read 1-byte operand")
                            .unwrap();
                        write!(f, " {}", operand)?;
                    }
                    _ => panic!("Unknown operand width {}", width),
                }
            }

            write!(f, "\n",)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Constant,
    Pop,

    Add,
    Sub,
    Mul,
    Div,

    True,
    False,

    Equal,
    NotEqual,
    GreaterThan,

    Minus,
    Bang,

    JumpNotTruthy,
    Jump,

    Null,

    GetGlobal,
    SetGlobal,

    Array,
    Hash,
}

impl Opcode {
    fn name(&self) -> &'static str {
        match self {
            Opcode::Constant => "Constant",
            Opcode::Add => "Add",
            Opcode::Pop => "Pop",
            Opcode::Sub => "Sub",
            Opcode::Mul => "Mul",
            Opcode::Div => "Div",
            Opcode::True => "True",
            Opcode::False => "False",
            Opcode::Equal => "Equal",
            Opcode::NotEqual => "NotEqual",
            Opcode::GreaterThan => "GreaterThan",
            Opcode::Minus => "Minus",
            Opcode::Bang => "Bang",
            Opcode::JumpNotTruthy => "JumpNotTruthy",
            Opcode::Jump => "Jump",
            Opcode::Null => "Null",
            Opcode::GetGlobal => "GetGlobal",
            Opcode::SetGlobal => "SetGlobal",
            Opcode::Array => "Array",
            Opcode::Hash => "Hash",
        }
    }

    /// The number of elements is the number of operands for the opcode
    ///
    /// The value of each element is the byte width of the operand
    ///
    /// E.g. OpConstant has one operand, which is a 2-byte unsigned integer (u16).
    /// In this case, this operand is an index into the constants pool that contains the actual value.
    fn operand_width(&self) -> Vec<usize> {
        match self {
            Opcode::Constant => vec![2],
            Opcode::Add => vec![],
            Opcode::Pop => vec![],
            Opcode::Sub => vec![],
            Opcode::Mul => vec![],
            Opcode::Div => vec![],
            Opcode::True => vec![],
            Opcode::False => vec![],
            Opcode::Equal => vec![],
            Opcode::NotEqual => vec![],
            Opcode::GreaterThan => vec![],
            Opcode::Minus => vec![],
            Opcode::Bang => vec![],
            Opcode::JumpNotTruthy => vec![2],
            Opcode::Jump => vec![2],
            Opcode::Null => vec![],
            Opcode::GetGlobal => vec![2],
            Opcode::SetGlobal => vec![2],
            Opcode::Array => vec![2],
            Opcode::Hash => vec![2],
        }
    }
}

impl TryFrom<u8> for Opcode {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Self> {
        let opcode = match value {
            0 => Opcode::Constant,
            1 => Opcode::Add,
            2 => Opcode::Pop,
            3 => Opcode::Sub,
            4 => Opcode::Mul,
            5 => Opcode::Div,
            6 => Opcode::True,
            7 => Opcode::False,
            8 => Opcode::Equal,
            9 => Opcode::NotEqual,
            10 => Opcode::GreaterThan,
            11 => Opcode::Minus,
            12 => Opcode::Bang,
            13 => Opcode::JumpNotTruthy,
            14 => Opcode::Jump,
            15 => Opcode::Null,
            16 => Opcode::GetGlobal,
            17 => Opcode::SetGlobal,
            18 => Opcode::Array,
            19 => Opcode::Hash,
            _ => bail!("unknown opcode: {}", value),
        };
        Ok(opcode)
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::Constant => 0,
            Opcode::Add => 1,
            Opcode::Pop => 2,
            Opcode::Sub => 3,
            Opcode::Mul => 4,
            Opcode::Div => 5,
            Opcode::True => 6,
            Opcode::False => 7,
            Opcode::Equal => 8,
            Opcode::NotEqual => 9,
            Opcode::GreaterThan => 10,
            Opcode::Minus => 11,
            Opcode::Bang => 12,
            Opcode::JumpNotTruthy => 13,
            Opcode::Jump => 14,
            Opcode::Null => 15,
            Opcode::GetGlobal => 16,
            Opcode::SetGlobal => 17,
            Opcode::Array => 18,
            Opcode::Hash => 19,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let tests = vec![
            (Opcode::Constant, vec![65534], vec![0u8, 255u8, 254u8]),
            (Opcode::Add, vec![], vec![1u8]),
        ];

        for (opcode, operands, expected) in tests {
            let instruction = Instructions::make(opcode, operands).unwrap();
            assert_eq!(instruction, expected);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = Instructions::from(vec![
            Instructions::make(Opcode::Add, vec![]).unwrap(),
            Instructions::make(Opcode::Constant, vec![2]).unwrap(),
            Instructions::make(Opcode::Constant, vec![65535]).unwrap(),
            Instructions::make(Opcode::Sub, vec![]).unwrap(),
            Instructions::make(Opcode::Mul, vec![]).unwrap(),
            Instructions::make(Opcode::Div, vec![]).unwrap(),
            Instructions::make(Opcode::False, vec![]).unwrap(),
            Instructions::make(Opcode::True, vec![]).unwrap(),
            Instructions::make(Opcode::Equal, vec![]).unwrap(),
            Instructions::make(Opcode::NotEqual, vec![]).unwrap(),
            Instructions::make(Opcode::GreaterThan, vec![]).unwrap(),
            Instructions::make(Opcode::JumpNotTruthy, vec![18]).unwrap(),
            Instructions::make(Opcode::Jump, vec![65535]).unwrap(),
            Instructions::make(Opcode::Null, vec![]).unwrap(),
            Instructions::make(Opcode::SetGlobal, vec![65535]).unwrap(),
            Instructions::make(Opcode::GetGlobal, vec![65535]).unwrap(),
            Instructions::make(Opcode::Array, vec![0]).unwrap(),
            Instructions::make(Opcode::Hash, vec![0]).unwrap(),
        ]);

        let expected = r#"0000 Add
0001 Constant 2
0004 Constant 65535
0007 Sub
0008 Mul
0009 Div
0010 False
0011 True
0012 Equal
0013 NotEqual
0014 GreaterThan
0015 JumpNotTruthy 18
0018 Jump 65535
0021 Null
0022 SetGlobal 65535
0025 GetGlobal 65535
0028 Array 0
0031 Hash 0
"#;

        assert_eq!(instructions.to_string(), expected);
    }
}
