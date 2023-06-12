use std::{vec, hash::Hash};

type Byte = u8;
type Instruction = Vec<Byte>;
type _Instructions = Vec<Instruction>;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub enum OpCode {
    OpConstant,
}

#[derive(Debug, Clone)]
pub struct OpcodeLayout {
    pub name: OpCode,
    pub operand_widths: Vec<usize>, // number of bytes each operand takes up
}

impl OpcodeLayout {
    pub fn new(name: OpCode) -> Self {
        match name {
            OpCode::OpConstant => OpcodeLayout {
                name,
                operand_widths: vec![2],
            },
        }
    }
}

/// Creates a bytecode instruction from an opcode and operands
/// 
/// # Arguments
/// 
/// * `opcode_name` - The opcode to use
/// * `operands` - The operands to use
/// 
/// # Returns
/// 
/// * `Result<Instruction, String>` - The bytecode instruction
///     * `Ok(Instruction)` - The bytecode instruction
///    * `Err(String)` - The error message
/// 
/// # Examples
/// 
/// ```
/// use monkey_lang::virtual_machine::bytecode::{make_instruction, OpCode};
/// 
/// let result = make_instruction(OpCode::OpConstant, vec![65534]);
/// assert_eq!(result, Ok(vec![0, 255, 254]));
/// ```
/// 
/// ```
/// use monkey_lang::virtual_machine::bytecode::{make_instruction, OpCode};
/// 
/// let result = make_instruction(OpCode::OpConstant, vec![65535]);
/// assert_eq!(result, Ok(vec![0, 255, 255]));
/// ```
pub fn make_instruction(opcode_name: OpCode, operands: Vec<usize>) -> Result<Instruction, String> {
    let opcode_layout = OpcodeLayout::new(opcode_name);
    let expected_operand_count = opcode_layout.operand_widths.len();

    let actual_operand_count = operands.len();

    if expected_operand_count != actual_operand_count {
        return Err(format!(
            "Expected {} operands, got {}",
            expected_operand_count,
            actual_operand_count,
        ));
    }

    let mut byte_operands = Vec::new();
    for i in 0..operands.len() {
        let operand_width = opcode_layout.operand_widths[i];
        let operand = operands[i];

        let mut bytes= match operand_width {
            1 => (operand as u8).to_be_bytes().to_vec(),
            2 => (operand as u16).to_be_bytes().to_vec(),
            _ => return Err(format!("Invalid operand width {}", operand_width)),
        };
        byte_operands.append(&mut bytes);
    }
    let mut instruction = vec![opcode_layout.name as u8];
    instruction.append(&mut byte_operands);
    Ok(instruction) 
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_instruction() {
        struct Test {
            opcode: OpCode,
            operands: Vec<usize>,
            expected: Instruction,
        }

        let inputs = vec![
            Test {
                opcode: OpCode::OpConstant,
                operands: vec![65534],
                expected: vec![0, 255, 254],
            },
            Test {
                opcode: OpCode::OpConstant,
                operands: vec![65535],
                expected: vec![0, 255, 255],
            },
            Test {
                opcode: OpCode::OpConstant,
                operands: vec![45],
                expected: vec![0, 0, 45],
            },
        ];

        for test in inputs {
            let result = match make_instruction(test.opcode, test.operands) {
                Ok(instruction) => instruction,
                Err(error) => panic!("Error: {}", error),
            };
            assert_eq!(result, test.expected);
        }
    }

}