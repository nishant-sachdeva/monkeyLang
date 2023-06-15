use std::{vec, hash::Hash, collections::{HashMap}};
use lazy_static::lazy_static;

pub type Byte = u8;
pub type Instruction = Vec<Byte>;
pub type Instructions = Vec<Instruction>;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
pub enum OpCode {
    OpConstant,
}

#[derive(Debug, Clone)]
pub struct OpCodeLayout {
    pub name: OpCode,
    pub operand_widths: Vec<usize>, // number of bytes each operand takes up
}

lazy_static! {
    static ref OPCODE_LAYOUTS: HashMap<OpCode, OpCodeLayout> = {
        HashMap::from([
            (
                OpCode::OpConstant,
                OpCodeLayout {
                    name: OpCode::OpConstant,
                    operand_widths: vec![2],
                },
            ),
        ])
    };
}

pub fn opcode_lookup(opcode: u8) -> Result<OpCode, String> {
    match opcode {
        0 => Ok(OpCode::OpConstant),
        _ => Err(format!("Opcode {} not found", opcode)),
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
/// use monkey_lang::virtual_machine::bytecode::{make_bytecode, OpCode};
/// 
/// let result = make_bytecode(OpCode::OpConstant, vec![65534]);
/// assert_eq!(result, Ok(vec![0, 255, 254]));
/// ```
/// 
/// ```
/// use monkey_lang::virtual_machine::bytecode::{make_bytecode, OpCode};
/// 
/// let result = make_bytecode(OpCode::OpConstant, vec![65535]);
/// assert_eq!(result, Ok(vec![0, 255, 255]));
/// ```
pub fn make_bytecode(opcode_name: OpCode, operands: Vec<usize>) -> Result<Instruction, String> {
    let opcode_layout = match OPCODE_LAYOUTS.get(&opcode_name) {
        Some(layout) => layout,
        None => return Err(format!("Opcode {} not found", opcode_name as u8)),
    };
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

pub fn get_raw_assembly(instructions: Instructions) -> Result<String, String> {
    let mut bytecode = String::new();
    for instruction in instructions {
        bytecode = bytecode + &format!(
            "{}",
            instruction
                .iter()
                .map(|byte| format!("{:02X}", byte)) // format as hexadecimals
                .collect::<String>()
        ) + "\n";
    }
    Ok(bytecode)
}

pub fn read_operands(opcode: OpCode, operands: &str) -> Result<(Vec<usize>, usize), String> {
    println!("opcode: {:?}", opcode);
    println!("operands: {:?}", operands);

    // let operands = operands.as_bytes();
    // println!("operands: {:?}", operands);

    let opcode_layout = match OPCODE_LAYOUTS.get(&opcode) {
        Some(layout) => layout,
        None => return Err(format!("Opcode {} not found", opcode as u8)),
    };

    let mut offset = 0;
    let mut operands_vec = Vec::new();
    for operand_width in opcode_layout.clone().operand_widths {
        let operand = match operand_width {
            2 => {
                usize::from_str_radix(&operands[offset..(offset + 2*operand_width)], 16).unwrap()
            }
            _ => return Err(format!("Invalid operand width {}", operand_width)),
        };
        operands_vec.push(operand);
        offset += operand_width;
    }
    Ok((operands_vec, offset))
}

pub fn format_instruction(opcode: OpCode, operands: Vec<usize>) -> String {
    let opcode_layout = match OPCODE_LAYOUTS.get(&opcode) {
        Some(layout) => layout,
        None => return format!("Opcode {} not found", opcode as u8),
    };

    let operand_count = opcode_layout.operand_widths.len();
    if operands.len() != operand_count {
        return format!(
            "Expected {} operands, got {}",
            operand_count,
            operands.len()
        );
    }

    match opcode {
        OpCode::OpConstant => format!("OpConstant {:?}", operands[0]),
    }
}

pub fn format_raw_assembly(bytecode: String) -> Result<String, String> {
    // break bytecode by /n into a vector of Strings
    let mut bytecode = bytecode.split("\n").collect::<Vec<&str>>();
    bytecode.pop(); // remove last element, which is an empty string
    println!("bytecode: {:?}", bytecode);

    let mut result = String::new();
    let mut offset = 0;
    for instruction in bytecode {
        // get instruction opcode
        let opcode = opcode_lookup(instruction[0..1].parse::<u8>().unwrap()).unwrap();

        // get instruction operands
        let (operands, bytes_read) = read_operands(opcode, &instruction[2..]).unwrap();

        // format instruction
        result = result + &format!("{:04} ", offset) + &format_instruction(opcode, operands) + "\n";

        // increment offset
        offset += bytes_read + 1;
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_operands() {
        struct Test {
            opcode: OpCode,
            operands: Vec<usize>,
            bytes_read: usize,
        }

        let inputs = vec![
            Test {
                opcode: OpCode::OpConstant,
                operands: vec![65534],
                bytes_read: 2,
            },
        ];

        for input in inputs {
            let instruction = get_raw_assembly(
                vec![make_bytecode(input.opcode, input.operands.clone()).unwrap()]
            ).unwrap();

            // first byte of the instruction should be the opcode.
            let opcode = opcode_lookup(instruction[0..1].parse::<u8>().unwrap()).unwrap();

            let (operands, bytes_read) = read_operands(opcode, &instruction[2..instruction.len()-1]).unwrap();

            assert_eq!(bytes_read, input.bytes_read);
            for (i, operand) in input.operands.iter().enumerate() {
                assert_eq!(*operand, operands[i]);
            }
        }
    }

    #[test]
    fn test_format_raw_assembly() {
        struct Test {
            raw_assembly: String,
            expected: String,
        }

        let inputs = vec![
            Test {
                raw_assembly: "000001\n000002\n00FFFF\n00FFFE\n".to_string(),
                expected: String::from("0000 OpConstant 1\n0003 OpConstant 2\n0006 OpConstant 65535\n0009 OpConstant 65534\n"),
            },
        ];

        for test in inputs {
            let result = match format_raw_assembly(test.raw_assembly) {
                Ok(result) => result,
                Err(error) => panic!("Error: {}", error),
            };

            assert_eq!(result, test.expected);
        }
    }

    #[test]
    fn test_make_raw_assembly() {
        struct Test {
            bytecode: Instructions,
            expected: String,
        }

        let inputs = vec![
            Test {
                bytecode: vec![
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![2]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![65535]).unwrap(),
                    make_bytecode(OpCode::OpConstant, vec![65534]).unwrap(),
                ],
                expected: String::from("000001\n000002\n00FFFF\n00FFFE\n"),
            },
            Test {
                bytecode: vec![
                    make_bytecode(OpCode::OpConstant, vec![1]).unwrap(),
                ],
                expected: String::from("000001\n"),
            }
        ];

        for test in inputs {
            let result = match get_raw_assembly(test.bytecode) {
                Ok(result) => result,
                Err(error) => panic!("Error: {}", error),
            };

            assert_eq!(result, test.expected);
        }
    }

    #[test]
    fn test_make_bytecode() {
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
            Test {
                opcode: OpCode::OpConstant,
                operands: vec![0],
                expected: vec![0, 0, 0],
            },
            Test {
                opcode: OpCode::OpConstant,
                operands: vec![1],
                expected: vec![0, 0, 1],
            }
        ];

        for test in inputs {
            let result = match make_bytecode(test.opcode, test.operands) {
                Ok(instruction) => instruction,
                Err(error) => panic!("Error: {}", error),
            };
            assert_eq!(result, test.expected);
        }
    }

}