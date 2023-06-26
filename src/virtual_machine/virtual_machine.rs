use std::collections::HashMap;

use crate::interpreter::evaluate::object_system::{self, ObjectInterface};
use crate::object_system::*;
use crate::compiler::compiler::RawAssembly;
use crate::virtual_machine::bytecode::{
    OpCode,
    opcode_lookup,
};

pub const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;

pub struct VmStack {
    pub stack: Vec<Object>,
    pub stack_pointer: usize,
}

impl VmStack {
    pub fn new() -> Self {
        VmStack {
            stack: vec![object_system::Object::Null; STACK_SIZE],
            stack_pointer: 0,
        }
    }

    pub fn stack_top(&self) -> Result<Object, String> {
        if self.stack_pointer == 0 {
            return Err(String::from("Stack is empty"));
        }

        let stack_top = self.stack[self.stack_pointer - 1].clone();

        return Ok(stack_top.clone());
    }

    pub fn last_popped_stack_element(&self) -> Result<Object, String> {
        let stack_top = self.stack[self.stack_pointer].clone();

        return Ok(stack_top.clone());
    }

    pub fn stack_pop(&mut self) -> Result<Object, String> {
        if self.stack_pointer == 0 {
            return Err(String::from("Stack is empty"));
        }

        let stack_top = self.stack[self.stack_pointer - 1].clone();
        self.stack_pointer -= 1;

        return Ok(stack_top);
    }

    pub fn push_constant(&mut self, object: Object) -> Result<(), String> {
        if self.stack_pointer >= STACK_SIZE {
            return Err(String::from("Stack overflow"));
        }
        self.stack[self.stack_pointer] = object.clone();
        self.stack_pointer += 1;

        return Ok(());
    }
}

pub struct VmGlobals {
    pub store: Vec<Object>,
    pub number_of_definitions: usize,
}

impl VmGlobals {
    pub fn new() -> Self {
        VmGlobals {
            store: vec![object_system::Object::Null; GLOBALS_SIZE],
            number_of_definitions: 0,
        }
    }
}

pub struct VirtualMachine {
    pub stack: VmStack,
    pub globals: VmGlobals,
    pub assembly: RawAssembly,
}

impl VirtualMachine {
    pub fn new(assembly: RawAssembly) -> Self {
        VirtualMachine {
            stack: VmStack::new(),
            globals: VmGlobals::new(),
            assembly,
        }
    }

    pub fn stack_top(&self) -> Result<Object, String> {
        self.stack.stack_top()
    }

    pub fn stack_pop(&mut self) -> Result<Object, String> {
        self.stack.stack_pop()
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut instruction_pointer = 0;
        while instruction_pointer < self.assembly.instructions.len() {
            let opcode = opcode_lookup(
                usize::from_str_radix(&self.assembly.instructions[instruction_pointer..instruction_pointer+2], 16).unwrap(),
            ).unwrap();
            instruction_pointer += 2;

            match opcode {
                OpCode::OpIndex => {
                    let index = match self.stack.stack_pop() {
                        Ok(object) => object,
                        Err(e) => return Err(e),
                    };

                    let left = match self.stack.stack_pop() {
                        Ok(object) => object,
                        Err(e) => return Err(e),
                    };

                    _ = match self.execute_index_operation(left, index) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpHash => {
                    let number_of_elements = usize::from_str_radix(
                        &self.assembly.instructions[instruction_pointer..instruction_pointer+4],
                        16
                    ).unwrap();
                    instruction_pointer += 4;

                    let mut hash_pairs = HashMap::new();
                    for _ in 0..(number_of_elements/2) {
                        let value = match self.stack.stack_pop() {
                            Ok(object) => object,
                            Err(e) => return Err(e),
                        };
                        let key = match self.stack.stack_pop() {
                            Ok(object) => {
                                match object.object_type() {
                                    object_system::ObjectType::BOOLEAN => {
                                        match object {
                                            Object::Boolean(boolean) => {
                                                HashableObject::Boolean(boolean)
                                            },
                                            _ => return Err(String::from("Unsupported types")),
                                        }
                                    },
                                    object_system::ObjectType::INTEGER => {
                                        match object {
                                            Object::Integer(integer) => {
                                                HashableObject::Integer(integer)
                                            },
                                            _ => return Err(String::from("Unsupported types")),
                                        }
                                    },
                                    object_system::ObjectType::StringObject => {
                                        match object {
                                            Object::StringObject(string) => {
                                                HashableObject::StringObject(string)
                                            },
                                            _ => return Err(String::from("Unsupported types")),
                                        }
                                    },
                                    _ => return Err(String::from("Unsupported types")),
                                }
                            }
                            Err(e) => return Err(e),
                        };

                        hash_pairs.insert(
                            HashableObject::from(key),
                            value
                        );
                    }

                    match self.stack.push_constant(
                        Object::HashObject(object_system::HashObject { pairs: hash_pairs })
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpArray => {
                    let number_of_elements = usize::from_str_radix(
                        &self.assembly.instructions[instruction_pointer..instruction_pointer+4],
                        16
                    ).unwrap();
                    instruction_pointer += 4;

                    let mut array_elements = Vec::new();
                    for _ in 0..number_of_elements {
                        let element = match self.stack.stack_pop() {
                            Ok(object) => object,
                            Err(e) => return Err(e),
                        };
                        array_elements.push(element);
                    }

                    array_elements.reverse();

                    match self.stack.push_constant(
                        Object::ArrayObject(object_system::ArrayObject { elements: array_elements })
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpSetGlobal => {
                    let global_index = self.assembly.instructions[instruction_pointer..instruction_pointer+4].parse::<usize>().unwrap();
                    instruction_pointer += 4;

                    self.globals.store[global_index] = match self.stack.stack_pop() {
                        Ok(object) => object,
                        Err(e) => return Err(e),
                    };
                    self.globals.number_of_definitions += 1;
                }
                OpCode::OpGetGlobal => {
                    let global_index = self.assembly.instructions[instruction_pointer..instruction_pointer+4].parse::<usize>().unwrap();
                    instruction_pointer += 4;

                    match self.stack.push_constant(
                        self.globals.store[global_index].clone()
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                }
                OpCode::OpConstant => {
                    let const_index = self.assembly.instructions[instruction_pointer..instruction_pointer+4].parse::<usize>().unwrap();
                    instruction_pointer += 4;

                    match self.stack.push_constant(
                        self.assembly.constants[const_index].clone()
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpAdd | OpCode::OpDiv | OpCode::OpMul | OpCode::OpSub => {
                    let _ = match self.run_binary_operation(opcode) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };
                },
                OpCode::OpPop => {
                    match self.stack.stack_pop() {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpTrue => {
                    match self.stack.push_constant(
                        Object::Boolean(object_system::Boolean { value: true })
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpFalse => {
                    match self.stack.push_constant(
                        Object::Boolean(object_system::Boolean { value: false })
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                OpCode::OpEqual | OpCode::OpNotEqual | OpCode::OpGreaterThan | OpCode::OpLessThan => {
                    let _ = match self.run_comparison(opcode) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };
                },
                OpCode::OpBang => {
                    let _ = match self.run_bang_operator() {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };
                },
                OpCode::OpMinus => {
                    let _ = match self.run_minus_operator() {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    };
                },
                OpCode::OpJump => {
                    let jump_position = usize::from_str_radix(
                        &self.assembly.instructions[instruction_pointer..instruction_pointer+4],
                        16
                    ).unwrap();
                    instruction_pointer = jump_position*2;
                },
                OpCode::OpJumpNotTruthy => {
                    let jump_position = usize::from_str_radix(
                        &self.assembly.instructions[instruction_pointer..instruction_pointer+4],
                        16
                    ).unwrap();
                    instruction_pointer += 4;

                    let condition = match self.stack.stack_pop() {
                        Ok(object) => object,
                        Err(e) => return Err(e),
                    };

                    match condition {
                        object_system::Object::Boolean(boolean) => {
                            if !boolean.value {
                                instruction_pointer = jump_position*2;
                            }
                        },
                        _ => () // defaults to True => No Jump Required
                    }
                },
                OpCode::OpNull => {
                    match self.stack.push_constant(
                        Object::Null
                    ) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                },
                _ => return Err(format!("Opcode {:?} not implemented", opcode)),
            }
        }

        return Ok(());
    }

    pub fn execute_index_operation(&mut self, left: object_system::Object, index: object_system::Object) -> Result<(), String> {
        match (left.object_type(), index.object_type()) {
            (object_system::ObjectType::ArrayObject, object_system::ObjectType::INTEGER) => {
                match self.execute_array_index_operation(left, index) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            (object_system::ObjectType::HashObject, _) => {
                match self.execute_hash_index_operation(left, index) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            _ => return Err(String::from("Unsupported types")),
        }

        return Ok(());
    }

    pub fn execute_hash_index_operation(&mut self, left: object_system::Object, index: object_system::Object) -> Result<(), String> {
        let left = match left {
            Object::HashObject(hash) => hash.pairs,
            _ => return Err(String::from("Unsupported types")),
        };
        
        let index = match index {
            Object::Boolean(boolean) => HashableObject::Boolean(boolean),
            Object::Integer(integer) => HashableObject::Integer(integer),
            Object::StringObject(string) => HashableObject::StringObject(string),
            _ => return Err(String::from("Unsupported types")),
        };
        
        match left.get(&index) {
            Some(value) => {
                match self.stack.push_constant(
                    value.clone()
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            None => {
                match self.stack.push_constant(
                    Object::Null
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
        }

        return Ok(())
    }

    pub fn execute_array_index_operation(&mut self, left: object_system::Object, index: object_system::Object) -> Result<(), String> {
        let left = match left {
            Object::ArrayObject(array) => array.elements,
            _ => return Err(String::from("Unsupported types")),
        };

        let index = match index {
            Object::Integer(integer) => integer.value,
            _ => return Err(String::from("Unsupported types")),
        };

        if index >=0 && index < left.len() as i64 {
            match self.stack.push_constant(
                left[index as usize].clone()
            ) {
                Ok(_) => (),
                Err(e) => return Err(e),
            }
        } else {
            match self.stack.push_constant(
                Object::Null
            ) {
                Ok(_) => (),
                Err(e) => return Err(e),
            }
        }

        return Ok(())
    }

    pub fn run_minus_operator(&mut self) -> Result<(), String> {
        let operand = match self.stack.stack_pop() {
            Ok(object) => object,
            Err(e) => return Err(e),
        };

        if operand.object_type() != object_system::ObjectType::INTEGER {
            return Err(String::from("Unsupported types"));
        }

        let operand = match operand {
            Object::Integer(integer) => integer.value,
            _ => return Err(String::from("Unsupported types")),
        };

        match self.stack.push_constant(
            Object::Integer(object_system::Integer { value: -operand })
        ) {
            Ok(_) => (),
            Err(e) => return Err(e),
        }

        return Ok(())
    }

    pub fn run_bang_operator(&mut self) -> Result<(), String> {
        let operand = match self.stack.stack_pop() {
            Ok(object) => object,
            Err(e) => return Err(e),
        };

        match operand {
            object_system::Object::Boolean(boolean) => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: !boolean.value })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            _ => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: false })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
        }
        return Ok(())
    }

    pub fn run_comparison(&mut self, opcode: OpCode) -> Result<(), String> {
        let right = match self.stack.stack_pop() {
            Ok(object) => object,
            Err(e) => return Err(e),
        };

        let left = match self.stack.stack_pop() {
            Ok(object) => object,
            Err(e) => return Err(e),
        };

        match (left.object_type(), right.object_type()) {
            (object_system::ObjectType::INTEGER, object_system::ObjectType::INTEGER) => {
                match self.run_int_comparison(opcode, left, right) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            }
            (object_system::ObjectType::BOOLEAN, object_system::ObjectType::BOOLEAN) => {
                match self.run_boolean_comparison(opcode, left, right) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            }
            _ => return Err(String::from("Mismatched/UnSupported types")),
        }

        return Ok(());
    }

    pub fn run_boolean_comparison(&mut self, opcode: OpCode, left: object_system::Object, right: object_system::Object) -> Result<(), String> {
        match opcode {
            OpCode::OpEqual => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: left == right })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            OpCode::OpNotEqual => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: left != right })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            _ => return Err(String::from("Unsupported types")),
        }
        return Ok(())
    }

    pub fn run_int_comparison(&mut self, opcode: OpCode, left: object_system::Object, right: object_system::Object) -> Result<(), String> {
        let left_value = match left {
            Object::Integer(integer) => integer.value,
            _ => return Err(String::from("Unsupported types")),
        };

        let right_value = match right {
            Object::Integer(integer) => integer.value,
            _ => return Err(String::from("Unsupported types")),
        };

        _ =  match opcode {
            OpCode::OpEqual => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: left_value == right_value })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            OpCode::OpNotEqual => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: left_value != right_value })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            OpCode::OpGreaterThan => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: left_value > right_value })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            OpCode::OpLessThan => {
                match self.stack.push_constant(
                    Object::Boolean(object_system::Boolean { value: left_value < right_value })
                ) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            _ => return Err(String::from("Unsupported types")),
        };
        return Ok(())
    }

    pub fn run_binary_operation(&mut self, opcode: OpCode) -> Result<(), String> {
        let right = match self.stack.stack_pop() {
            Ok(object) => object,
            Err(e) => return Err(e),
        };

        let left = match self.stack.stack_pop() {
            Ok(object) => object,
            Err(e) => return Err(e),
        };

        match (left.object_type(), right.object_type()) {
            (object_system::ObjectType::INTEGER, object_system::ObjectType::INTEGER) => {
                match self.run_int_binary_operation(opcode, left, right) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            (object_system::ObjectType::StringObject , object_system::ObjectType::StringObject) => {
                match self.run_string_binary_operation(opcode, left, right) {
                    Ok(_) => (),
                    Err(e) => return Err(e),
                }
            },
            _ => return Err(String::from("Unsupported types")),
        }

        return Ok(());
    }

    pub fn run_string_binary_operation(&mut self, opcode: OpCode, left: object_system::Object, right: object_system::Object) -> Result<(), String> {
        let left = match left {
            Object::StringObject(string) => string.value,
            _ => return Err(String::from("Unsupported types")),
        };

        let right = match right {
            Object::StringObject(string) => string.value,
            _ => return Err(String::from("Unsupported types")),
        };

        let result = match opcode {
            OpCode::OpAdd => left + &right,
            _ => return Err(String::from("Unsupported types")),
        };

        let result_object = Object::StringObject(object_system::StringObject { value: result });

        match self.stack.push_constant(result_object) {
            Ok(_) => (),
            Err(e) => return Err(e),
        }

        return Ok(());
    }

    pub fn run_int_binary_operation(&mut self, opcode: OpCode, left: object_system::Object, right: object_system::Object) -> Result<(), String> {
        let left = match left {
            Object::Integer(integer) => integer.value,
            _ => return Err(String::from("Unsupported types")),
        };

        let right = match right {
            Object::Integer(integer) => integer.value,
            _ => return Err(String::from("Unsupported types")),
        };

        let result = match opcode {
            OpCode::OpAdd => left + right,
            OpCode::OpSub => left - right,
            OpCode::OpMul => left * right,
            OpCode::OpDiv => {
                if right == 0 {
                    return Err(String::from("Division by zero"));
                }
                left / right
            }
            _ => return Err(String::from("Unsupported types")),
        };

        let result_object = Object::Integer(object_system::Integer { value: result });

        match self.stack.push_constant(result_object) {
            Ok(_) => (),
            Err(e) => return Err(e),
        }

        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use std::vec;
    use std::collections::HashMap;

    use crate::virtual_machine::bytecode::format_raw_assembly;
    use crate::{parser::Parser, interpreter::lexer::Lexer};
    use crate::object_system::{
        Object,
        Integer,
    };
    use crate::compiler::compiler::Compiler;
    use crate::interpreter::ast;

    use super::*;

    fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program.unwrap()
    }

    fn test_expected_object(expected: Object, actual: Object) -> Result<(), String> {
        match expected {
            Object::Integer(integer) => {
                test_integer_object(integer.value, actual)
            },
            Object::Boolean(boolean) => {
                test_boolean_object(boolean.value, actual)
            },
            Object::Null => {
                test_null_object(actual)
            },
            Object::StringObject(string) => {
                test_string_object(string.value, actual)
            },
            Object::ArrayObject(array) => {
                test_array_object(array.elements, actual)
            },
            Object::HashObject(hash) => {
                test_hash_object(hash.pairs, actual)
            },
            _ => {
                return Err(format!("Wrong object type. Expected: Integer Actual: {:?}", actual));
            }
        }
    }

    fn test_hash_object(expected: HashMap<HashableObject, Object>, actual: Object) -> Result<(), String> {
        match actual {
            Object::HashObject(hash) => {
                if hash.pairs.len() != expected.len() {
                    return Err(format!("Wrong hash length. Expected: {} Actual: {}", expected.len(), hash.pairs.len()));
                }

                for (key, value) in expected.iter() {
                    let pair = match hash.pairs.get(key) {
                        Some(pair) => pair,
                        None => return Err(format!("Key not found in hash")),
                    };

                    test_expected_object(
                        value.clone(),
                        pair.clone()
                    ).unwrap();
                }
            },
            _ => {
                return Err(format!("Wrong object type. Expected: Hash Actual: {:?}", actual));
            }
        }

        return Ok(());
    }

    fn test_array_object(expected: Vec<Object>, actual: Object) -> Result<(), String> {
        match actual {
            Object::ArrayObject(array) => {
                if array.elements.len() != expected.len() {
                    return Err(format!("Wrong array length. Expected: {} Actual: {}", expected.len(), array.elements.len()));
                }

                for (index, element) in array.elements.iter().enumerate() {
                    test_expected_object(
                        expected.get(index).unwrap().clone(),
                        element.clone()
                    ).unwrap();
                }
            },
            _ => {
                return Err(format!("Wrong object type. Expected: Array Actual: {:?}", actual));
            }
        }

        return Ok(());
    }

    fn test_string_object(expected: String, actual: Object) -> Result<(), String> {
        match actual {
            Object::StringObject(string) => {
                if string.value != expected {
                    return Err(format!("Wrong string value. Expected: {} Actual: {}", expected, string.value));
                }
            },
            _ => {
                return Err(format!("Wrong object type. Expected: String Actual: {:?}", actual));
            }
        }

        return Ok(());
    }

    fn test_null_object(actual: Object) -> Result<(), String> {
        match actual {
            Object::Null => (),
            _ => {
                return Err(format!("Wrong object type. Expected: Null Actual: {:?}", actual));
            }
        }

        return Ok(());
    }

    fn test_boolean_object(expected: bool, actual: Object) -> Result<(), String> {
        match actual {
            Object::Boolean(boolean) => {
                if boolean.value != expected {
                    return Err(format!("Wrong boolean value. Expected: {} Actual: {}", expected, boolean.value));
                }
            },
            _ => {
                return Err(format!("Wrong object type. Expected: Boolean Actual: {:?}", actual));
            }
        }

        return Ok(());
    }

    fn test_integer_object(expected: i64, actual: Object) -> Result<(), String> {
        match actual {
            Object::Integer(integer) => {
                if integer.value != expected {
                    return Err(format!("Wrong integer value. Expected: {} Actual: {}", expected, integer.value));
                }
            },
            _ => {
                return Err(format!("Wrong object type. Expected: Integer Actual: {:?}", actual));
            }
        }

        return Ok(());
    }

    struct VirtualMachineTest {
        pub input: String,
        pub expected_stack: Vec<Object>,
    }

    fn run_vm_tests(tests: Vec<VirtualMachineTest>) {
        for test in tests {
            let mut program = parse(test.input);

            let mut compiler = Compiler::new();
            
            match compiler.compile(&mut program) {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            }

            let raw_assembly = compiler.raw_assembly().unwrap();

            println!("Raw Assembly : {:?}", format_raw_assembly(raw_assembly.instructions.clone()));

            let mut vm = VirtualMachine::new(
                raw_assembly
            );

            match vm.run() {
                Ok(_) => (),
                Err(e) => panic!("{e}"),
            }

            let stack_element = vm.stack.last_popped_stack_element().unwrap();

            test_expected_object(
                test.expected_stack.get(0).unwrap().clone(),
                stack_element.clone()
            ).unwrap();
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VirtualMachineTest {
                input: String::from("1 + 2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 3 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 1 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 2 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 - 2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: -1 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 * 2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 2 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("4 / 2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 2 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("50 / 2 * 2 + 10 - 5"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 55 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("5 * (2 + 10)"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 60 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("-5"),
                expected_stack: vec![
                    Object::Integer(Integer { value: -5 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("-10"),
                expected_stack: vec![
                    Object::Integer(Integer { value: -10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("-50 + 100 + -50"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 0 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("(5 + 10 * 2 + 15 / 3) * 2 + -10"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 50 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("5 + 5 + 5 + 5 - 10"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("2*2*2*2*2"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 32 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("5 * 2 + 10"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 20 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("5 + 2 * 10"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 25 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("5 * (2 + 10)"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 60 }),
                ],
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 < 2"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 > 2"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 < 1"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 > 1"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 == 1"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 != 1"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 == 2"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("1 != 2"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("true == true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("false == false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("true == false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("true != false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("false != true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("(1 < 2) == true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("(1 < 2) == false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("(1 > 2) == true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("(1 > 2) == false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("!true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("!false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("!5"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("!!true"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
            VirtualMachineTest {
                input: String::from("!!false"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: false }),
                ],
            },
            VirtualMachineTest {
                input: String::from("!!5"),
                expected_stack: vec![
                    Object::Boolean(object_system::Boolean { value: true }),
                ],
            },
        ];
        run_vm_tests(inputs);
    }

    #[test]
    fn test_conditionals() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("if (true) { 10 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (true) { 10 } else { 20 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (false) { 10 } else { 20 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 20 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (1) { 10 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (1 < 2) { 10 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (1 < 2) { 10 } else { 20 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 10 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (1 > 2) { 10 } else { 20 }"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 20 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("if (1 > 2) { 10 }"),
                expected_stack: vec![
                    Object::Null,
                ],
            },
            VirtualMachineTest {
                input: String::from("if (false) { 10 }"),
                expected_stack: vec![
                    Object::Null,
                ],
            }
        ];
        run_vm_tests(inputs);
    }

    #[test]
    fn test_global_let_statements() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("let one = 1; one;"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 1 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("let one = 1; let two = 2; one + two;"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 3 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("let one = 1; let two = one + one; one + two;"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 3 }),
                ],
            }
        ];
        run_vm_tests(inputs);
    }

    #[test]
    fn test_string_literals() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("\"monkey\""),
                expected_stack: vec![
                    Object::StringObject(StringObject { value: String::from("monkey") }),
                ],
            },
            VirtualMachineTest {
                input: String::from("\"mon\" + \"key\""),
                expected_stack: vec![
                    Object::StringObject(StringObject { value: String::from("monkey") }),
                ],
            },
            VirtualMachineTest {
                input: String::from("\"mon\" + \"key\" + \"banana\""),
                expected_stack: vec![
                    Object::StringObject(StringObject { value: String::from("monkeybanana") }),
                ],
            }
        ];
        run_vm_tests(inputs);
    }

    #[test]
    fn test_array_literals() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("[]"),
                expected_stack: vec![
                    Object::ArrayObject(object_system::ArrayObject { elements: vec![] }),
                ],
            },
            VirtualMachineTest {
                input: String::from("[1, 2, 3]"),
                expected_stack: vec![
                    Object::ArrayObject(object_system::ArrayObject { elements: vec![
                        Object::Integer(Integer { value: 1 }),
                        Object::Integer(Integer { value: 2 }),
                        Object::Integer(Integer { value: 3 }),
                    ] }),
                ],
            },
            VirtualMachineTest {
                input: String::from("[1 + 2, 3 - 4, 5 * 6]"),
                expected_stack: vec![
                    Object::ArrayObject(object_system::ArrayObject { elements: vec![
                        Object::Integer(Integer { value: 3 }),
                        Object::Integer(Integer { value: -1 }),
                        Object::Integer(Integer { value: 30 }),
                    ] }),
                ],
            },
        ];
        run_vm_tests(inputs);
    }

    #[test]
    fn test_hash_literals() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("{}"),
                expected_stack: vec![
                    Object::HashObject(object_system::HashObject { pairs: HashMap::new() }),
                ],
            },
            VirtualMachineTest {
                input: String::from("{1: 2, 2: 3}"),
                expected_stack: vec![
                    Object::HashObject(object_system::HashObject {
                        pairs: {
                            let mut pairs = HashMap::new();
                            pairs.insert(HashableObject::Integer(Integer { value: 1 }), Object::Integer(Integer { value: 2 }));
                            pairs.insert(HashableObject::Integer(Integer { value: 2 }), Object::Integer(Integer { value: 3 }));
                            pairs
                        } 
                    }),
                ],
            },
            VirtualMachineTest {
                input: String::from("{1+1:2+3, 4+5:5*6}"),
                expected_stack: vec![
                    Object::HashObject(object_system::HashObject {
                        pairs: {
                            let mut pairs = HashMap::new();
                            pairs.insert(HashableObject::Integer(Integer { value: 2 }), Object::Integer(Integer { value: 5 }));
                            pairs.insert(HashableObject::Integer(Integer { value: 9 }), Object::Integer(Integer { value: 30 }));
                            pairs
                        } 
                    }),
                ],
            },
            VirtualMachineTest {
                input: String::from("{1+1: 2*2, 3+3: 4*4}"),
                expected_stack: vec![
                    Object::HashObject(object_system::HashObject {
                        pairs: {
                            let mut pairs = HashMap::new();
                            pairs.insert(HashableObject::Integer(Integer { value: 2 }), Object::Integer(Integer { value: 4 }));
                            pairs.insert(HashableObject::Integer(Integer { value: 6 }), Object::Integer(Integer { value: 16 }));
                            pairs
                        } 
                    }),
                ],
            }
        ];
        run_vm_tests(inputs);
    }

    #[test]
    fn test_index_expressions() {
        let inputs = vec![
            VirtualMachineTest {
                input: String::from("[1,2,3][1]"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 2 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("[1,2,3][0+2]"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 3 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("[[1,1,1]][0][0]"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 1 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("[][0]"),
                expected_stack: vec![
                    Object::Null,
                ],
            },
            VirtualMachineTest {
                input: String::from("[1,2,3][99]"),
                expected_stack: vec![
                    Object::Null,
                ],
            },
            VirtualMachineTest {
                input: String::from("[1][-1]"),
                expected_stack: vec![
                    Object::Null,
                ],
            },
            VirtualMachineTest {
                input: String::from("{1:1, 2:2}[1]"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 1 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("{1:1, 2:2}[2]"),
                expected_stack: vec![
                    Object::Integer(Integer { value: 2 }),
                ],
            },
            VirtualMachineTest {
                input: String::from("{1:1}[0]"),
                expected_stack: vec![
                    Object::Null,
                ],
            },
            VirtualMachineTest {
                input: String::from("{}[0]"),
                expected_stack: vec![
                    Object::Null,
                ],
            },
        ];
        run_vm_tests(inputs);
    }
}
