pub mod config;
pub mod interpreter;
pub mod compiler;
pub mod virtual_machine;

use interpreter::{
    evaluate::{
        object_system,
        environment,
        eval
    },
    parser,
    lexer,
};

use compiler::compiler::Compiler;
use virtual_machine::virtual_machine::VirtualMachine;

use std::fs;

use crate::interpreter::evaluate::object_system::ObjectInterface;

fn run_code(input: &str, env: Option<&mut environment::Environment>) -> object_system::Object {
    let lexer = lexer::Lexer::new(input.to_string());
    let mut parser = parser::Parser::new(lexer);

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            return object_system::Object::EvalError(
                object_system::EvalError{
                    message: e.message
                }
            )
        }
    };

    let result = match env {
        Some(env) => eval(program, env),
        None => {
            let mut env = environment::Environment::new(None);
            eval(program, &mut env)
        }
    };
    result
}

pub fn compile_and_run(input: &str) -> object_system::Object {
    let lexer = lexer::Lexer::new(input.to_string());
    let mut parser = parser::Parser::new(lexer);

    let mut program = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            return object_system::Object::EvalError(
                object_system::EvalError{
                    message: e.message
                }
            )
        }
    };

    let mut compiler = Compiler::new();
    match compiler.compile(&mut program) {
        Ok(_) => (),
        Err(e) => {
            return object_system::Object::EvalError(
                object_system::EvalError{
                    message: e
                }
            )
        }
    }

    let bytecode = match compiler.raw_assembly() {
        Ok(bytecode) => bytecode,
        Err(e) => {
            return object_system::Object::EvalError(
                object_system::EvalError{
                    message: e
                }
            )
        }
    };

    let mut vm = VirtualMachine::new(bytecode);

    match vm.run() {
        Ok(_) => (),
        Err(e) => {
            return object_system::Object::EvalError(
                object_system::EvalError{
                    message: e
                }
            )
        }
    }

    let result = match vm.stack.last_popped_stack_element() {
        Ok(object) => object,
        Err(e) => object_system::Object::EvalError(
            object_system::EvalError{
                message: e
            }
        ),
    };

    result
}

pub fn start_compiler_repl() {
    loop {
        // read input
        println!(">>");
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {},
            Err(e) => println!("Error reading input: {}", e),
        }

        // if input is "quit", break
        if input.trim() == "quit" {
            break;
        }

        // run code
        let result = compile_and_run(&input);

        // print result
        match result {
            object_system::Object::Integer(i) => println!("{}", i.log()),
            object_system::Object::Boolean(b) => println!("{}", b.log()),
            object_system::Object::EvalError(e) => println!("{}", e.log()),
            object_system::Object::ReturnValue(r) => println!("{}", r.log()),
            object_system::Object::StringObject(s) => println!("{}", s.log()),
            object_system::Object::ArrayObject(a) => println!("{}", a.log()),
            _ => {}
        }
    }
}

pub fn start_repl() {
    let mut env = environment::Environment::new(None);
    loop {
        // read input
        println!(">>");
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {},
            Err(e) => println!("Error reading input: {}", e),
        }

        // if input is "quit", break
        if input.trim() == "quit" {
            break;
        }

        // run code
        let result = run_code(&input, Some(&mut env));

        // print result
        match result {
            object_system::Object::Integer(i) => println!("{}", i.log()),
            object_system::Object::Boolean(b) => println!("{}", b.log()),
            object_system::Object::EvalError(e) => println!("{}", e.log()),
            object_system::Object::ReturnValue(r) => println!("{}", r.log()),
            object_system::Object::StringObject(s) => println!("{}", s.log()),
            object_system::Object::ArrayObject(a) => println!("{}", a.log()),
            _ => {}
        }
    }
}

pub fn start_interpreter(config: &config::Config) {
    match run_interpreter(&config) {
        object_system::Object::Integer(i) => println!("{:?}", i),
        object_system::Object::Boolean(b) => println!("{:?}", b),
        object_system::Object::Null => println!("null"),
        object_system::Object::EvalError(e) => println!("{:?}", e),
        object_system::Object::ReturnValue(r) => println!("{:?}", r),
        object_system::Object::FunctionObject(f) => println!("{:?}", f),
        object_system::Object::StringObject(s) => println!("{:?}", s),
        object_system::Object::ArrayObject(a) => println!("{:?}", a),
        object_system::Object::HashObject(h) => println!("{:?}", h),
        object_system::Object::BuiltinFunctionObject(b) => println!("{:?}", b),
    }
}

fn run_interpreter(config: &config::Config) -> object_system::Object {
    // read code from config file
    let code = fs::read_to_string(*config.file_name.clone())
        .expect("Something went wrong reading the file");

    // run code
    let result = run_code(&code, None);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpreter_run() {
        struct Test {
            input: String,
            expected: object_system::Object,
        }

        let inputs = vec![
            Test {
                input: "let a = 5;".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "let a = 5; a;".to_string(),
                expected: object_system::Object::Integer(
                    object_system::Integer { value: 5 }
                ),
            },
            Test {
                input: "let a = 5 * 5; a;".to_string(),
                expected: object_system::Object::Integer(
                    object_system::Integer { value: 25 }
                ),
            },
            Test {
                input: "let a = 5; let b = a; b;".to_string(),
                expected: object_system::Object::Integer(
                    object_system::Integer { value: 5 }
                ),
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;".to_string(),
                expected: object_system::Object::Integer(
                    object_system::Integer { value: 15 }
                ),
            },
        ];

        for test in inputs {
            let result = run_code(&test.input, None);
            assert_eq!(result, test.expected);
        }
    }
}
