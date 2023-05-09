pub mod config;
pub mod interpreter;

use interpreter::{
    evaluate::{
        object_system,
        environment,
        eval
    },
    parser,
    lexer,
};

use std::fs;

fn run_code(input: &str) -> object_system::Object {
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

    let mut env = environment::Environment::new(Box::new(None));

    let result = eval(program, &mut env);
    result
}

pub fn run_interpreter(config: &config::Config) -> object_system::Object {
    // read code from config file
    let code = fs::read_to_string(*config.file_name.clone())
        .expect("Something went wrong reading the file");

    // run code
    let result = run_code(&code);
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
            let result = run_code(&test.input);
            assert_eq!(result, test.expected);
        }
    }
}
