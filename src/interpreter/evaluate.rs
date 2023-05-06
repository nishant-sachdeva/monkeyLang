use std::ops::Deref;

pub use crate::interpreter::{
    ast,
    tokens
};

pub mod object_system {
    use crate::interpreter::ast::Interface;

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Object {
        Integer(Integer),
        Boolean(Boolean),
        StringObject(StringObject),
        ReturnValue(ReturnValue),
        EvalError(EvalError),
        FunctionObject(FunctionObject),
        Null,
    }
    
    #[derive(Debug, Clone, PartialEq)]
    pub enum ObjectType {
        INTEGER,
        BOOLEAN,
        EvalError,
        StringObject,
        NULL,
        FunctionObject
    }
    
    pub trait ObjectInterface {
        fn log(&self) -> String;
        fn object_type(&self) -> ObjectType;
    }

    impl ObjectInterface for Object {
        fn log(&self) -> String {
            match self {
                Object::Integer(i) => i.log(),
                Object::Boolean(b) => b.log(),
                Object::ReturnValue(rv) => rv.log(),
                Object::EvalError(eo) => eo.log(),
                Object::FunctionObject(fo) => fo.log(),
                Object::StringObject(so) => so.log(),
                Object::Null => "0".to_string(),            
            }
        }

        fn object_type(&self) -> ObjectType {
            match self {
                Object::Integer(i) => i.object_type(),
                Object::Boolean(b) => b.object_type(),
                Object::ReturnValue(rv) => rv.object_type(),
                Object::EvalError(eo) => eo.object_type(),
                Object::FunctionObject(fo) => fo.object_type(),
                Object::StringObject(so) => so.object_type(),
                Object::Null => ObjectType::NULL,            
            }
        }
    }


    #[derive(Debug, Clone, PartialEq)]
    pub struct StringObject {
        pub value: String,
    }

    impl ObjectInterface for StringObject {
        fn log(&self) -> String {
            format!("{}", self.value)
        }

        fn object_type(&self) -> ObjectType {
            ObjectType::StringObject
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct FunctionObject {
        pub token: tokens::Token,
        pub parameters: Vec<ast::Identifier>,
        pub body: ast::BlockStatement,
        pub env: std::rc::Rc<std::cell::RefCell<super::environment::Environment>>,
    }

    impl ObjectInterface for FunctionObject {
        fn log(&self) -> String {
            format!("fn({}) {{\n{}\n}}",
                self.parameters.iter().map(|p| p.log()).collect::<Vec<String>>().join(", "),
                self.body.statements.iter().map(|s| s.log()).collect::<Vec<String>>().join("\n"))
        }

        fn object_type(&self) -> ObjectType {
            ObjectType::FunctionObject
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct EvalError {
        pub message: String,
    }

    impl ObjectInterface for EvalError {
        fn log(&self) -> String {
            format!("{}", self.message)
        }

        fn object_type(&self) -> ObjectType {
            ObjectType::EvalError
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct ReturnValue {
        pub value: Box<Object>,
    }

    impl ObjectInterface for ReturnValue {
        fn log(&self) -> String {
            self.value.log()
        }

        fn object_type(&self) -> ObjectType {
            self.value.object_type()
        }
    }
    
    #[derive(Debug, Clone, PartialEq)]
    pub struct Integer {
        pub value: i64,
    }
    
    impl ObjectInterface for Integer {
        fn log(&self) -> String {
            format!("{}", self.value)
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::INTEGER
        }
    }
    
    #[derive(Debug, Clone, PartialEq)]
    pub struct Boolean {
        pub value: bool,
    }
    
    impl ObjectInterface for Boolean {
        fn log(&self) -> String {
            format!("{}", self.value)
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::BOOLEAN
        }
    }
    
    pub struct Null;
    
    impl ObjectInterface for Null {
        fn log(&self) -> String {
            format!("0")
        }
    
        fn object_type(&self) -> ObjectType {
            ObjectType::NULL
        }
    }

}

use object_system::ObjectInterface;


pub mod environment {
    
    #[derive(Debug, Clone, PartialEq)]
    pub struct Environment {
        pub store: std::collections::HashMap<String, super::object_system::Object>,
        pub outer: Box<Option<Environment>>,
    }

    /// Implementing the environment structure.
    impl Environment {
        /// Creates a new environment.
        ///
        /// # Arguments
        ///
        /// * `outer` - The outer environment that this environment is nested inside.
        ///
        /// # Returns
        ///
        /// A new environment.
        pub fn new(outer: Box<Option<Environment>>) -> Environment {
            Environment {
                store: std::collections::HashMap::new(),
                outer: match *outer {
                    Some(ref o) => Box::new(Some(o.clone())),
                    None => Box::new(None),
                }
            }
        }

        /// Gets the object associated with the given name from the environment.
        ///
        /// # Arguments
        ///
        /// * `name` - The name of the object to get.
        ///
        /// # Returns
        ///
        /// The object associated with the name, or an error message if the name is not found.    
        pub fn get(&self, name: &str) -> Result<&super::object_system::Object, String> {
            match self.store.get(name) {
                Some(v) => Ok(v),
                None => match *self.outer {
                    Some(ref o) => o.get(name),
                    None => Err(format!("Identifier not found: {}", name)),
                }
            }
        }

        /// Sets the value of the given name to the given object in the environment.
        ///
        /// # Arguments
        ///
        /// * `name` - The name to set.
        /// * `value` - The value to set the name to.
        ///
        /// # Returns
        ///
        /// The old object associated with the name, or Null if there was no old object.
        pub fn set(&mut self, name: &str, value: super::object_system::Object) -> Result <super::object_system::Object, String> {
            match self.store.insert(name.to_string(), value) {
                Some(v) => Ok(v),
                None => Ok(super::object_system::Object::Null),
            }
        }
    }
    
}

pub fn eval(program: ast::Program, env: &mut environment::Environment) -> object_system::Object {
    let mut result = object_system::Object::Null;
    for statement in program.statements {
        result = eval_statement(statement, env);
        if let object_system::Object::ReturnValue(r) = result {
            return *r.value;
        } else if let object_system::Object::EvalError(_) = result {
            return result;
        }
    }
    result
}

pub fn eval_block_statement(block: ast::BlockStatement, env: &mut environment::Environment) -> object_system::Object {
    let mut result = object_system::Object::Null;
    for statement in block.statements {
        result = eval_statement(statement, env);
        if let object_system::Object::ReturnValue(_) = result {
            return result;
        } else if let object_system::Object::EvalError(_) = result {
            return result;
        }
    }
    result
}

fn eval_statement(statement: ast::Statement, env: &mut environment::Environment) -> object_system::Object {
    match statement {
        ast::Statement::ExpressionStatement(expression_statement) => {
            eval_expression(expression_statement.expression, env)
        }
        ast::Statement::ReturnStatement(return_statement) => {
            object_system::Object::ReturnValue(object_system::ReturnValue {
                value: Box::new(
                    eval_expression(return_statement.return_value, env)),
            })
        }
        ast::Statement::LetStatement(let_statement) => {
            let value = eval_expression(let_statement.value.clone(), env);
            if let object_system::Object::EvalError(_) = value {
                return value;
            }
            match env.set(&let_statement.name.value, value) {
                Ok(_) => object_system::Object::Null,
                Err(e) => object_system::Object::EvalError(object_system::EvalError {
                    message: e,
                }),
            }
        }
    }
}

fn eval_expression(expression: ast::Expression, env: &mut environment::Environment) -> object_system::Object {
    match expression {
        ast::Expression::IntegerLiteral(integer_literal) => {
            object_system::Object::Integer(object_system::Integer {
                value: integer_literal.value,
            })
        }
        ast::Expression::BooleanLiteral(bool_literal) => {
            object_system::Object::Boolean(object_system::Boolean{
                value: bool_literal.value,
            })
        }
        ast::Expression::PrefixExpression(prefix_expression) => {
            let right = eval_expression(*prefix_expression.right, env);
            eval_prefix_expression(prefix_expression.operator, right)
        }
        ast::Expression::InfixExpression(infix_expression) => {
            let left = eval_expression(*infix_expression.left, env);
            let right = eval_expression(*infix_expression.right, env);
            eval_infix_expression(infix_expression.operator, left, right)
        }
        ast::Expression::IfExpression(if_expression) => {
            eval_if_expression(if_expression, env)
        }
        ast::Expression::Identifier(identifier) => {
            match env.get(identifier.value.as_str()) {
                Ok(v) => v.clone(),
                Err(e) => object_system::Object::EvalError(object_system::EvalError {
                    message: e,
                }),
            }
        }
        ast::Expression::FunctionLiteral(function_literal) => {
            object_system::Object::FunctionObject(object_system::FunctionObject {
                token: function_literal.token,
                parameters: function_literal.parameters,
                body: function_literal.body,
                env: std::rc::Rc::new(std::cell::RefCell::new(env.clone())),
            })
        }
        ast::Expression::CallExpression(call_expression) => {
            let function = eval_expression(*call_expression.function, env);
            if let object_system::Object::EvalError(_) = function {
                return function;
            }
            let arguments = eval_expressions(call_expression.arguments, env);
            if arguments.len() == 1 && {
                if let object_system::Object::EvalError(_) = arguments[0] {
                    true
                } else {
                    false
                }
            } {
                return arguments[0].clone();
            }

            return apply_function(function, arguments);
        }
        ast::Expression::StringLiteral(string_literal) => {
            object_system::Object::StringObject(object_system::StringObject {
                value: string_literal.value,
            })
        }
    }
}

fn apply_function(function: object_system::Object, arguments: Vec<object_system::Object>) -> object_system::Object {
    let object_system::Object::FunctionObject(function_object) = function else {
        return object_system::Object::EvalError(object_system::EvalError {
            message: format!("Not a function: {:?}", function),
        });
    };
    let mut extended_env = match extend_function_env(&function_object, arguments) {
        Ok(env) => env,
        Err(e) => return e,
    };
    let evaluated = eval_block_statement(function_object.body, &mut extended_env);
    if let object_system::Object::ReturnValue(r) = evaluated {
        return *r.value;
    } else {
        return evaluated;
    }
}


fn extend_function_env(function: &object_system::FunctionObject, arguments: Vec<object_system::Object>)
    -> Result<environment::Environment, object_system::Object> {
    let mut env = environment::Environment::new(
        Box::new(Some(function.env.borrow().deref().clone())),
    );
    for (i, parameter) in function.parameters.iter().enumerate() {
        match env.set(&parameter.value, arguments[i].clone()) {
            Ok(_) => {}
            Err(e) => {
                return Err(
                    object_system::Object::EvalError(object_system::EvalError {
                        message: e,
                    }
                ));
            }
        }
    }
    Ok(env)
}

fn eval_expressions(arguments: Vec<ast::Expression>, env: &mut environment::Environment) -> Vec<object_system::Object> {
    let mut result = Vec::new();
    for argument in arguments {
        let evaluated = eval_expression(argument, env);
        if let object_system::Object::EvalError(_) = evaluated {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn eval_if_expression(if_expression: ast::IfExpression, env: &mut environment::Environment) -> object_system::Object {
    let condition = eval_expression(*if_expression.condition,env);
    if match condition {
        object_system::Object::Boolean(b) => b.value,
        object_system::Object::Integer(_i) => true,
        _ => false,
    } {
        eval_block_statement(if_expression.consequence, env)
    } else if let Some(alternative) = if_expression.alternative {
        eval_block_statement(alternative, env)
    } else {
        object_system::Object::Null
    }
}

fn eval_infix_expression(operator: String, left: object_system::Object, right: object_system::Object) -> object_system::Object {

    if left.object_type() == object_system::ObjectType::INTEGER && right.object_type() == object_system::ObjectType::INTEGER {
        return eval_integer_infix_expression(operator, left, right);
    } else if left.object_type() == object_system::ObjectType::StringObject && right.object_type() == object_system::ObjectType::StringObject {
        return eval_string_infix_expression(operator, left, right);
    } else if left.object_type() == object_system::ObjectType::BOOLEAN && right.object_type() == object_system::ObjectType::BOOLEAN {
        return eval_boolean_infix_expression(operator, left, right);
    } else if left.object_type() != right.object_type() {
        return object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("type mismatch: {:?} {} {:?}", left.object_type(), operator, right.object_type()),
            }
        );
    }
    else {
        return object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("unknown operator: {:?} {} {:?}", left.object_type(), operator, right.object_type()),
            }
        );
    }
}

fn eval_string_infix_expression(operator: String, left: object_system::Object, right: object_system::Object) -> object_system::Object {
    let object_system::Object::StringObject(left_val) = left else { return object_system::Object::Null; };
    let object_system::Object::StringObject(right_val) = right else { return object_system::Object::Null; };

    match operator.as_str() {
        "+" => object_system::Object::StringObject(object_system::StringObject {
            value: left_val.value + &right_val.value,
        }),
        "==" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value == right_val.value,
        }),
        "!=" => object_system::Object::Boolean(
            object_system::Boolean {
                value: left_val.value != right_val.value,
            }
        ),
        ">" => object_system::Object::Boolean(
            object_system::Boolean {
                value: left_val.value > right_val.value,
            }
        ),
        "<" => object_system::Object::Boolean(
            object_system::Boolean {
                value: left_val.value < right_val.value,
            }
        ),
        _ => object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("unknown operator: {:?} {} {:?}", left_val.object_type(), operator, right_val.object_type()),
            }
        ),
    }    
}

fn eval_boolean_infix_expression(operator: String, left: object_system::Object, right: object_system::Object) -> object_system::Object {
    let object_system::Object::Boolean(left_val) = left else { return object_system::Object::Null; };
    let object_system::Object::Boolean(right_val) = right else { return object_system::Object::Null; };

    match operator.as_str() {
        "==" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value == right_val.value,
        }),
        "!=" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value != right_val.value,
        }),
        _ => object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("unknown operator: {:?} {} {:?}", left_val.object_type(), operator, right_val.object_type()),
            }
        )
    }
}

fn eval_integer_infix_expression(operator: String, left: object_system::Object, right: object_system::Object) -> object_system::Object {
    let object_system::Object::Integer(left_val) = left else { return object_system::Object::Null; };
    let object_system::Object::Integer(right_val) = right else { return object_system::Object::Null; };

    match operator.as_str() {
        "+" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value + right_val.value,
        }),
        "-" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value - right_val.value,
        }),
        "*" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value * right_val.value,
        }),
        "/" => object_system::Object::Integer(object_system::Integer {
            value: left_val.value / right_val.value,
        }),
        "<" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value < right_val.value,
        }),
        ">" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value > right_val.value,
        }),
        "==" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value == right_val.value,
        }),
        "!=" => object_system::Object::Boolean(object_system::Boolean {
            value: left_val.value != right_val.value,
        }),
        _ => object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("unknown operator: {:?} {} {:?}", left_val.object_type(), operator, right_val.object_type()),
            }
        )
    }
}

fn eval_prefix_expression(operator: String, right: object_system::Object) -> object_system::Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("unknown operator: {}{:?}", operator, right.object_type()),
            }
        )
    }
}

fn eval_minus_prefix_operator_expression(right: object_system::Object) -> object_system::Object { 
    match right {
        object_system::Object::Integer(integer) => {
            object_system::Object::Integer(object_system::Integer {
                value: -integer.value,
            })
        }
        _ => object_system::Object::EvalError(
            object_system::EvalError {
                message: format!("unknown operator: -{:?}", right.object_type()),
            }
        )
    }
}

fn eval_bang_operator_expression(right: object_system::Object) -> object_system::Object {
    match right {
        object_system::Object::Boolean(boolean) => {
            object_system::Object::Boolean(object_system::Boolean {
                value: !boolean.value,
            })
        }
        object_system::Object::Null => object_system::Object::Boolean(object_system::Boolean {
            value: true,
        }),
        _ => object_system::Object::Boolean(object_system::Boolean {
            value: false,
        }),
    }
}



#[cfg(test)]
mod tests {
    use crate::interpreter::*;
    use crate::interpreter::{
        evaluate::*,
        ast::*
    };

    fn test_run(input: &str) -> object_system::Object {
        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = parser::Parser::new(lexer);

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e.message),
        };

        let mut env = environment::Environment::new(Box::new(None));

        let result = eval(program, &mut env);
        result
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let result = test_run(input);

        let object_system::Object::StringObject(string) = result
        else {
            panic!("expected string")
        };

        assert_eq!(string.value, "Hello World!");
    }

    #[test]
    fn test_string_literals() {
        let input = "\"hello world\"";

        let result = test_run(input);
        let object_system::Object::StringObject(string) = result
        else {
            panic!("expected string")
        };
        assert_eq!(string.value, "hello world");
    }

    #[test]
    fn test_closures() {
        let input = "
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        
        let addTwo = newAdder(2);
        addTwo(2);
        ";

        let result = test_run(input);
        let object_system::Object::Integer(integer) = result
        else {
            panic!("expected integer")
        };
        assert_eq!(integer.value, 4);
    }

    #[test]
    fn test_function_evaluation() {
        pub struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "fn(x) { x + 2; }(2);".to_string(), expected: 4},
            Test {input: "let identity = fn(x) { x; }; identity(5);".to_string(), expected: 5},
            Test {input: "let double = fn(x) { x * 2; }; double(5);".to_string(), expected: 10},
            Test {input: "let add = fn(x, y) { x + y; }; add(5, 5);".to_string(), expected: 10},
            Test {input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".to_string(), expected: 20},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            let integer = match result {
                object_system::Object::Integer(integer) => integer,
                _ => panic!("expected integer"),
            };

            assert_eq!(integer.value, input.expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";
        let result = test_run(input);

        assert_eq!(result.object_type(), object_system::ObjectType::FunctionObject);
        assert_eq!(result.log(), "fn(x) {\n(x + 2)\n}");

        let function = match result {
            object_system::Object::FunctionObject(function) => function,
            _ => panic!("expected function object"),
        };

        assert_eq!(function.parameters.len(), 1);
        assert_eq!(function.parameters[0].log(), "x");
        assert_eq!(function.body.log(), "{(x + 2)}");
    }

    #[test]
    fn test_error_handling() {
        struct Test {
            input: String,
            expected: String,
        }

        let inputs = vec![
            Test {input: "5 + true;".to_string(), expected: "type mismatch: INTEGER + BOOLEAN".to_string()},
            Test {input: "5 + true; 5;".to_string(), expected: "type mismatch: INTEGER + BOOLEAN".to_string()},
            Test {input: "-true".to_string(), expected: "unknown operator: -BOOLEAN".to_string()},
            Test {input: "true + false;".to_string(), expected: "unknown operator: BOOLEAN + BOOLEAN".to_string()},
            Test {input: "5; true + false; 5".to_string(), expected: "unknown operator: BOOLEAN + BOOLEAN".to_string()},
            Test {input: "if (10 > 1) { true + false; }".to_string(), expected: "unknown operator: BOOLEAN + BOOLEAN".to_string()},
            Test {input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }".to_string(), expected: "unknown operator: BOOLEAN + BOOLEAN".to_string()},
            Test {input: "foobar".to_string(), expected: "Identifier not found: foobar".to_string()},
            Test {input: "\"Hello\" - \"World\"".to_string(), expected: "unknown operator: StringObject - StringObject".to_string()},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            match result {
                object_system::Object::EvalError(error) => assert_eq!(error.message, input.expected),
                _ => panic!("Got {} Expected {}. Result {}", result.log(), input.expected, result.log() == input.expected),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct Test {
            input: String,
            expected: i64,
        }

        let input = vec![
            Test {input: "let test_str = 5; test_str;".to_string(), expected: 5},
            Test {input: "let a = 5 * 5; a;".to_string(), expected: 25},
            Test {input: "let a = 5; let b = a; b;".to_string(), expected: 5},
            Test {input: "let a = 5; let b = a; let c = a + b + 5; c;".to_string(), expected: 15},
        ];

        for test in input {
            let result = test_run(&test.input);
            
            assert_eq!(result.object_type(), object_system::ObjectType::INTEGER);
            assert_eq!(result.log(), test.expected.to_string());
        }
    }


    #[test]
    fn test_return_statements() {
        struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "return 10;".to_string(), expected: 10},
            Test {input: "return 10; 9;".to_string(), expected: 10},
            Test {input: "return 2 * 5; 9;".to_string(), expected: 10},
            Test {input: "9; return 2 * 5; 9;".to_string(), expected: 10},
            Test {input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }".to_string(), expected: 10},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result.log(), input.expected.to_string());

        }
    }

    #[test]
    fn test_if_else_expressions() {
        struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "if (true) { 10 }".to_string(), expected: 10},
            Test {input: "if (false) { 10 }".to_string(), expected: 0},
            Test {input: "if (1) { 10 }".to_string(), expected: 10},
            Test {input: "if (1 < 2) { 10 }".to_string(), expected: 10},
            Test {input: "if (1 > 2) { 10 }".to_string(), expected: 0},
            Test {input: "if (1 > 2) { 10 } else { 20 }".to_string(), expected: 20},
            Test {input: "if (1 < 2) { 10 } else { 20 }".to_string(), expected: 10},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result.log(), input.expected.to_string());
        }
    }

    #[test]
    fn test_bang_operator() {
        struct Test {
            input: String,
            expected: bool,
        }

        let inputs = vec![
            Test {input: "!true".to_string(), expected: false},
            Test {input: "!false".to_string(), expected: true},
            Test {input: "!0".to_string(), expected: false},
            Test {input: "!5".to_string(), expected: false},
            Test {input: "!!true".to_string(), expected: true},
            Test {input: "!!false".to_string(), expected: false},
            Test {input: "!!5".to_string(), expected: true},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result.log(), input.expected.to_string());
        }
    }

    #[test]
    fn test_integer_eval() {
        struct Test {
            input: String,
            expected: i64,
        }

        let inputs = vec![
            Test {input: "5".to_string(), expected: 5},
            Test {input: "10".to_string(), expected: 10},
            Test {input: "-5".to_string(), expected: -5},
            Test {input: "-10".to_string(), expected: -10},
            Test {input: "5 + 5 + 5 + 5 - 10".to_string(), expected: 10},
            Test {input: "2 * 2 * 2 * 2 * 2".to_string(), expected: 32},
            Test {input: "-50 + 100 + -50".to_string(), expected: 0},
            Test {input: "5 * 2 + 10".to_string(), expected: 20},
            Test {input: "5 + 2 * 10".to_string(), expected: 25},
            Test {input: "20 + 2 * -10".to_string(), expected: 0},
            Test {input: "50 / 2 * 2 + 10".to_string(), expected: 60},
            Test {input: "2 * (5 + 10)".to_string(), expected: 30},
            Test {input: "3 * 3 * 3 + 10".to_string(), expected: 37},
            Test {input: "3 * (3 * 3) + 10".to_string(), expected: 37},
            Test {input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(), expected: 50},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result.log(), input.expected.to_string());
        }
    }

    #[test]
    fn test_boolean_eval() {
        struct Test {
            input: String,
            expected: bool,
        }

        let inputs = vec![
            Test {input: "true".to_string(), expected: true},
            Test {input: "false".to_string(), expected: false},
            Test {input: "1 < 2".to_string(), expected: true},
            Test {input: "1 > 2".to_string(), expected: false},
            Test {input: "1 < 1".to_string(), expected: false},
            Test {input: "1 > 1".to_string(), expected: false},
            Test {input: "1 == 1".to_string(), expected: true},
            Test {input: "1 != 1".to_string(), expected: false},
            Test {input: "1 == 2".to_string(), expected: false},
            Test {input: "1 != 2".to_string(), expected: true},
            Test {input: "true == true".to_string(), expected: true},
            Test {input: "false == false".to_string(), expected: true},
            Test {input: "true == false".to_string(), expected: false},
            Test {input: "true != false".to_string(), expected: true},
            Test {input: "false != true".to_string(), expected: true},
            Test {input: "(1 < 2) == true".to_string(), expected: true},
            Test {input: "(1 < 2) == false".to_string(), expected: false},
            Test {input: "(1 > 2) == true".to_string(), expected: false},
            Test {input: "(1 > 2) == false".to_string(), expected: true},
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result.log(), input.expected.to_string());
        }
    }
}
