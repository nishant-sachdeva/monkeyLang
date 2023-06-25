pub use crate::interpreter::{
    ast,
    tokens
};
use std::collections::HashMap;

use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::cell::RefCell;

pub mod object_system {
    use crate::interpreter::ast::Interface;

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Object {
        Integer(Integer),
        Boolean(Boolean),
        StringObject(StringObject),
        ReturnValue(ReturnValue),
        FunctionObject(FunctionObject),
        BuiltinFunctionObject(BuiltinFunctionObject),
        ArrayObject(ArrayObject),
        HashObject(HashObject),
        EvalError(EvalError),
        Null,
    }
    
    #[derive(Debug, Clone, PartialEq)]
    pub enum ObjectType {
        INTEGER,
        BOOLEAN,
        StringObject,
        FunctionObject,
        BuiltinFunctionObject,
        ArrayObject,
        HashObject,
        EvalError,
        NULL,
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
                Object::BuiltinFunctionObject(bfo) => bfo.log(),
                Object::ArrayObject(ao) => ao.log(),
                Object::HashObject(ho) => ho.log(),
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
                Object::BuiltinFunctionObject(bfo) => bfo.object_type(),
                Object::ArrayObject(ao) => ao.object_type(),
                Object::StringObject(so) => so.object_type(),
                Object::HashObject(ho) => ho.object_type(),
                Object::Null => ObjectType::NULL,            
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum HashableObject {
        Integer(Integer),
        Boolean(Boolean),
        StringObject(StringObject),
    }

    impl ObjectInterface for HashableObject {
        fn log(&self) -> String {
            match self {
                HashableObject::Integer(i) => i.log(),
                HashableObject::Boolean(b) => b.log(),
                HashableObject::StringObject(so) => so.log(),
            }
        }

        fn object_type(&self) -> ObjectType {
            match self {
                HashableObject::Integer(i) => i.object_type(),
                HashableObject::Boolean(b) => b.object_type(),
                HashableObject::StringObject(so) => so.object_type(),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct HashObject {
        pub pairs: HashMap<HashableObject, Object>,
    }

    impl Hash for HashObject {
        fn hash<H: Hasher>(&self, _state: &mut H) {
            self.pairs.hasher();
        }
    }

    impl ObjectInterface for HashObject {
        fn log(&self) -> String {
            format!("{{{}}}", self.pairs.iter().
                                map(|(k, v)| format!("{:?}: {:?}", k.log(), v.log()))
                                .collect::<Vec<String>>()
                                .join(", ")
            )
        }

        fn object_type(&self) -> ObjectType {
            ObjectType::HashObject
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct ArrayObject {
        pub elements: Vec<Object>,
    }

    impl ObjectInterface for ArrayObject {
        fn log(&self) -> String {
            format!("[{}]", self.elements.iter().map(|e| e.log()).collect::<Vec<String>>().join(", "))
        }

        fn object_type(&self) -> ObjectType {
            ObjectType::ArrayObject
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct BuiltinFunctionObject {
        pub func: fn(Vec<Object>) -> Object,
    }

    impl ObjectInterface for BuiltinFunctionObject {
        fn log(&self) -> String {
            format!("builtin function")
        }

        fn object_type(&self) -> ObjectType {
            ObjectType::BuiltinFunctionObject
        }
    }


    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        pub environ: Rc<RefCell<super::environment::Environment>>,
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
    
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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


pub mod built_in_functions {
    /// defining the builtin functions
    use super::object_system::{*, self};

    /// returns the builtin function if it exists
    /// 
    /// # Arguments
    /// 
    /// * `name` - the name of the builtin function
    /// 
    /// # Returns
    /// 
    /// * `Option<Object>` - the builtin function if it exists
    /// 
    /// # Example
    /// 
    /// ```
    /// use monkey_lang::interpreter::evaluate::object_system::*;
    /// use monkey_lang::interpreter::evaluate::built_in_functions::*;
    /// 
    /// let builtin_function = get_builtin_function("len");
    /// assert!(builtin_function.is_some());
    /// ```
    pub fn get_builtin_function(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: len })),
            "type" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: r#type })),
            "first" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: first })),
            "last" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: last })),
            "rest" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: rest })),
            "push" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: push })),
            "puts" => Some(Object::BuiltinFunctionObject(BuiltinFunctionObject { func: puts })),
            _ => None,
        }
    }

    pub fn puts(object: Vec<object_system::Object>) -> object_system::Object {
        for o in object {
            println!("{}", o.log());
        }
        object_system::Object::Null
    }

    pub fn r#type(object: Vec<object_system::Object>) -> object_system::Object {
        if object.len() != 1 {
            return Object::EvalError(EvalError { message: format!("wrong number of arguments. got={}, want=1", object.len()) });
        }
        let object = object[0].clone();
        Object::StringObject(StringObject { value: format!("{:?}", object.object_type()) })

    }

    pub fn len(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::EvalError(EvalError { message: format!("wrong number of arguments. got={}, want=1", args.len()) });
        }

        match args[0].clone() {
            Object::StringObject(so) => Object::Integer(Integer { value: so.value.len() as i64 }),
            Object::ArrayObject(ao) => Object::Integer(Integer { value: ao.elements.len() as i64 }),
            _ => Object::EvalError(EvalError { message: format!("argument to `len` not supported, got {:?}", args[0].object_type()) }),
        }
    }

    pub fn first(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::EvalError(EvalError { message: format!("wrong number of arguments. got={}, want=1", args.len()) });
        }

        match args[0].clone() {
            Object::ArrayObject(ao) => {
                if ao.elements.len() > 0 {
                    ao.elements[0].clone()
                } else {
                    Object::Null
                }
            },
            Object::StringObject(so) => {
                if so.value.len() > 0 {
                    Object::StringObject(StringObject { value: so.value[0..1].to_string() })
                } else {
                    Object::Null
                }
            },
            _ => Object::EvalError(EvalError { message: format!("argument to `first` must be ARRAY or STRING, got {:?}", args[0].object_type()) }),
        }
    }

    pub fn last(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::EvalError(EvalError { message: format!("wrong number of arguments. got={}, want=1", args.len()) });
        }

        match args[0].clone() {
            Object::ArrayObject(ao) => {
                if ao.elements.len() > 0 {
                    ao.elements[ao.elements.len() - 1].clone()
                } else {
                    Object::Null
                }
            },
            Object::StringObject(so) => {
                if so.value.len() > 0 {
                    Object::StringObject(StringObject { value: so.value[so.value.len() - 1..so.value.len()].to_string() })
                } else {
                    Object::Null
                }
            },
            _ => Object::EvalError(EvalError { message: format!("argument to `last` must be ARRAY or STRING, got {:?}", args[0].object_type()) }),
        }
    }

    pub fn rest(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::EvalError(EvalError { message: format!("wrong number of arguments. got={}, want=1", args.len()) });
        }

        match args[0].clone() {
            Object::ArrayObject(ao) => {
                if ao.elements.len() > 0 {
                    let mut new_elements = Vec::new();
                    for i in 1..ao.elements.len() {
                        new_elements.push(ao.elements[i].clone());
                    }
                    Object::ArrayObject(ArrayObject { elements: new_elements })
                } else {
                    Object::Null
                }
            },
            Object::StringObject(so) => {
                if so.value.len() > 0 {
                    Object::StringObject(StringObject { value: so.value[1..so.value.len()].to_string() })
                } else {
                    Object::Null
                }
            },
            _ => Object::EvalError(EvalError { message: format!("argument to `rest` must be ARRAY or STRING, got {:?}", args[0].object_type()) }),
        }
    }

    pub fn push(args: Vec<Object>) -> Object {
        if args.len() != 2 {
            return Object::EvalError(EvalError { message: format!("wrong number of arguments. got={}, want=2", args.len()) });
        }

        match args[0].clone() {
            Object::ArrayObject(ao) => {
                let mut a1 = ao.clone();
                a1.elements.push(args[1].clone());
                Object::ArrayObject(a1)
            },
            Object::StringObject(so) => {
                let mut s1 = so.clone();
                let Object::StringObject(s2) = args[1].clone()
                else {
                    return Object::EvalError(EvalError { message: format!("argument to STRING `push` must be STRING, got {:?}", args[1].object_type()) });
                };
                s1.value.push_str(
                    &s2.value
                );
                Object::StringObject(s1)
            },
            _ => Object::EvalError(EvalError { message: format!("argument to `push` must be ARRAY or STRING, got {:?}", args[0].object_type()) }),
        }
    }
}


pub mod environment {
    use std::collections::HashMap;
    
    #[derive(Debug, Clone, PartialEq)]
    pub struct Environment {
        pub store: HashMap<String, super::object_system::Object>,
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
        pub fn new(outer: Option<Environment>) -> Environment {
            Environment {
                store: std::collections::HashMap::new(),
                outer: match outer {
                    Some(o) => Box::new(Some(o.clone())),
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
            let value = eval_expression(let_statement.value, env);
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
        },
        ast::Expression::BooleanLiteral(bool_literal) => {
            object_system::Object::Boolean(object_system::Boolean{
                value: bool_literal.value,
            })
        },
        ast::Expression::PrefixExpression(prefix_expression) => {
            let right = eval_expression(*prefix_expression.right, env);
            eval_prefix_expression(prefix_expression.operator, right)
        },
        ast::Expression::InfixExpression(infix_expression) => {
            let left = eval_expression(*infix_expression.left, env);
            let right = eval_expression(*infix_expression.right, env);
            eval_infix_expression(infix_expression.operator, left, right)
        },
        ast::Expression::IfExpression(if_expression) => {
            eval_if_expression(if_expression, env)
        },
        ast::Expression::Identifier(identifier) => {
            eval_identifier(identifier, env)
        },
        ast::Expression::FunctionLiteral(function_literal) => {
            object_system::Object::FunctionObject(object_system::FunctionObject {
                token: function_literal.token,
                parameters: function_literal.parameters.clone(),
                body: function_literal.body.clone(),
                environ: Rc::new(RefCell::new(env.clone())),
            })
        },
        ast::Expression::CallExpression(call_expression) => {
            let function = eval_expression(*call_expression.function, env);
            let function = match function {
                object_system::Object::FunctionObject(function_object) => {
                    eval_expression(ast::Expression::FunctionLiteral(ast::FunctionLiteral {
                        token: function_object.token,
                        parameters: function_object.parameters,
                        body: function_object.body,
                    }), {
                        function_object.environ.deref().borrow_mut().outer = Box::new(Some(env.clone()));
                        &mut function_object.environ.borrow().deref().clone()
                    })
                },
                _ => function,
            };
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
        },
        ast::Expression::StringLiteral(string_literal) => {
            object_system::Object::StringObject(object_system::StringObject {
                value: string_literal.value,
            })
        },
        ast::Expression::ArrayLiteral(array_literal) => {
            let elements = eval_expressions(array_literal.elements, env);
            if elements.len() == 1 && {
                if let object_system::Object::EvalError(_) = elements[0] {
                    true
                } else {
                    false
                }
            } {
                return elements[0].clone();
            }
            object_system::Object::ArrayObject(object_system::ArrayObject {
                elements: elements,
            })
        },
        ast::Expression::IndexExpression(index_expression) => {
            let left = eval_expression(*index_expression.left, env);
            if let object_system::Object::EvalError(_) = left {
                return left;
            }
            let index = eval_expression(*index_expression.index, env);
            if let object_system::Object::EvalError(_) = index {
                return index;
            }
            eval_index_expression(left, index)
        },
        ast::Expression::HashLiteral(hash_literal) => {
            eval_hash_literal(hash_literal, env)
        }
    }
}

fn eval_hash_literal(hash_literal: ast::HashLiteral, env: &mut environment::Environment) -> object_system::Object {
    let mut pairs = std::collections::HashMap::new();
    for (key_expression, value_expression) in hash_literal.pairs {
        let key = eval_expression(key_expression, env);
        if let object_system::Object::EvalError(_) = key {
            return key;
        }
        let value = eval_expression(value_expression, env);
        if let object_system::Object::EvalError(_) = value {
            return value;
        }
        let hash_key = match key.object_type() {
            object_system::ObjectType::INTEGER => {
                object_system::HashableObject::Integer(object_system::Integer {
                    value: {
                        if let object_system::Object::Integer(integer) = key {
                            integer.value
                        } else {
                            unreachable!()
                        }
                    },
                })
            }
            object_system::ObjectType::BOOLEAN => {
                object_system::HashableObject::Boolean(object_system::Boolean {
                    value: {
                        if let object_system::Object::Boolean(boolean) = key {
                            boolean.value
                        } else {
                            unreachable!()
                        }
                    },
                })
            }
            object_system::ObjectType::StringObject => {
                object_system::HashableObject::StringObject(object_system::StringObject {
                    value: {
                        if let object_system::Object::StringObject(string_object) = key {
                            string_object.value
                        } else {
                            unreachable!()
                        }
                    },
                })
            }
            _ => {
                return object_system::Object::EvalError(object_system::EvalError {
                    message: format!("unusable as hash key: {:?}", key.object_type()),
                });
            }
        };
        pairs.insert(hash_key, value);
    }
    object_system::Object::HashObject(object_system::HashObject {
        pairs: pairs,
    })
}

fn eval_index_expression(left: object_system::Object, index: object_system::Object) -> object_system::Object {
    match (left.clone(), index.clone()) {
        (object_system::Object::ArrayObject(array_object), object_system::Object::Integer(integer)) => {
            if integer.value < 0 || integer.value >= array_object.elements.len() as i64 {
                return object_system::Object::Null;
            }
            array_object.elements[integer.value as usize].clone()
        }
        (object_system::Object::StringObject(string_object), object_system::Object::Integer(integer)) => {
            if integer.value < 0 || integer.value >= string_object.value.len() as i64 {
                return object_system::Object::Null;
            }
            object_system::Object::StringObject(object_system::StringObject {
                value: string_object.value.chars().nth(integer.value as usize).unwrap().to_string(),
            })
        }
        (object_system::Object::HashObject(hash_object), key) => {
            // create Hashable Object out of key
            let object_key = match key {
                object_system::Object::Integer(integer) => {
                    object_system::HashableObject::Integer(object_system::Integer {
                        value: integer.value,
                    })
                }
                object_system::Object::Boolean(boolean) => {
                    object_system::HashableObject::Boolean(object_system::Boolean {
                        value: boolean.value,
                    })
                }
                object_system::Object::StringObject(string_object) => {
                    object_system::HashableObject::StringObject(object_system::StringObject {
                        value: string_object.value,
                    })
                }
                _ => {
                    return object_system::Object::EvalError(object_system::EvalError {
                        message: format!("unusable as hash key: {:?}", key.object_type()),
                    });
                }
            };

            if let Some(value) = hash_object.pairs.get(&object_key) {
                value.clone()
            } else {
                object_system::Object::Null
            }
        }
        _ => object_system::Object::EvalError(object_system::EvalError {
            message: format!("index operator not supported: {:?}[{:?}]", left.object_type(), index.object_type()),
        }),
    }
}

fn eval_identifier(identifier: ast::Identifier, env: &mut environment::Environment) -> object_system::Object {
    match env.get(identifier.value.as_str()) {
        Ok(v) => v.clone(),
        Err(e) => {
            // check if this is a builtin Function
            match built_in_functions::get_builtin_function(identifier.value.as_str()) {
                Some(f) => f,
                None => object_system::Object::EvalError(object_system::EvalError {
                    message: e,
                }),
            }
        }
    }
}

fn apply_function(function: object_system::Object, arguments: Vec<object_system::Object>) -> object_system::Object {
    match function {
        object_system::Object::FunctionObject(function_object) => {
            apply_function_object(function_object, arguments)
        }
        object_system::Object::BuiltinFunctionObject(built_in_function) => {
            apply_builtin_function(built_in_function, arguments)
        }
        _ => object_system::Object::EvalError(object_system::EvalError {
            message: format!("not a function: {:?}", function),
        }),
    }
}

fn apply_function_object(function_object: object_system::FunctionObject, arguments: Vec<object_system::Object>) -> object_system::Object {
    let mut extended_env = match extend_function_env(&function_object, arguments) {
        Ok(env) => env,
        Err(e) => return e,
    };
    let evaluated = eval_block_statement(function_object.body, &mut extended_env);
    unwrap_return_value(evaluated)
}

fn unwrap_return_value(evaluated: object_system::Object) -> object_system::Object {
    match evaluated {
        object_system::Object::ReturnValue(return_value) => {
            *return_value.value
        }
        _ => evaluated,
    }
}

fn apply_builtin_function(built_in_function: object_system::BuiltinFunctionObject, arguments: Vec<object_system::Object>) -> object_system::Object {
    (built_in_function.func)(arguments)
}

fn extend_function_env(function: &object_system::FunctionObject, arguments: Vec<object_system::Object>)
    -> Result<environment::Environment, object_system::Object> {
    let mut env = environment::Environment::new(
        Some(function.environ.borrow().deref().clone()),
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
        "/" => {
            if right_val.value == 0 {
                return object_system::Object::EvalError(
                    object_system::EvalError {
                        message: format!("division by zero"),
                    }
                );
            }
            object_system::Object::Integer(object_system::Integer {
                value: left_val.value / right_val.value,
            })
        },
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
    use crate::interpreter::evaluate::object_system::HashableObject;
    use crate::interpreter::{
        evaluate::*,
        ast::*
    };
    use std::collections::HashMap;

    fn test_run(input: &str) -> object_system::Object {
        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = parser::Parser::new(lexer);

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e.message),
        };

        let mut env = environment::Environment::new(None);

        let result = eval(program, &mut env);
        result
    }

    #[test]
    fn test_recursive_function_calls() {
        let input = "
        let countDown = fn(x) {
            if (x == 0) {
                return 0;
            } else {
                countDown(x - 1);
            }
        };
        countDown(1);
        ";

        let result = test_run(input);
        assert_eq!(result, object_system::Object::Integer(object_system::Integer { value: 0 }));
    }

    #[test]
    fn test_hash_index_expressions() {
        struct Test {
            input: String,
            expected: object_system::Object,
        }

        let inputs = vec![
            Test {
                input: "{\"foo\": 5}[\"foo\"]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 5 }),
            },
            Test {
                input: "{\"foo\": 5}[\"bar\"]".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "let key = \"foo\"; {\"foo\": 5}[key]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 5 }),
            },
            Test {
                input: "{}[\"foo\"]".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "{5: 5}[5]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 5 }),
            },
            Test {
                input: "{true: 5}[true]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 5 }),
            },
            Test {
                input: "{false: 5}[false]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 5 }),
            },
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result, input.expected);
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = "
            let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }";
        let result = test_run(input);

        let mut expected: HashMap<HashableObject, object_system::Object> = HashMap::new();

        expected.insert(
            object_system::HashableObject::StringObject(object_system::StringObject { value: "one".to_string() }),
            object_system::Object::Integer(object_system::Integer { value: 1 }),
        );
        expected.insert(
            object_system::HashableObject::StringObject(object_system::StringObject { value: "two".to_string() }),
            object_system::Object::Integer(object_system::Integer { value: 2 }),
        );
        expected.insert(
            object_system::HashableObject::StringObject(object_system::StringObject { value: "three".to_string() }),
            object_system::Object::Integer(object_system::Integer { value: 3 }),
        );
        expected.insert(
            object_system::HashableObject::Integer(object_system::Integer { value: 4 }),
            object_system::Object::Integer(object_system::Integer { value: 4 }),
        );
        expected.insert(
            object_system::HashableObject::Boolean(object_system::Boolean { value: true }),
            object_system::Object::Integer(object_system::Integer { value: 5 }),
        );
        expected.insert(
            object_system::HashableObject::Boolean(object_system::Boolean { value: false }),
            object_system::Object::Integer(object_system::Integer { value: 6 }),
        );

        assert_eq!(result.object_type(), object_system::ObjectType::HashObject);
        let result_hash = match result {
            object_system::Object::HashObject(hash) => hash,
            _ => panic!("result is not a hash object"),
        };

        assert_eq!(result_hash.pairs.len(), expected.len());

        for (key, value) in expected {
            let pair = result_hash.pairs.get(&key).unwrap();
            assert_eq!(*pair, value);
        }
    }

    #[test]
    fn test_array_indexing() {
        struct Test {
            input: String,
            expected: object_system::Object,
        }

        let inputs = vec![
            Test {
                input: "[1, 2, 3][0]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 1 }),
            },
            Test {
                input: "[1, 2, 3][1]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 2 }),
            },
            Test {
                input: "[1, 2, 3][2]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 3 }),
            },
            Test {
                input: "let i = 0; [1][i];".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 1 }),
            },
            Test {
                input: "[1, 2, 3][1 + 1];".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 3 }),
            },
            Test {
                input: "let myArray = [1, 2, 3]; myArray[2];".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 3 }),
            },
            Test {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 6 }),
            },
            Test {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]".to_string(),
                expected: object_system::Object::Integer(object_system::Integer { value: 2 }),
            },
            Test {
                input: "[1, 2, 3][3]".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "[1, 2, 3][-1]".to_string(),
                expected: object_system::Object::Null,
            },
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result, input.expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let result = test_run(input);

        let object_system::Object::ArrayObject(array) = result 
        else {
            panic!("object is not ArrayObject. got={:?}", result);
        };

        assert_eq!(array.elements.len(), 3);
        assert_eq!(array.elements[0], object_system::Object::Integer(object_system::Integer { value: 1 }));
        assert_eq!(array.elements[1], object_system::Object::Integer(object_system::Integer { value: 4 }));
        assert_eq!(array.elements[2], object_system::Object::Integer(object_system::Integer { value: 6 }));
    }

    #[test]
    fn test_builtin_functions() {
        struct Test {
            input: String,
            expected: object_system::Object,
        }

        let inputs = vec![
            Test {
                input: "len(\"\")".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 0,
                }),
            },
            Test {
                input: "len(\"four\")".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 4,
                }),
            },
            Test {
                input: "len(\"hello world\")".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 11,
                }),
            },
            Test {
                input: "len(1)".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "argument to `len` not supported, got INTEGER".to_string(),
                    }
                ),
            },
            Test {
                input: "len(\"one\", \"two\")".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "wrong number of arguments. got=2, want=1".to_string(),
                    }
                ),
            },
            Test {
                input: "type(1)".to_string(),
                expected: object_system::Object::StringObject(object_system::StringObject {
                    value: "INTEGER".to_string(),
                }),
            },
            Test {
                input: "type(\"hello\")".to_string(),
                expected: object_system::Object::StringObject(object_system::StringObject {
                    value: "StringObject".to_string(),
                }),
            },
            Test {
                input: "type(\"one\", \"two\")".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "wrong number of arguments. got=2, want=1".to_string(),
                    }
                ),
            },
            Test {
                input: "type(1, 2)".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "wrong number of arguments. got=2, want=1".to_string(),
                    }
                ),
            },
            Test {
                input: "len([1, 2, 3])".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 3,
                }),
            },
            Test {
                input: "len([])".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 0,
                }),
            },
            Test {
                input: "first([1, 2, 3])".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 1,
                }),
            },
            Test {
                input: "first([])".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "first(1)".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "argument to `first` must be ARRAY or STRING, got INTEGER".to_string(),
                    }
                ),
            },
            Test {
                input: "last([1, 2, 3])".to_string(),
                expected: object_system::Object::Integer(object_system::Integer {
                    value: 3,
                }),
            },
            Test {
                input: "last([])".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "last(1)".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "argument to `last` must be ARRAY or STRING, got INTEGER".to_string(),
                    }
                ),
            },
            Test {
                input: "rest([1, 2, 3])".to_string(),
                expected: object_system::Object::ArrayObject(object_system::ArrayObject {
                    elements: vec![
                        object_system::Object::Integer(object_system::Integer { value: 2 }),
                        object_system::Object::Integer(object_system::Integer { value: 3 }),
                    ],
                }),
            },
            Test {
                input: "rest([])".to_string(),
                expected: object_system::Object::Null,
            },
            Test {
                input: "push([], 1)".to_string(),
                expected: object_system::Object::ArrayObject(object_system::ArrayObject {
                    elements: vec![
                        object_system::Object::Integer(object_system::Integer { value: 1 }),
                    ],
                }),
            },
            Test {
                input: "push(1, 1)".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "argument to `push` must be ARRAY or STRING, got INTEGER".to_string(),
                    }
                ),
            },
            Test {
                input: "push([], 1, 2)".to_string(),
                expected: object_system::Object::EvalError(
                    object_system::EvalError {
                        message: "wrong number of arguments. got=3, want=2".to_string(),
                    }
                ),
            }
        ];

        for input in inputs {
            let result = test_run(&input.input);
            assert_eq!(result, input.expected);
        }
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
            Test {input: "len(1)".to_string(), expected: "argument to `len` not supported, got INTEGER".to_string()},
            Test {input: "len(\"one\", \"two\")".to_string(), expected: "wrong number of arguments. got=2, want=1".to_string()},
            Test {input: "first(\"one\", \"two\")".to_string(), expected: "wrong number of arguments. got=2, want=1".to_string()},
            Test {input: "last(\"one\", \"two\")".to_string(), expected: "wrong number of arguments. got=2, want=1".to_string()},
            Test {input: "rest(\"one\", \"two\")".to_string(), expected: "wrong number of arguments. got=2, want=1".to_string()},
            Test {input: "{\"name\" : \"Monkey\"}[fn(x) {x}];".to_string(), expected: "unusable as hash key: FunctionObject".to_string()},
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
