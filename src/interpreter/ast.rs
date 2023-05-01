use crate::interpreter::tokens::*;

/// Represents a node in the abstract syntax tree (AST) of the Monkey programming language.
/// 
/// # Examples
/// 
/// ```
/// use monkey_lang::interpreter::ast::{Program, Statement, LetStatement, Identifier, Expression, IntegerLiteral};
/// use monkey_lang::interpreter::tokens::{Token, TokenType};
/// 
/// let program = Program {
///    statements: vec![
///       Statement::LetStatement(LetStatement {
///         token: Token::new(TokenType::LET, "let".to_string()),
///        name: Identifier {
///          token: Token::new(TokenType::IDENT, "myVar".to_string()),
///         value: "myVar".to_string(),
///       },
///      value: Expression::Identifier(Identifier {
///       token: Token::new(TokenType::IDENT, "anotherVar".to_string()),
///      value: "anotherVar".to_string(),
///    }),
/// }),
/// Statement::LetStatement(LetStatement {
///  token: Token::new(TokenType::LET, "let".to_string()),
/// name: Identifier {
/// token: Token::new(TokenType::IDENT, "anotherVar".to_string()),
/// value: "anotherVar".to_string(),
/// },
/// value: Expression::IntegerLiteral(IntegerLiteral {
///     token: Token::new(TokenType::INT, "5".to_string()),
///    value: 5,
/// }),
/// }),
/// ],
/// };
/// ```
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

pub trait Interface {
    fn log(&self) -> String;
}

impl Interface for Program {
    // method to print the AST
    fn log(&self) -> String {
        let mut output = String::new();

        for statement in &self.statements {
            output.push_str(&statement.log());
        }
        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Interface for Statement {
    fn log(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.log(),
            Statement::ReturnStatement(return_statement) => return_statement.log(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.log(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Interface for LetStatement {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push_str(&self.token.literal);
        output.push_str(" ");
        output.push_str(&self.name.value);
        output.push_str(" = ");

        if let Expression::Identifier(identifier) = &self.value {
            output.push_str(&identifier.value);
        }

        output.push_str(";");

        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl Interface for ReturnStatement {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push_str(&self.token.literal);
        output.push_str(" ");

        if let Expression::Identifier(identifier) = &self.return_value {
            output.push_str(&identifier.value);
        }

        output.push_str(";");

        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Interface for ExpressionStatement {
    fn log(&self) -> String {
        self.expression.log()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Interface for Expression {
    fn log(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.log(),
            Expression::IntegerLiteral(integer_literal) => integer_literal.log(),
            Expression::BooleanLiteral(boolean_literal) => boolean_literal.log(),
            Expression::PrefixExpression(prefix_expression) => prefix_expression.log(),
            Expression::InfixExpression(infix_expression) => infix_expression.log(),
            Expression::IfExpression(if_expression) => if_expression.log(),
            Expression::FunctionLiteral(function_literal) => function_literal.log(),
            Expression::CallExpression(call_expression) => call_expression.log(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Interface for CallExpression {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push_str(&self.function.log());
        output.push_str("(");

        for (index, argument) in self.arguments.iter().enumerate() {
            output.push_str(&argument.log());

            if index < self.arguments.len() - 1 {
                output.push_str(", ");
            }
        }

        output.push_str(")");

        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Interface for FunctionLiteral {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push_str(&self.token.literal);
        output.push_str("(");

        for (index, parameter) in self.parameters.iter().enumerate() {
            output.push_str(&parameter.value);

            if index < self.parameters.len() - 1 {
                output.push_str(", ");
            }
        }

        output.push_str(") ");
        output.push_str(&self.body.log());

        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Interface for IfExpression {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push_str("if");
        output.push_str(&self.condition.log());
        output.push_str(" ");
        output.push_str(&self.consequence.log());

        if let Some(alternative) = &self.alternative {
            output.push_str("else ");
            output.push_str(&alternative.log());
        }

        output
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Interface for BlockStatement {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push_str("{");

        for statement in &self.statements {
            output.push_str(&statement.log());
        }

        output.push_str("}");

        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Interface for BooleanLiteral {
    fn log(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Interface for Identifier {
    fn log(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Interface for IntegerLiteral {
    fn log(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Interface for PrefixExpression {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push('(');
        output.push_str(&self.operator);
        output.push_str(&self.right.log());
        output.push(')');

        output
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Interface for InfixExpression {
    fn log(&self) -> String {
        let mut output = String::new();

        output.push('(');
        output.push_str(&self.left.log());
        output.push(' ');
        output.push_str(&self.operator);
        output.push(' ');
        output.push_str(&self.right.log());
        output.push(')');

        output
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Precedence {
    LOWEST,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunction(X)
}

// define precedence for each operator
impl Precedence {
    pub fn get_precedence(operator: TokenType) -> Precedence {
        match operator {
            TokenType::EQ => Precedence::EQUALS,
            TokenType::NotEq => Precedence::EQUALS,
            TokenType::LT => Precedence::LESSGREATER,
            TokenType::GT => Precedence::LESSGREATER,
            TokenType::PLUS => Precedence::SUM,
            TokenType::MINUS => Precedence::SUM,
            TokenType::SLASH => Precedence::PRODUCT,
            TokenType::ASTERISK => Precedence::PRODUCT,
            TokenType::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }
}
