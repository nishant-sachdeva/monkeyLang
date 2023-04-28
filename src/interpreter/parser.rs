use std::collections::HashMap;
use crate::interpreter::{ast, lexer, tokens};

use super::ast::BlockStatement;

/// `Parser` is a struct that represents a parser for a programming language.
/// ```
/// use monkey_lang::interpreter::lexer::Lexer;
/// use monkey_lang::interpreter::parser::Parser;
/// 
/// let input = "1 + 2".to_string();
/// let lexer = Lexer::new(input);
/// let mut parser = Parser::new(lexer);
/// assert_eq!(parser.cur_token.literal, "1");
/// assert_eq!(parser.peek_token.literal, "+");
/// parser.next_token();
/// assert_eq!(parser.cur_token.literal, "+");
/// assert_eq!(parser.peek_token.literal, "2");
/// 
/// ```
pub struct Parser {
    lex: lexer::Lexer, // Lexer used by the parser
    pub cur_token: tokens::Token, // Current token being parsed
    pub peek_token: tokens::Token, // Next token to be parsed
    errors: Vec<String>, // List of error messages encountered during parsing

    // HashMaps containing functions for parsing different types of tokens
    prefix_parse_functions: HashMap<tokens::TokenType, fn(&mut Parser) -> ast::Expression>,
    infix_parse_functions: HashMap<tokens::TokenType, fn(&mut Parser, &ast::Expression) -> ast::Expression>,
}


/// `Parser` implementation

impl Parser {
    /// `new` creates a new `Parser` struct
    /// ```
    /// use monkey_lang::interpreter::lexer::Lexer;
    /// use monkey_lang::interpreter::parser::Parser;
    /// 
    /// let input = "1 + 2".to_string();
    /// let lexer = Lexer::new(input);
    /// let mut parser = Parser::new(lexer);
    /// assert_eq!(parser.cur_token.literal, "1");
    /// assert_eq!(parser.peek_token.literal, "+");
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.literal, "+");
    /// assert_eq!(parser.peek_token.literal, "2");
    ///
    /// ```
    /// 
    /// ```
    /// use monkey_lang::interpreter::lexer::Lexer;
    /// use monkey_lang::interpreter::parser::Parser;
    /// 
    /// let input = "let x = 5;".to_string();
    /// let lexer = Lexer::new(input);
    /// let mut parser = Parser::new(lexer);
    /// assert_eq!(parser.cur_token.literal, "let");
    /// assert_eq!(parser.peek_token.literal, "x");
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.literal, "x");
    /// assert_eq!(parser.peek_token.literal, "=");
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.literal, "=");
    /// assert_eq!(parser.peek_token.literal, "5");
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.literal, "5");
    /// assert_eq!(parser.peek_token.literal, ";");
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.literal, ";");
    /// assert_eq!(parser.peek_token.literal, "");
    /// parser.next_token();
    /// assert_eq!(parser.cur_token.literal, "");
    /// assert_eq!(parser.peek_token.literal, "");
    /// 
    /// ```
    pub fn new(lex: lexer::Lexer) -> Parser {
        let mut parser = Parser {
            lex,
            cur_token: tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
            peek_token: tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
            errors: Vec::new(),
            prefix_parse_functions: HashMap::new(),
            infix_parse_functions: HashMap::new(),
        };

        parser.initialize_prefix_functions();
        parser.initialize_infix_functions();

        parser.next_token();
        parser.next_token();

        parser
    }

    fn initialize_prefix_functions(&mut self) {
        self.register_prefix_function(tokens::TokenType::IDENT, Parser::parse_identifier);
        self.register_prefix_function(tokens::TokenType::INT, Parser::parse_integer);
        self.register_prefix_function(tokens::TokenType::BANG, Parser::parse_prefix_expression);
        self.register_prefix_function(tokens::TokenType::MINUS, Parser::parse_prefix_expression);
        self.register_prefix_function(tokens::TokenType::TRUE, Parser::parse_boolean);
        self.register_prefix_function(tokens::TokenType::FALSE, Parser::parse_boolean);
        self.register_prefix_function(tokens::TokenType::LPAREN, Parser::parse_grouped_expression);
        self.register_prefix_function(tokens::TokenType::IF, Parser::parse_if_expression);
        self.register_prefix_function(tokens::TokenType::FUNCTION, Parser::parse_function_literal);
    }

    fn register_prefix_function(&mut self, t: tokens::TokenType, f: fn(&mut Parser) -> ast::Expression) {
        self.prefix_parse_functions.insert(t, f);
    }

    fn initialize_infix_functions(&mut self) {
        self.register_infix_function(tokens::TokenType::PLUS, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::MINUS, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::SLASH, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::ASTERISK, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::EQ, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::NotEq, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::LT, Parser::parse_infix_expression);
        self.register_infix_function(tokens::TokenType::GT, Parser::parse_infix_expression);
    }

    fn register_infix_function(&mut self, t: tokens::TokenType, f: fn(&mut Parser, &ast::Expression) -> ast::Expression) {
        self.infix_parse_functions.insert(t, f);
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, String> {
        let mut program = ast::Program::new();

        while self.cur_token.token_type != tokens::TokenType::EOF {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(e) => {
                    let error = format!("Error parsing statement: {}", e);
                    self.errors.push(error.clone());
                    return Err(error);
                }
            }
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, String> {
        match self.cur_token.token_type {
            tokens::TokenType::LET => Ok(
                ast::Statement::LetStatement(
                    match self.parse_let_statement() {
                        Ok(stmt) => stmt,
                        Err(e) => {
                            return Err(
                                format!("Error parsing let statement: {}", e)
                            );
                        }
                    }
                )),
            tokens::TokenType::RETURN => Ok(
                ast::Statement::ReturnStatement(
                    match self.parse_return_statement() {
                        Ok(stmt) => stmt,
                        Err(e) => {
                            return Err(
                                format!("Error parsing return statement: {}", e)
                            );
                        }
                    }
                )),
            _ => Ok(
                ast::Statement::ExpressionStatement(
                    match self.parse_expression_statement() {
                        Ok(stmt) => stmt,
                        Err(e) => {
                            return Err(
                                format!("Error parsing expression statement: {}", e)
                            );
                        }
                    }
                )),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatement, String> {
        let stmt = ast::ExpressionStatement {
            token: self.cur_token.clone(),
            expression: match self.parse_expression(ast::Precedence::LOWEST) {
                Ok(exp) => exp,
                Err(e) => {
                    return Err(
                        format!("Error parsing expression: {}", e)
                    );
                }
            }
        };

        if self.peek_token_is(tokens::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self, precedence: ast::Precedence) -> Result<ast::Expression, String> {
        let prefix = match self.prefix_parse_functions.get(&self.cur_token.token_type) {
            Some(f) => f,
            None => {
                let error = format!("No prefix parse function for {:?} found", self.cur_token.token_type);
                return Err(error);
            }
        };

        let mut left_exp = prefix(self);

        while !self.peek_token_is(tokens::TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            self.next_token();

            let _infix = match self.infix_parse_functions.get(&self.cur_token.token_type) {
                Some(f) => f,
                None => {
                    return Ok(left_exp);
                }
            };

            left_exp = _infix(self, &left_exp);
        }

        return Ok(left_exp);
    }

    fn parse_grouped_expression(&mut self) -> ast::Expression {
        self.next_token();

        let exp = match self.parse_expression(ast::Precedence::LOWEST) {
            Ok(exp) => exp,
            Err(e) => {
                panic!("Error parsing grouped expression: {}", e);
            }
        };

        match self.assert_peek(tokens::TokenType::RPAREN) {
            Ok(_) => {},
            Err(e) => {
                panic!("Error parsing grouped expression: {}", e);
            }
        }

        return exp;
    }

    fn parse_function_literal(&mut self) -> ast::Expression {
        let mut expression = ast::FunctionLiteral {
            token: self.cur_token.clone(),
            parameters: vec![],
            body: ast::BlockStatement {
                token: tokens::Token {
                    token_type: tokens::TokenType::LBRACE,
                    literal: "{".to_string(),
                },
                statements: vec![],
            },
        };
        
        match self.assert_peek(tokens::TokenType::LPAREN) {
            Ok(_) => (),
            Err(e) => {
                panic!("Error parsing function literal: {}", e);
            }
        }

        expression.parameters = match self.parse_function_parameters() {
            Ok(params) => params,
            Err(e) => {
                panic!("Error parsing function literal: {}", e);
            }
        };

        match self.assert_peek(tokens::TokenType::LBRACE) {
            Ok(_) => (),
            Err(e) => {
                panic!("Error parsing function literal: {}", e);
            }
        }

        expression.body = self.parse_block_statement();

        return ast::Expression::FunctionLiteral(expression);

    }

    fn parse_function_parameters(&mut self) -> Result<Vec<ast::Identifier>, String> {
        let mut identifiers = vec![];

        loop {
            match self.assert_peek(tokens::TokenType::IDENT) {
                Ok(_) => (),
                Err(_) => {
                    match self.assert_peek(tokens::TokenType::RPAREN) {
                        Ok(_) => {
                            break;
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
            }

            let ident = ast::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };

            identifiers.push(ident);

            match self.assert_peek(tokens::TokenType::COMMA) {
                Ok(_) => {},
                Err(_) => {
                    match self.assert_peek(tokens::TokenType::RPAREN) {
                        Ok(_) => {
                            break;
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
            }

        }

        return Ok(identifiers);

    }

    fn parse_if_expression(&mut self) -> ast::Expression {
        let mut expression = ast::IfExpression {
            token: tokens::Token {
                token_type: tokens::TokenType::IF,
                literal: "if".to_string(),
            },
            condition: Box::new(ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token {
                    token_type: tokens::TokenType::IDENT,
                    literal: "".to_string(),
                },
                value: "".to_string(),
            })),
            consequence: BlockStatement {
                token: tokens::Token {
                    token_type: tokens::TokenType::LBRACE,
                    literal: "{".to_string(),
                },
                statements: vec![],
            },
            alternative: None,
        };

        match self.assert_peek(tokens::TokenType::LPAREN) {
            Ok(_) => {},
            Err(e) => {
                panic!("Error parsing if expression: {}", e);
            }
        }

        self.next_token();

        expression.condition = Box::new(
            match self.parse_expression(ast::Precedence::LOWEST) {
                Ok(exp) => exp,
                Err(e) => {
                    panic!("Error parsing if expression: {}", e);
                }
            }
        );

        match self.assert_peek(tokens::TokenType::RPAREN) {
            Ok(_) => {},
            Err(e) => {
                panic!("Error parsing if expression: {}", e);
            }
        }

        match self.assert_peek(tokens::TokenType::LBRACE) {
            Ok(_) => {},
            Err(e) => {
                panic!("Error parsing if expression: {}", e);
            }
        }

        expression.consequence = self.parse_block_statement();

        if self.peek_token_is(tokens::TokenType::ELSE) {
            self.next_token();

            match self.assert_peek(tokens::TokenType::LBRACE) {
                Ok(_) => {},
                Err(e) => {
                    panic!("Error parsing if expression: {}", e);
                }
            }

            expression.alternative = Some(self.parse_block_statement());
        }

        return ast::Expression::IfExpression(expression);
    
    }

    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let mut block = ast::BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.cur_token_is(tokens::TokenType::RBRACE) && !self.cur_token_is(tokens::TokenType::EOF) {
            match self.parse_statement() {
                Ok(stmt) => {
                    block.statements.push(stmt);
                },
                Err(e) => {
                    panic!("Error parsing block statement: {}", e);
                }
            }

            self.next_token();
        }

        return block;
    }

    fn parse_identifier(&mut self) -> ast::Expression {
        ast::Expression::Identifier(ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })
    }

    fn parse_integer(&mut self) -> ast::Expression {
        ast::Expression::IntegerLiteral(ast::IntegerLiteral {
            token: self.cur_token.clone(),
            value: match self.cur_token.literal.parse::<i64>() {
                Ok(v) => v,
                Err(_) => {
                    let error = format!("Could not parse {} as integer", self.cur_token.literal);
                    self.errors.push(error.clone());
                    panic!("{}", error);
                }
            }
        })
    }

    fn parse_boolean(&mut self) -> ast::Expression {
        ast::Expression::BooleanLiteral(ast::BooleanLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token_is(tokens::TokenType::TRUE),
        })
    }

    fn parse_prefix_expression(&mut self) -> ast::Expression {
        let mut expression = ast::PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: Box::new(ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            })),
        };

        self.next_token();

        expression.right = Box::new(match self.parse_expression(ast::Precedence::PREFIX) {
            Ok(exp) => exp,
            Err(_) => {
                return ast::Expression::Identifier(ast::Identifier {
                    token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                    value: "".to_string(),
                });
            }
        });

        ast::Expression::PrefixExpression(expression)
    }

    fn parse_infix_expression(&mut self, left: &ast::Expression) -> ast::Expression {
        // initialize expression
        let mut expression = ast::InfixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            left: Box::new(left.clone()),
            right: Box::new(ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            })),
        };

        // get precedence of current token
        let precedence = self.cur_precedence();

        // advance to next token
        self.next_token();

        // parse right expression
        expression.right = Box::new(match self.parse_expression(precedence) {
            Ok(exp) => exp,
            Err(_) => {
                return ast::Expression::Identifier(ast::Identifier {
                    token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                    value: "".to_string(),
                });
            }
        });

        ast::Expression::InfixExpression(expression)
    }

    fn parse_return_statement(&mut self) -> Result<ast::ReturnStatement, String> {
        let statement = ast::ReturnStatement {
            token: self.cur_token.clone(),
            return_value: ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            }),
        };

        self.next_token();

        // TODO :: parseExpression , followed by assert semicolon

        while !self.cur_token_is(tokens::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(statement)
    }
    
    fn parse_let_statement(&mut self) -> Result<ast::LetStatement, String> {
        let mut stmt = ast::LetStatement {
            token: self.cur_token.clone(),
            name: ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            },
            value: ast::Expression::Identifier(ast::Identifier {
                token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            }),
        };

        match self.assert_peek(tokens::TokenType::IDENT) {
            Ok(_) => {},
            Err(e) => {
                return Err(
                    format!("{}", e)
                );
            }
        }

        stmt.name = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        match self.assert_peek(tokens::TokenType::ASSIGN) {
            Ok(_) => {},
            Err(e) => {
                return Err(
                    format!("{}", e)
                );
            }
        }

        // TODO :: parseExpression , followed by assert semicolon

        while !self.cur_token_is(tokens::TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(stmt)
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = match self.lex.next_token() {
            Some(token) => token,
            None => tokens::Token::new(tokens::TokenType::EOF, "".to_string()),
        };
    }
    
    fn assert_peek(&mut self, t: tokens::TokenType) -> Result<(), String> {
        if self.peek_token_is(t) {
            self.next_token();
            return Ok(());
        } else {
            let error = format!("Expected next token to be {:?}, got {:?} instead", t, self.peek_token.token_type);
            self.errors.push(error.clone());
            return Err(error);
        }
    }

    fn cur_token_is(&self, t: tokens::TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: tokens::TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn peek_precedence(&self) -> ast::Precedence {
        return ast::Precedence::get_precedence(self.peek_token.token_type);
    }

    fn cur_precedence(&self) -> ast::Precedence {
        return ast::Precedence::get_precedence(self.cur_token.token_type);
    }
}

#[cfg(test)]
mod tests {

    use crate::interpreter::*;
    use super::*;

    #[test]
    fn test_function_parameters_parsing() {
        struct Test {
            input: String,
            expected_params: Vec<String>,
        }

        let inputs = vec![
            Test {
                input: "fn() {};".to_string(),
                expected_params: vec![],
            },
            Test {
                input: "fn(x) {};".to_string(),
                expected_params: vec!["x".to_string()],
            },
            Test {
                input: "fn(x, y, z) {};".to_string(),
                expected_params: vec!["x".to_string(), "y".to_string(), "z".to_string()],
            },
        ];

        for input in inputs {
            let lexer = lexer::Lexer::new(input.input);
            let mut parser = Parser::new(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(e) => {
                    panic!("{}", e);
                }
            };

            let stmt = &program.statements[0];

            match stmt {
                ast::Statement::ExpressionStatement(stmt) => {
                    match &stmt.expression {
                        ast::Expression::FunctionLiteral(exp) => {
                            assert_eq!(exp.parameters.len(), input.expected_params.len());

                            for (i, param) in exp.parameters.iter().enumerate() {
                                assert_eq!(param.value, input.expected_params[i]);
                            }
                        },
                        _ => {
                            panic!("Expected function literal");
                        }
                    }
                },
                _ => {
                    panic!("Expected expression statement");
                }
            }
        }
    }


    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => {
                panic!("{}", e);
            }
        };

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            ast::Statement::ExpressionStatement(stmt) => {
                match &stmt.expression {
                    ast::Expression::FunctionLiteral(exp) => {
                        assert_eq!(exp.parameters.len(), 2);
                        assert_eq!(exp.parameters[0].value, "x");
                        assert_eq!(exp.parameters[1].value, "y");

                        assert_eq!(exp.body.statements.len(), 1);

                        match &exp.body.statements[0] {
                            ast::Statement::ExpressionStatement(stmt) => {
                                match &stmt.expression {
                                    ast::Expression::InfixExpression(exp) => {
                                        assert_eq!(exp.operator, "+");
                                        assert_eq!(*exp.left, ast::Expression::Identifier(ast::Identifier {
                                            token: tokens::Token {
                                                token_type: tokens::TokenType::IDENT,
                                                literal: "x".to_string(),
                                            },
                                            value: "x".to_string(),
                                        }));
                                        assert_eq!(*exp.right, ast::Expression::Identifier(ast::Identifier {
                                            token: tokens::Token {
                                                token_type: tokens::TokenType::IDENT,
                                                literal: "y".to_string(),
                                            },
                                            value: "y".to_string(),
                                        }));
                                    },
                                    _ => {
                                        panic!("Expected InfixExpression, got {:?}", stmt.expression);
                                    }
                                }
                            },
                            _ => {
                                panic!("Expected ExpressionStatement, got {:?}", exp.body.statements[0]);
                            }
                        }
                    },
                    _ => {
                        panic!("Expected FunctionLiteral, got {:?}", stmt.expression);
                    }
                }
            },
            _ => {
                panic!("Expected ExpressionStatement, got {:?}", program.statements[0]);
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => {
                panic!("{}", e);
            }
        };

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            ast::Statement::ExpressionStatement(stmt) => {
                match &stmt.expression {
                    ast::Expression::IfExpression(exp) => {
                        assert_eq!(*exp.condition, ast::Expression::InfixExpression(ast::InfixExpression {
                            token: tokens::Token {
                                token_type: tokens::TokenType::LT,
                                literal: "<".to_string(),
                            },
                            operator: "<".to_string(),
                            left: Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: tokens::Token {
                                    token_type: tokens::TokenType::IDENT,
                                    literal: "x".to_string(),
                                },
                                value: "x".to_string(),
                            })),
                            right: Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: tokens::Token {
                                    token_type: tokens::TokenType::IDENT,
                                    literal: "y".to_string(),
                                },
                                value: "y".to_string(),
                            })),
                        }));

                        assert_eq!(exp.consequence.statements.len(), 1);

                        match &exp.consequence.statements[0] {
                            ast::Statement::ExpressionStatement(stmt) => {
                                match &stmt.expression {
                                    ast::Expression::Identifier(ident) => {
                                        assert_eq!(ident.value, "x");
                                    },
                                    _ => {
                                        panic!("Expected expression statement to be an identifier");
                                    }
                                }
                            },
                            _ => {
                                panic!("Expected consequence to be an expression statement");
                            }
                        }
                    },
                    _ => {
                        panic!("Expected expression statement to be an if expression");
                    }
                }
            },
            _ => {
                panic!("Expected statement to be an expression statement");
            }
        }
    }


    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => {
                panic!("{}", e);
            }
        };

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];

        match stmt {
            ast::Statement::ExpressionStatement(stmt) => {
                match &stmt.expression {
                    ast::Expression::IfExpression(exp) => {
                        assert_eq!(*exp.condition, ast::Expression::InfixExpression(ast::InfixExpression {
                            token: tokens::Token {
                                token_type: tokens::TokenType::LT,
                                literal: "<".to_string(),
                            },
                            operator: "<".to_string(),
                            left: Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: tokens::Token {
                                    token_type: tokens::TokenType::IDENT,
                                    literal: "x".to_string(),
                                },
                                value: "x".to_string(),
                            })),
                            right: Box::new(ast::Expression::Identifier(ast::Identifier {
                                token: tokens::Token {
                                    token_type: tokens::TokenType::IDENT,
                                    literal: "y".to_string(),
                                },
                                value: "y".to_string(),
                            })),
                        }));

                        assert_eq!(exp.consequence.statements.len(), 1);

                        match &exp.consequence.statements[0] {
                            ast::Statement::ExpressionStatement(stmt) => {
                                match &stmt.expression {
                                    ast::Expression::Identifier(ident) => {
                                        assert_eq!(ident.value, "x");
                                    },
                                    _ => {
                                        panic!("Expected expression statement to be an identifier");
                                    }
                                }
                            },
                            _ => {
                                panic!("Expected consequence to be an expression statement");
                            }
                        }

                        assert_eq!(exp.alternative.as_ref().unwrap().statements.len(), 1);

                        match &exp.alternative.as_ref().unwrap().statements[0] {
                            ast::Statement::ExpressionStatement(stmt) => {
                                match &stmt.expression {
                                    ast::Expression::Identifier(ident) => {
                                        assert_eq!(ident.value, "y");
                                    },
                                    _ => {
                                        panic!("Expected expression statement to be an identifier");
                                    }
                                }
                            },
                            _ => {
                                panic!("Expected consequence to be an expression statement");
                            }
                        }
                    },
                    _ => {
                        panic!("Expected expression statement to be an if expression");
                    }
                }
            },
            _ => {
                panic!("Expected statement to be an expression statement");
            }
        }
    }


    #[test]
    fn test_boolean_expression() {
        struct Test {
            input: String,
            expected: bool,
        }

        // creating an array of inputs
        let inputs = vec![
            Test {
                input: "true".to_string(),
                expected: true,
            },

            Test {
                input: "false".to_string(),
                expected: false,
            },
        ];

        for input in inputs {
            let lexer = lexer::Lexer::new(input.input);
            let mut parser = Parser::new(lexer);
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(e) => {
                    panic!("{}", e);
                }
            };

            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];

            match stmt {
                ast::Statement::ExpressionStatement(stmt) => {
                    match &stmt.expression {
                        ast::Expression::BooleanLiteral(exp) => {
                            assert_eq!(exp.value, input.expected);
                        },
                        _ => {
                            assert!(false);
                        }
                    }
                },
                _ => {
                    assert!(false);
                }
            }
        }
    }

    #[test]
    fn test_infix_expression_precedence() {
        struct Test {
            input: String,
            _expected: String,
        }

        // creating an array of inputs
        let inputs = vec![
            Test {
                input: "!-a".to_string(),
                _expected: "(!(-a))".to_string(),
            },

            Test {
                input: "a + b + c".to_string(),
                _expected: "((a + b) + c)".to_string(),
            },

            Test {
                input: "a + b - c".to_string(),
                _expected: "((a + b) - c)".to_string(),
            },

            Test {
                input: "a * b * c".to_string(),
                _expected: "((a * b) * c)".to_string(),
            },

            Test {
                input: "a * b / c".to_string(),
                _expected: "((a * b) / c)".to_string(),
            },

            Test {
                input: "a + b / c".to_string(),
                _expected: "(a + (b / c))".to_string(),
            },

            Test {
                input: "a + b * c + d / e - f".to_string(),
                _expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },

            Test {
                input: "3 + 4; -5 * 5".to_string(),
                _expected: "(3 + 4)((-5) * 5)".to_string(),
            },

            Test {
                input: "5 > 4 == 3 < 4".to_string(),
                _expected: "((5 > 4) == (3 < 4))".to_string(),
            },

            Test {
                input: "5 < 4 != 3 > 4".to_string(),
                _expected: "((5 < 4) != (3 > 4))".to_string(),
            },

            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                _expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },

            Test {
                input: "true".to_string(),
                _expected: "true".to_string(),
            },

            Test {
                input: "false".to_string(),
                _expected: "false".to_string(),
            },

            Test {
                input: "3 > 5 == false".to_string(),
                _expected: "((3 > 5) == false)".to_string(),
            },

            Test {
                input: "3 < 5 == true".to_string(),
                _expected: "((3 < 5) == true)".to_string(),
            },

            Test {
                input: "1 + (2 + 3) + 4".to_string(),
                _expected: "((1 + (2 + 3)) + 4)".to_string(),
            },

            Test {
                input: "(5 + 5) * 2".to_string(),
                _expected: "((5 + 5) * 2)".to_string(),
            },

            Test {
                input: "2 / (5 + 5)".to_string(),
                _expected: "(2 / (5 + 5))".to_string(),
            },

            Test {
                input: "-(5 + 5)".to_string(),
                _expected: "(-(5 + 5))".to_string(),
            },

            Test {
                input: "!(true == true)".to_string(),
                _expected: "(!(true == true))".to_string(),
            },

        ];

        for input in inputs {
            let l = crate::interpreter::lexer::Lexer::new(input.input);
            let mut p = Parser::new(l);
            let _program = p.parse_program();

            assert!(p.errors.is_empty());

            // TODO :: complete this test case
            // we need a funtion to print out the parsed program

            // let actual = program.to_string();
            // assert_eq!(actual, input._expected);
        }
    }

    #[test]
    fn test_infix_expression() {
        struct Test {
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
        }

        // creating an array of inputs
        let inputs = vec![
            Test {
                input: "5 + 5;".to_string(),
                left_value: 5,
                operator: "+".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 - 5;".to_string(),
                left_value: 5,
                operator: "-".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 * 5;".to_string(),
                left_value: 5,
                operator: "*".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 / 5;".to_string(),
                left_value: 5,
                operator: "/".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 > 5;".to_string(),
                left_value: 5,
                operator: ">".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 < 5;".to_string(),
                left_value: 5,
                operator: "<".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 == 5;".to_string(),
                left_value: 5,
                operator: "==".to_string(),
                right_value: 5,
            },
            Test {
                input: "5 != 5;".to_string(),
                left_value: 5,
                operator: "!=".to_string(),
                right_value: 5,
            },
        ];

        for input in inputs {
            // send input for parsing
            let mut parser = Parser::new(lexer::Lexer::new(input.input));
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(e) => panic!("{}", e),
            };

            // check if the program has only one statement
            assert_eq!(program.statements.len(), 1);

            // check if the statement is an expression statement
            let stmt = match program.statements[0].clone() {
                ast::Statement::ExpressionStatement(stmt) => stmt,
                _ => panic!("program.statements[0] is not ast::Statement::ExpressionStatement"),
            };

            // check if the expression is an infix expression
            let exp = match stmt.expression {
                ast::Expression::InfixExpression(exp) => exp,
                _ => panic!("stmt.expression is not ast::Expression::InfixExpression"),
            };

            // check if the left value is an integer literal
            let left = match exp.left.as_ref() {
                ast::Expression::IntegerLiteral(left) => left,
                _ => panic!("exp.left is not ast::Expression::IntegerLiteral"),
            };

            // check if the right value is an integer literal
            let right = match exp.right.as_ref() {
                ast::Expression::IntegerLiteral(right) => right,
                _ => panic!("exp.right is not ast::Expression::IntegerLiteral"),
            };

            // check if the left value is correct
            assert_eq!(left.value, input.left_value);

            // check if the operator is correct
            assert_eq!(exp.operator, input.operator);

            // check if the right value is correct
            assert_eq!(right.value, input.right_value);
        }
    }

    #[test]
    fn test_prefix_expression() {
        struct Test {
            input: String,
            operator: String,
            value: i64,
        }

        let inputs = vec![
            Test {
                input: "!5;".to_string(),
                operator: "!".to_string(),
                value: 5,
            },
            Test {
                input: "-15;".to_string(),
                operator: "-".to_string(),
                value: 15,
            },
        ];

        for input in inputs {
            let mut parser = Parser::new(lexer::Lexer::new(input.input));
            let program = match parser.parse_program() {
                Ok(program) => program,
                Err(e) => panic!("{}", e),
            };

            assert_eq!(program.statements.len(), 1);
            let stmt = match program.statements[0].clone() {
                ast::Statement::ExpressionStatement(stmt) => stmt,
                _ => panic!("Expected ExpressionStatement, got {:?}", program.statements[0]),
            };

            let exp = match stmt.expression {
                ast::Expression::PrefixExpression(exp) => exp,
                _ => panic!("Expected PrefixExpression, got {:?}", stmt.expression),
            };

            let expression = match exp.right.as_ref() {
                ast::Expression::IntegerLiteral(literal) => literal,
                _ => panic!("Expected IntegerLiteral, got {:?}", exp.right),
            };

            assert_eq!(exp.operator, input.operator);
            assert_eq!(expression.value , input.value);

        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };

        assert_eq!(program.statements.len(), 1);
        let stmt = match program.statements[0].clone() {
            ast::Statement::ExpressionStatement(stmt) => stmt,
            _ => panic!("Expected ExpressionStatement, got {:?}", program.statements[0]),
        };

        let literal = match stmt.expression {
            ast::Expression::IntegerLiteral(literal) => literal,
            _ => panic!("Expected IntegerLiteral, got {:?}", stmt.expression),
        };

        assert_eq!(literal.value, 5);
        assert_eq!(literal.token.literal, "5");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };

        assert_eq!(program.statements.len(), 1);
        let stmt = match program.statements[0].clone() {
            ast::Statement::ExpressionStatement(stmt) => stmt,
            _ => panic!("Expected ExpressionStatement, got {:?}", program.statements[0]),
        };

        let ident = match stmt.expression {
            ast::Expression::Identifier(ident) => ident,
            _ => panic!("Expected Identifier, got {:?}", stmt.expression),
        };

        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token.literal, "foobar");
    }

    #[test]
    #[should_panic(expected = "Expected next token to be IDENT, got ASSIGN instead")]
    fn test_let_statement_missing_ident_negative() {
        let input = "let = 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let _program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };
    }

    #[test]
    #[should_panic(expected = "Expected next token to be ASSIGN")]
    fn test_let_statement_missing_assign_negative() {
        let input = "let var 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let _program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };
    }

    #[test]
    fn test_let_statement() {
        let input = "let five = 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };

        assert_eq!(program.statements.len(), 1);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::LetStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::LET);
                    assert_eq!(stmt.name.value, "five");
                    assert_eq!(stmt.name.token.token_type, tokens::TokenType::IDENT);

                    // confirm that the value is an illegal token
                    assert_eq!(stmt.value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
                        })
                    );
                }
                _ => panic!("Expected Let Statement"),
            }

        }

    }
    
    
    #[test]
    fn test_multiple_let_statements() {
        let input = "let five = 5; let ten = 10; let result = 45;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };
        // confirm that the program has 3 statements
        assert_eq!(program.statements.len(), 3);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::LetStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::LET);
                    assert_eq!(stmt.name.token.token_type, tokens::TokenType::IDENT);

                    // confirm that the value is Illegal Token
                    assert_eq!(stmt.value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
    
                        })
                    );
                }
                _ => panic!("Expected Let Statement"),
            }

        }

    }


    #[test]
    fn test_return_statement() {
        let input = "return 5;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };

        // confirm that the program has 1 statement
        assert_eq!(program.statements.len(), 1);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::ReturnStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::RETURN);
                    assert_eq!(stmt.return_value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
                        })
                    );
                }
                _ => panic!("Expected Return Statement"),
            }

        }
    }

    
    #[test]
    // #[should_panic(expected = "Expected next token to be EXPRESSION, got SEMICOLON instead")]
    fn test_return_statement_negative() {
        let input = "return ;";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let _program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("{}", e),
        };
    }


    #[test]
    fn test_multiple_return_statements() {
        let input = "return 5; return 10; return add(5,10);";
        let mut parser = Parser::new(lexer::Lexer::new(input.to_string()));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => panic!("Error parsing program: {}", e),
        };
        // confirm that the program has 3 statements
        assert_eq!(program.statements.len(), 3);

        for statement in program.statements {
            // confirm that the statement is a let statement
            match statement {
                ast::Statement::ReturnStatement(stmt) => {
                    assert_eq!(stmt.token.token_type, tokens::TokenType::RETURN);
                    assert_eq!(stmt.return_value,
                        ast::Expression::Identifier(ast::Identifier {
                            token: tokens::Token::new(tokens::TokenType::ILLEGAL, "".to_string()),
                            value: "".to_string()
                        })
                    );
                }
                _ => panic!("Expected Return Statement"),
            }

        }

    }
   
}
