/// Token types
#[derive(PartialEq, PartialOrd, Debug)]
pub enum TokenType {
    ILLEGAL, EOF,
    IDENT, INT,
    ASSIGN, PLUS, MINUS, BANG, ASTERISK, SLASH, LT, GT, EQ, NotEq,
    COMMA, SEMICOLON,
    LPAREN, RPAREN, LBRACE, RBRACE,
    FUNCTION, LET, TRUE, FALSE, IF, ELSE, RETURN
}

/// Token struct
/// Contains the type of the token and the literal value
/// of the token.
/// 
/// # Examples
/// 
/// ```
/// use monkey_lang::interpreter::tokens::{Token, TokenType};
/// let token = Token::new(TokenType::IDENT, "foobar".to_string());
/// 
/// assert_eq!(token.token_type, TokenType::IDENT);
/// assert_eq!(token.literal, "foobar");
/// ```
/// 
/// ```
/// use monkey_lang::interpreter::tokens::{Token, TokenType};
/// let token = Token::new(TokenType::INT, "5".to_string());
/// 
/// assert_eq!(token.token_type, TokenType::INT);
/// assert_eq!(token.literal, "5");
/// ```
#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type && self.literal == other.literal
    }
}
