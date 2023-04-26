/// Defines the types of tokens used in the Monkey programming language.
#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum TokenType {
    ILLEGAL, EOF,
    IDENT, INT,
    ASSIGN, PLUS, MINUS, BANG, ASTERISK, SLASH, LT, GT, EQ, NotEq,
    COMMA, SEMICOLON,
    LPAREN, RPAREN, LBRACE, RBRACE,
    FUNCTION, LET, TRUE, FALSE, IF, ELSE, RETURN
}

/// Represents a token in the Monkey programming language.
///
/// # Examples
///
/// ```
/// use monkey_lang::interpreter::tokens::{Token, TokenType};
///
/// let token = Token::new(TokenType::IDENT, "foobar".to_string());
///
/// assert_eq!(token.token_type, TokenType::IDENT);
/// assert_eq!(token.literal, "foobar");
/// ```
///
/// ```
/// use monkey_lang::interpreter::tokens::{Token, TokenType};
///
/// let token = Token::new(TokenType::INT, "5".to_string());
///
/// assert_eq!(token.token_type, TokenType::INT);
/// assert_eq!(token.literal, "5");
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The type of token.
    pub token_type: TokenType,
    /// The literal value of the token.
    pub literal: String,
}

impl Token {
    /// Creates a new token with the given type and literal value.
    ///
    /// # Examples
    ///
    /// ```
    /// use monkey_lang::interpreter::tokens::{Token, TokenType};
    ///
    /// let token = Token::new(TokenType::IDENT, "foobar".to_string());
    ///
    /// assert_eq!(token.token_type, TokenType::IDENT);
    /// assert_eq!(token.literal, "foobar");
    /// ```
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}
