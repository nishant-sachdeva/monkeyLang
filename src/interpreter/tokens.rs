#[derive(PartialEq, PartialOrd, Debug)]
pub enum TokenType {
    ILLEGAL, EOF,
    IDENT, INT,
    ASSIGN, PLUS, MINUS, BANG, ASTERISK, SLASH, LT, GT, EQ, NotEq,
    COMMA, SEMICOLON,
    LPAREN, RPAREN, LBRACE, RBRACE,
    FUNCTION, LET, TRUE, FALSE, IF, ELSE, RETURN
}

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
