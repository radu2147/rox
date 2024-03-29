use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub line: u128,
    pub typ: TokenType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Grater,
    GraterEqual,
    Less,
    LessEqual,
    Identifier(String, String),
    String(String),
    Number(f32),
    And,
    Or,
    Class,
    If,
    Else,
    Fun,
    For,
    Nil,
    Print,
    Return,
    This(String),
    True,
    False,
    Var,
    While,
    Break,
    Eof,
}

impl Token {
    pub fn get_variable(&self) -> Variable {
        match &self.typ {
            TokenType::Identifier(name, id) => Variable {
                name: name.clone(),
                line: self.line,
                id: id.clone(),
            },
            TokenType::This(name) => Variable {
                name: name.clone(),
                line: self.line,
                id: "1".to_string(),
            },
            _ => panic!("Not a variable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
    pub line: u128,
    pub id: String,
}
