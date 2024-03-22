use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LeftParen(String, i32),
    RightParen(String, i32),
    LeftBrace(String, i32),
    RightBrace(String, i32),
    Comma(String, i32),
    Dot(String, i32),
    Minus(String, i32),
    Plus(String, i32),
    Semicolon(String, i32),
    Slash(String, i32),
    Star(String, i32),
    Bang(String, i32),
    BangEqual(String, i32),
    Equal(String, i32),
    EqualEqual(String, i32),
    Grater(String, i32),
    GraterEqual(String, i32),
    Less(String, i32),
    LessEqual(String, i32),
    Identifier(String, i32),
    String(String, String, i32),
    Number(String, f32, i32),
    And(String, i32),
    Or(String, i32),
    Class(String, i32),
    If(String, i32),
    Else(String, i32),
    Fun(String, i32),
    For(String, i32),
    Nil(String, i32),
    Print(String, i32),
    Return(String, i32),
    Super(String, i32),
    This(String, i32),
    True(String, i32),
    False(String, i32),
    Var(String, i32),
    While(String, i32),
    Eof(String, i32),
}

impl Token {
    pub fn get_variable(&self) -> Variable {
        match self {
            Token::Identifier(name, line) => Variable {
                name: name.clone(),
                line: line.clone(),
            },
            Token::This(name, line) => Variable {
                name: name.clone(),
                line: line.clone(),
            },
            _ => panic!("Not a variable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
    pub line: i32,
}
