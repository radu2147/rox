use crate::environment::Environment;
use crate::expression_visitor::Visitor;
use crate::types::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    CallExpression(Box<CallExpression>),
    Binary(Box<BinaryExpression>),
    Unary(Box<UnaryExpression>),
    BoolLiteral(bool),
    NumberLiteral(f32),
    StringLiteral(String),
    NilLiteral,
    Identifier(Identifier),
    Group(Box<GroupExpression>),
    Assignment(Box<AssignmentExpression>),
}

impl Expression {
    pub fn get_identifier_name(&self) -> String {
        match self {
            Self::Identifier(ident) => ident.name.clone(),
            _ => panic!("Shouldn't be reachable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub name: Token,
    pub asignee: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub raw_token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Expression,
    pub operator: Operator,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: Operator,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpression {
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Div,
    Mul,
    GE,
    LE,
    EQ,
    Greater,
    Less,
    Not,
    NotEqual,
    And,
    Or,
}

impl Operator {
    pub fn from(token: &Token) -> Self {
        match token {
            Token::Plus(_, _) => Operator::Plus,
            Token::Minus(_, _) => Operator::Minus,
            Token::Star(_, _) => Operator::Mul,
            Token::Slash(_, _) => Operator::Div,
            Token::GraterEqual(_, _) => Operator::GE,
            Token::LessEqual(_, _) => Operator::LE,
            Token::Less(_, _) => Operator::Less,
            Token::Grater(_, _) => Operator::Greater,
            Token::EqualEqual(_, _) => Operator::EQ,
            Token::BangEqual(_, _) => Operator::NotEqual,
            Token::Bang(_, _) => Operator::Not,
            Token::And(_, _) => Operator::And,
            Token::Or(_, _) => Operator::Or,
            _ => panic!("Unknown operator {:?}", token),
        }
    }
}

impl Expression {
    pub fn accept<T: Visitor<V, E>, V, E>(
        &mut self,
        visitor: &mut T,
        env: &mut Environment,
    ) -> Result<V, E> {
        visitor.visit_expression(self, env)
    }

    pub fn get_name(&self) -> String {
        match self {
            Self::Identifier(ref ident) => ident.name.clone(),
            _ => panic!("Not an identifier"),
        }
    }
}
