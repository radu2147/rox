extern crate rox_errors;
pub mod token;
pub mod scanner;
pub mod expression;
pub mod expression_visitor;
pub mod statement;
pub mod statement_visitor;

use crate::expression::{
    AssignmentExpression, BinaryExpression, CallExpression, Expr, Expression, GroupExpression,
    Identifier, MemberExpression, ThisExpression, UnaryExpression,
};
use rox_errors::ParseError;
use crate::statement::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Statement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser {
    tokens_iter: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens_iter: tokens.into_iter().peekable(),
        }
    }

    pub fn consume_token(
        &mut self,
        token_type: TokenType,
        error_message: &str,
    ) -> Result<u128, ParseError> {
        if self.peek().typ == token_type {
            return Ok(self.advance().unwrap().line);
        }
        Err(ParseError {
            message: error_message.to_string(),
            line: self.peek().line,
        })
    }

    pub fn parse_program(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut stmts = vec![];
        while !self.at_end() {
            let stmt = self.parse_declaration()?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }

    pub fn parse_declaration(&mut self) -> Result<Statement, ParseError> {
        return match self.peek().typ {
            TokenType::Class => {
                let start_line = self.advance().unwrap().line;
                if let TokenType::Identifier(name, _) = &self.peek().typ {
                    let class_name = name.to_string();
                    let raw_name_token = self.advance().unwrap();
                    self.consume_token(
                        TokenType::LeftParen,
                        "Expected left paren after class declaration",
                    )?;
                    let mut stmts = vec![];
                    let mut fields = vec![];
                    loop {
                        if let TokenType::RightParen = self.peek().typ {
                            break;
                        }
                        if self.at_end() {
                            break;
                        }
                        let stmt = self.parse_declaration()?;
                        if let Stmt::FunctionDeclaration(_) = &stmt.typ {
                            stmts.push(stmt);
                        } else {
                            fields.push(stmt);
                        }
                    }
                    let line = self.consume_token(
                        TokenType::RightParen,
                        "Expected '}' after class declaration",
                    )?;
                    Ok(Statement {
                        from: start_line,
                        to: line,
                        typ: Stmt::ClassDeclaration(ClassDeclaration {
                            name: class_name,
                            raw_token: raw_name_token,
                            methods: stmts,
                            fields,
                        }),
                    })
                } else {
                    return Err(ParseError {
                        message: "Expected identifier after class keyword".to_string(),
                        line: start_line,
                    });
                }
            }
            TokenType::Fun => {
                let start_line = self.advance().unwrap().line;
                let token = self.advance().unwrap();
                if let TokenType::Identifier(name, _) = token.typ {
                    self.consume_token(
                        TokenType::LeftBrace,
                        "Expected left brace after function declaration",
                    )?;
                    let params = self.parse_parameters()?;
                    let line = self.consume_token(
                        TokenType::RightBrace,
                        "Expected right brace after function declaration",
                    )?;
                    let body = self.parse_stmt()?;
                    return Ok(Statement {
                        from: start_line,
                        to: line,
                        typ: Stmt::FunctionDeclaration(FunctionDeclaration {
                            params,
                            body: Box::new(body),
                            name,
                        }),
                    });
                }
                return Err(ParseError {
                    message: "Function must ahve a name".to_string(),
                    line: start_line,
                });
            }
            TokenType::Var => {
                let line = self.advance().unwrap().line;
                let token = self.advance().unwrap();
                if let TokenType::Identifier(name, _) = token.typ {
                    let line = token.line;
                    let mut initializer = Expression {
                        from: line,
                        to: line,
                        typ: Expr::NilLiteral,
                    };
                    if let TokenType::Equal = self.peek().typ {
                        self.advance();
                        initializer = self.parse_expression()?;
                    }
                    let end_line = self.consume_token(
                        TokenType::Semicolon,
                        "; expected after a variable declaration statement",
                    )?;
                    Ok(Statement {
                        from: line,
                        to: end_line,
                        typ: Stmt::VarDeclarationStatement(VariableDeclarationStatement {
                            initializer,
                            name,
                        }),
                    })
                } else {
                    Err(ParseError {
                        message: "Expected varibale identifier after keyword 'var'".to_string(),
                        line,
                    })
                }
            }
            _ => self.parse_stmt(),
        };
    }

    pub fn parse_parameters(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut rez = Vec::new();
        while let TokenType::Identifier(_, _) = &self.peek().typ {
            rez.push(self.parse_primary()?);
            if let TokenType::Comma = self.peek().typ {
                let line = self.advance().unwrap().line;
                if rez.len() > 255 {
                    return Err(ParseError {
                        message: "Cannot have more than 255 arguments for a function call"
                            .to_string(),
                        line,
                    });
                }
            } else {
                break;
            }
        }

        Ok(rez)
    }

    pub fn parse_stmt(&mut self) -> Result<Statement, ParseError> {
        let start_line = self.peek().line;
        return match self.peek().typ {
            TokenType::Print => {
                self.advance();
                let expression = self.parse_expression()?;
                let end_line =
                    self.consume_token(TokenType::Semicolon, "; expected after a print statement")?;
                Ok(Statement {
                    from: start_line,
                    to: end_line,
                    typ: Stmt::PrintStatement(PrintStatement { expression }),
                })
            }
            TokenType::Break => {
                self.advance();
                let end_line = self
                    .consume_token(TokenType::Semicolon, "; expected after a return statement")?;
                Ok(Statement {
                    from: start_line,
                    to: end_line,
                    typ: Stmt::BreakStatement,
                })
            }
            TokenType::Return => {
                self.advance();
                if let TokenType::Semicolon = self.peek().typ {
                    let end_line = self.advance().unwrap().line;
                    Ok(Statement {
                        from: start_line,
                        to: end_line,
                        typ: Stmt::ReturnStatement(ReturnStatement {
                            return_expression: None,
                        }),
                    })
                } else {
                    let expr = self.parse_expression()?;
                    let end_line = self.consume_token(
                        TokenType::Semicolon,
                        "; expected after a return statement",
                    )?;
                    Ok(Statement {
                        from: start_line,
                        to: end_line,
                        typ: Stmt::ReturnStatement(ReturnStatement {
                            return_expression: Some(expr),
                        }),
                    })
                }
            }
            TokenType::LeftParen => {
                self.advance();
                let mut stmts = vec![];
                let end_line;
                loop {
                    match self.peek().typ {
                        TokenType::RightParen => {
                            if self.at_end() {
                                return Err(ParseError {
                                    message: "Expected matching } at the end of statements"
                                        .to_string(),
                                    line: self.peek().line,
                                });
                            }
                            end_line = self.advance().unwrap().line;
                            break;
                        }
                        _ => {
                            let stmt = self.parse_declaration()?;
                            stmts.push(stmt);
                        }
                    }
                }
                return Ok(Statement {
                    from: start_line,
                    to: end_line,
                    typ: Stmt::BlockStatement(BlockStatement { statements: stmts }),
                });
            }
            TokenType::While => {
                self.advance();
                self.consume_token(
                    TokenType::LeftBrace,
                    "While statement should start with a parenthesized expression",
                )?;
                let expr = self.parse_expression()?;
                self.consume_token(
                    TokenType::RightBrace,
                    "Expression from while should contain ending brace",
                )?;
                let stmt = self.parse_stmt()?;
                let end_line = stmt.to;
                Ok(Statement {
                    from: start_line,
                    to: end_line,
                    typ: Stmt::WhileStatement(WhileStatement {
                        expression: expr,
                        statement: Box::new(stmt),
                    }),
                })
            }
            TokenType::For => {
                self.advance();
                self.consume_token(
                    TokenType::LeftBrace,
                    "For statement should start with a parenthesized expression",
                )?;
                let init_expr = match self.peek().typ {
                    TokenType::Var => Some(self.parse_declaration()?),
                    TokenType::Semicolon => {
                        self.advance();
                        None
                    }
                    _ => Some(self.parse_expression_statement()?),
                };
                let end_expr = match self.peek().typ {
                    TokenType::Semicolon => {
                        self.advance();
                        None
                    }
                    _ => Some(self.parse_expression()?),
                };
                self.consume_token(
                    TokenType::Semicolon,
                    "Should have ; in for statement expression",
                )?;
                let incr_expr = match self.peek().typ {
                    TokenType::RightBrace => None,
                    _ => Some(self.parse_expression()?),
                };
                self.consume_token(
                    TokenType::RightBrace,
                    "Expression from while should contain ending brace",
                )?;
                let stmt = self.parse_stmt()?;
                let expr = end_expr.unwrap();
                let from = expr.from;
                let to = expr.to;
                let while_stmt = Statement {
                    from: start_line,
                    to: stmt.to,
                    typ: Stmt::WhileStatement(WhileStatement {
                        expression: expr,
                        statement: Box::new(Statement {
                            from: stmt.from,
                            to: stmt.to,
                            typ: Stmt::BlockStatement(BlockStatement {
                                statements: vec![
                                    stmt,
                                    Statement {
                                        from,
                                        to,
                                        typ: Stmt::ExpressionStatement(ExpressionStatement {
                                            expression: incr_expr.unwrap(),
                                        }),
                                    },
                                ],
                            }),
                        }),
                    }),
                };
                if init_expr.is_some() {
                    Ok(Statement {
                        from: start_line,
                        to,
                        typ: Stmt::BlockStatement(BlockStatement {
                            statements: vec![init_expr.unwrap(), while_stmt],
                        }),
                    })
                } else {
                    Ok(while_stmt)
                }
            }
            TokenType::If => {
                self.advance();
                self.consume_token(
                    TokenType::LeftBrace,
                    "If statement should start with a parenthesized expression",
                )?;
                let expr = self.parse_expression()?;
                self.consume_token(
                    TokenType::RightBrace,
                    "If expression should contain ending brace",
                )?;
                let stmt = self.parse_stmt()?;
                if let TokenType::Else = self.peek().typ {
                    self.advance();
                    let else_stmt = self.parse_stmt()?;
                    let end_line = else_stmt.to;
                    return Ok(Statement {
                        from: start_line,
                        to: end_line,
                        typ: Stmt::IfStatement(IfStatement {
                            expression: expr,
                            statement: Box::new(stmt),
                            else_statement: Some(Box::new(else_stmt)),
                        }),
                    });
                } else {
                    return Ok(Statement {
                        from: start_line,
                        to: stmt.to,
                        typ: Stmt::IfStatement(IfStatement {
                            expression: expr,
                            statement: Box::new(stmt),
                            else_statement: None,
                        }),
                    });
                }
            }
            _ => self.parse_expression_statement(),
        };
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression()?;
        self.consume_token(TokenType::Semicolon, "; expected after a print statement")?;
        Ok(Statement {
            from: expression.from,
            to: expression.to,
            typ: Stmt::ExpressionStatement(ExpressionStatement { expression }),
        })
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment()
    }

    pub fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let receiver = self.parse_or_expression()?;
        if let TokenType::Equal = self.peek().typ {
            let line = self.advance().unwrap().line;
            let asignee = self.parse_assignment()?;
            if let Expr::Identifier(ident) = receiver.typ {
                return Ok(Expression {
                    from: asignee.from,
                    to: receiver.to,
                    typ: Expr::Assignment(Box::new(AssignmentExpression {
                        name: ident.raw_token,
                        asignee,
                    })),
                });
            } else {
                return Err(ParseError {
                    message: "Assignement expression illegal".to_string(),
                    line,
                });
            }
        }

        Ok(receiver)
    }

    pub fn parse_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_and_expression()?;
        while let TokenType::Or = self.peek().typ {
            let op = self.advance().unwrap();
            rez = {
                let right_hand = self.parse_and_expression()?;
                Expression {
                    from: rez.from,
                    to: right_hand.to,
                    typ: Expr::Binary(Box::new(BinaryExpression {
                        left: rez,
                        operator: op.into(),
                        right: right_hand,
                    })),
                }
            }
        }
        Ok(rez)
    }

    pub fn parse_and_expression(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_equality()?;
        while let TokenType::And = self.peek().typ {
            let op = self.advance().unwrap();
            rez = {
                let right_hand = self.parse_equality()?;
                Expression {
                    from: rez.from,
                    to: right_hand.to,
                    typ: Expr::Binary(Box::new(BinaryExpression {
                        left: rez,
                        operator: op.into(),
                        right: right_hand,
                    })),
                }
            }
        }
        Ok(rez)
    }

    pub fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_comparison()?;
        loop {
            match &self.peek().typ {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    let op = self.advance().unwrap();
                    rez = {
                        let rhs = self.parse_comparison()?;
                        Expression {
                            from: rez.from,
                            to: rhs.to,
                            typ: Expr::Binary(Box::new(BinaryExpression {
                                left: rez,
                                operator: op.into(),
                                right: rhs,
                            })),
                        }
                    };
                }
                _ => break,
            }
        }
        Ok(rez)
    }

    pub fn parse_comparison(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_term()?;
        while let TokenType::Grater
        | TokenType::GraterEqual
        | TokenType::Less
        | TokenType::LessEqual = self.peek().typ
        {
            let op = self.advance().unwrap();
            rez = {
                let right_hand = self.parse_term()?;
                Expression {
                    from: rez.from,
                    to: rez.to,
                    typ: Expr::Binary(Box::new(BinaryExpression {
                        left: rez,
                        operator: op.into(),
                        right: right_hand,
                    })),
                }
            }
        }
        Ok(rez)
    }

    pub fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_factor()?;
        while let TokenType::Plus | TokenType::Minus = self.peek().typ {
            let op = self.advance().unwrap();
            rez = {
                let right_hand = self.parse_factor()?;
                Expression {
                    from: rez.from,
                    to: rez.to,
                    typ: Expr::Binary(Box::new(BinaryExpression {
                        left: rez,
                        operator: op.into(),
                        right: right_hand,
                    })),
                }
            }
        }
        Ok(rez)
    }

    pub fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_unary()?;
        while let TokenType::Star | TokenType::Slash = self.peek().typ {
            let op = self.advance().unwrap();
            rez = {
                let right_hand = self.parse_unary()?;
                Expression {
                    from: rez.from,
                    to: rez.to,
                    typ: Expr::Binary(Box::new(BinaryExpression {
                        left: rez,
                        operator: op.into(),
                        right: right_hand,
                    })),
                }
            }
        }
        Ok(rez)
    }

    pub fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        if let TokenType::Bang | TokenType::Minus = self.peek().typ {
            let op = self.advance().unwrap();
            let right_hand = self.parse_unary()?;
            return Ok(Expression {
                from: op.line,
                to: right_hand.to,
                typ: Expr::Unary(Box::new(UnaryExpression {
                    operator: op.into(),
                    expression: right_hand,
                })),
            });
        }
        self.parse_call()
    }

    pub fn parse_call(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_primary()?;
        loop {
            if let TokenType::LeftBrace = self.peek().typ {
                self.advance();
                if let TokenType::RightBrace = self.peek().typ {
                    let end_line = self.advance().unwrap().line;
                    rez = Expression {
                        from: rez.from,
                        to: end_line,
                        typ: Expr::CallExpression(Box::new(CallExpression {
                            callee: rez,
                            arguments: vec![],
                        })),
                    };
                } else {
                    let args = self.parse_arguments()?;
                    if let TokenType::RightBrace = self.peek().typ {
                        let end_line = self.advance().unwrap().line;
                        rez = Expression {
                            from: rez.from,
                            to: end_line,
                            typ: Expr::CallExpression(Box::new(CallExpression {
                                callee: rez,
                                arguments: args,
                            })),
                        };
                    }
                }
            } else if let TokenType::Dot = self.peek().typ {
                let line = self.advance().unwrap().line;
                if let TokenType::Identifier(_, _) = self.peek().typ {
                    let token = self.advance().unwrap();
                    rez = Expression {
                        from: rez.from,
                        to: token.line,
                        typ: Expr::Member(Box::new(MemberExpression {
                            name: token,
                            object: rez,
                        })),
                    };
                } else {
                    return Err(ParseError {
                        message: "Expected identifier after .".to_string(),
                        line,
                    });
                }
            } else {
                break;
            }
        }

        Ok(rez)
    }

    pub fn parse_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut rez = Vec::new();
        let arg = self.parse_expression()?;
        rez.push(arg);
        while let TokenType::Comma = self.peek().typ {
            let line = self.advance().unwrap().line;
            let arg = self.parse_expression()?;
            rez.push(arg);
            if rez.len() > 255 {
                return Err(ParseError {
                    message: "Cannot have more than 255 arguments for a function call".to_string(),
                    line,
                });
            }
        }

        Ok(rez)
    }

    pub fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let token = self.advance().unwrap();
        match token.typ {
            TokenType::True => {
                let line = token.line;
                Ok(Expression {
                    from: line,
                    to: line,
                    typ: Expr::BoolLiteral(true),
                })
            }
            TokenType::False => {
                let line = token.line;
                Ok(Expression {
                    from: line,
                    to: line,
                    typ: Expr::BoolLiteral(false),
                })
            }
            TokenType::Number(val) => {
                let line = token.line;
                Ok(Expression {
                    from: line,
                    to: line,
                    typ: Expr::NumberLiteral(val),
                })
            }
            TokenType::String(val) => {
                let line = token.line;
                Ok(Expression {
                    from: line,
                    to: line,
                    typ: Expr::StringLiteral(val),
                })
            }
            TokenType::This(_) => Ok(Expression {
                from: token.line,
                to: token.line,
                typ: Expr::ThisExpression(Box::new(ThisExpression { keyword: token })),
            }),
            TokenType::Identifier(ref name, _) => {
                let from = token.line;
                let to = token.line;
                Ok(Expression {
                    from,
                    to,
                    typ: Expr::Identifier(Identifier {
                        name: name.clone(),
                        raw_token: token,
                    }),
                })
            }
            TokenType::Nil => {
                let line = token.line;
                Ok(Expression {
                    from: line,
                    to: line,
                    typ: Expr::NilLiteral,
                })
            }
            TokenType::LeftBrace => {
                let start_line = token.line;
                let group_expr = self.parse_expression()?;
                let end_line = self.consume_token(
                    TokenType::RightBrace,
                    "Expected token ) at the end of expression",
                )?;
                Ok(Expression {
                    from: start_line,
                    to: end_line,
                    typ: Expr::Group(Box::new(GroupExpression {
                        expression: group_expr,
                    })),
                })
            }
            _ => Err(ParseError {
                message: "Expression expected".to_string(),
                line: token.line,
            }),
        }
    }

    fn at_end(&mut self) -> bool {
        self.peek().typ == TokenType::Eof
    }

    fn peek(&mut self) -> &Token {
        self.tokens_iter.peek().unwrap()
    }

    fn advance(&mut self) -> Option<Token> {
        if self.at_end() {
            return None;
        }
        self.tokens_iter.next()
    }
}
