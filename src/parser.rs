use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expression, GroupExpression,
    Identifier, Operator, UnaryExpression,
};
use crate::stmt::{
    BlockStatement, ExpressionStatement, FunctionDeclaration, IfStatement, PrintStatement,
    ReturnStatement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::Token;
use crate::ErrorHandler;

pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    pub error_handler: &'a ErrorHandler,
    current: i32,
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, error_handler: &'a ErrorHandler) -> Self {
        Self {
            tokens,
            current: 0,
            error_handler,
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            let stmt = self.parse_declaration()?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }

    pub fn parse_declaration(&mut self) -> Result<Stmt, ParseError> {
        return match self.peek() {
            Token::Fun(_, _) => {
                self.advance();
                if let Token::Identifier(name, _) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    if let Token::LeftBrace(_, _) = self.peek() {
                        self.advance();
                        let params = self.parse_parameters()?;
                        if let Token::RightBrace(_, _) = self.peek() {
                            self.advance();
                            let body = self.parse_stmt()?;
                            return Ok(Stmt::FunctionDeclaration(FunctionDeclaration {
                                params,
                                body: Box::new(body),
                                name,
                            }));
                        } else {
                            return Err(ParseError {
                                message: "Expected right brace after function declaration"
                                    .to_string(),
                            });
                        }
                    } else {
                        return Err(ParseError {
                            message: "Expected left brace after function declaration".to_string(),
                        });
                    }
                }
                return Err(ParseError {
                    message: "Function must ahve a name".to_string(),
                });
            }
            Token::Var(_, _) => {
                self.advance();
                if let Token::Identifier(name, _) = self.peek() {
                    let name = name.clone();
                    self.advance();
                    let mut initializer = Expression::NilLiteral;
                    if let Token::Equal(_, _) = self.peek() {
                        self.advance();
                        initializer = self.parse_expression()?;
                    }
                    if let Token::Semicolon(_, _) = self.peek() {
                        self.advance();
                        Ok(Stmt::VarDeclarationStatement(
                            VariableDeclarationStatement { initializer, name },
                        ))
                    } else {
                        Err(ParseError {
                            message: "; expected after a variable declaration statement"
                                .to_string(),
                        })
                    }
                } else {
                    Err(ParseError {
                        message: "Expected varibale identifier after keyword 'var'".to_string(),
                    })
                }
            }
            _ => self.parse_stmt(),
        };
    }

    pub fn parse_parameters(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut rez = Vec::new();
        while let Token::Identifier(name, _) = self.peek() {
            rez.push(self.parse_primary()?);
            if let Token::Comma(_, _) = self.peek() {
                self.advance();
                if rez.len() > 255 {
                    return Err(ParseError {
                        message: "Cannot have more than 255 arguments for a function call"
                            .to_string(),
                    });
                }
            } else {
                break;
            }
        }

        Ok(rez)
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        return match self.peek() {
            Token::Print(_, _) => {
                self.advance();
                let expression = self.parse_expression()?;
                if let Some(Token::Semicolon(_, _)) = self.advance() {
                    Ok(Stmt::PrintStatement(PrintStatement { expression }))
                } else {
                    Err(ParseError {
                        message: "; expected after a print statement".to_string(),
                    })
                }
            }
            Token::Return(_, _) => {
                self.advance();
                if let Token::Semicolon(_, _) = self.peek() {
                    self.advance();
                    Ok(Stmt::ReturnStatement(ReturnStatement {
                        return_expression: None,
                    }))
                } else {
                    let expr = self.parse_expression()?;
                    if let Token::Semicolon(_, _) = self.peek() {
                        self.advance();
                        Ok(Stmt::ReturnStatement(ReturnStatement {
                            return_expression: Some(expr),
                        }))
                    } else {
                        Err(ParseError {
                            message: "; expected after a return statement".to_string(),
                        })
                    }
                }
            }
            Token::LeftParen(_, _) => {
                self.advance();
                let mut stmts = vec![];
                loop {
                    match self.peek() {
                        Token::RightParen(_, _) => {
                            self.advance();
                            break;
                        }
                        _ => {
                            let stmt = self.parse_declaration()?;
                            stmts.push(stmt);
                        }
                    }
                }
                return Ok(Stmt::BlockStatement(BlockStatement { statements: stmts }));
            }
            Token::While(_, _) => {
                self.advance();
                if let Some(Token::LeftBrace(_, _)) = self.advance() {
                    let expr = self.parse_expression()?;
                    if let Some(Token::RightBrace(_, _)) = self.advance() {
                        let stmt = self.parse_stmt()?;
                        return Ok(Stmt::WhileStatement(WhileStatement {
                            expression: expr,
                            statement: Box::new(stmt),
                        }));
                    } else {
                        Err(ParseError {
                            message: "Expression from while should contain ending brace"
                                .to_string(),
                        })
                    }
                } else {
                    Err(ParseError {
                        message: "While statement should start with a parenthesized expression"
                            .to_string(),
                    })
                }
            }
            Token::For(_, _) => {
                self.advance();
                if let Some(Token::LeftBrace(_, _)) = self.advance() {
                    let init_expr = match self.peek() {
                        Token::Var(_, _) => Some(self.parse_declaration()?),
                        Token::Semicolon(_, _) => {
                            self.advance();
                            None
                        }
                        _ => Some(self.parse_expression_statement()?),
                    };
                    let end_expr = match self.peek() {
                        Token::Semicolon(_, _) => {
                            self.advance();
                            None
                        }
                        _ => Some(self.parse_expression()?),
                    };
                    if let Some(Token::Semicolon(_, _)) = self.advance() {
                        let incr_expr = match self.peek() {
                            Token::RightBrace(_, _) => None,
                            _ => Some(self.parse_expression()?),
                        };
                        if let Some(Token::RightBrace(_, _)) = self.advance() {
                            let stmt = self.parse_stmt()?;
                            if init_expr.is_some() {
                                return Ok(Stmt::BlockStatement(BlockStatement {
                                    statements: vec![
                                        init_expr.unwrap(),
                                        Stmt::WhileStatement(WhileStatement {
                                            expression: end_expr.unwrap(),
                                            statement: Box::new(Stmt::BlockStatement(
                                                BlockStatement {
                                                    statements: vec![
                                                        stmt,
                                                        Stmt::ExpressionStatement(
                                                            ExpressionStatement {
                                                                expression: incr_expr.unwrap(),
                                                            },
                                                        ),
                                                    ],
                                                },
                                            )),
                                        }),
                                    ],
                                }));
                            } else {
                                return Ok(Stmt::WhileStatement(WhileStatement {
                                    expression: end_expr.unwrap(),
                                    statement: Box::new(Stmt::BlockStatement(BlockStatement {
                                        statements: vec![
                                            stmt,
                                            Stmt::ExpressionStatement(ExpressionStatement {
                                                expression: incr_expr.unwrap(),
                                            }),
                                        ],
                                    })),
                                }));
                            }
                        } else {
                            Err(ParseError {
                                message: "Expression from while should contain ending brace"
                                    .to_string(),
                            })
                        }
                    } else {
                        Err(ParseError {
                            message: "Should have ; in for statement expression".to_string(),
                        })
                    }
                } else {
                    Err(ParseError {
                        message: "For statement should start with a parenthesized expression"
                            .to_string(),
                    })
                }
            }
            Token::If(_, _) => {
                self.advance();
                if let Some(Token::LeftBrace(_, _)) = self.advance() {
                    let expr = self.parse_expression()?;
                    if let Some(Token::RightBrace(_, _)) = self.advance() {
                        let stmt = self.parse_stmt()?;
                        if let Token::Else(_, _) = self.peek() {
                            self.advance();
                            let else_stmt = self.parse_stmt()?;
                            return Ok(Stmt::IfStatement(IfStatement {
                                expression: expr,
                                statement: Box::new(stmt),
                                else_statement: Some(Box::new(else_stmt)),
                            }));
                        } else {
                            return Ok(Stmt::IfStatement(IfStatement {
                                expression: expr,
                                statement: Box::new(stmt),
                                else_statement: None,
                            }));
                        }
                    } else {
                        Err(ParseError {
                            message: "If expression should contain ending brace".to_string(),
                        })
                    }
                } else {
                    Err(ParseError {
                        message: "If statement should start with a parenthesized expression"
                            .to_string(),
                    })
                }
            }
            _ => self.parse_expression_statement(),
        };
    }

    pub fn parse_expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expression = self.parse_expression()?;
        if let Some(Token::Semicolon(_, _)) = self.advance() {
            Ok(Stmt::ExpressionStatement(ExpressionStatement {
                expression,
            }))
        } else {
            Err(ParseError {
                message: "; expected after a print statement".to_string(),
            })
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment()
    }

    pub fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let receiver = self.parse_or_expression()?;
        if let Token::Equal(_, _) = self.peek() {
            self.advance();
            let asignee = self.parse_assignment()?;
            if let Expression::Identifier(ident) = receiver {
                return Ok(Expression::Assignment(Box::new(AssignmentExpression {
                    name: ident.raw_token,
                    asignee,
                })));
            } else {
                return Err(ParseError {
                    message: "Assignement expression illegal".to_string(),
                });
            }
        }

        Ok(receiver)
    }

    pub fn parse_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_and_expression()?;
        while let Token::Or(_, _) = self.peek() {
            let op = self.advance().unwrap().clone();
            rez = {
                let right_hand = self.parse_and_expression()?;
                Expression::Binary(Box::new(BinaryExpression {
                    left: rez,
                    operator: Operator::from(&op),
                    right: right_hand,
                }))
            }
        }
        Ok(rez)
    }

    pub fn parse_and_expression(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_equality()?;
        while let Token::And(_, _) = self.peek() {
            let op = self.advance().unwrap().clone();
            rez = {
                let right_hand = self.parse_equality()?;
                Expression::Binary(Box::new(BinaryExpression {
                    left: rez,
                    operator: Operator::from(&op),
                    right: right_hand,
                }))
            }
        }
        Ok(rez)
    }

    pub fn parse_equality(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_comparison()?;
        loop {
            match self.peek() {
                Token::EqualEqual(_, _) | Token::BangEqual(_, _) => {
                    let op = self.advance().unwrap().clone();
                    rez = {
                        let rhs = self.parse_comparison()?;
                        Expression::Binary(Box::new(BinaryExpression {
                            left: rez,
                            operator: Operator::from(&op),
                            right: rhs,
                        }))
                    };
                }
                _ => break,
            }
        }
        Ok(rez)
    }

    pub fn parse_comparison(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_term()?;
        while let Token::Grater(_, _)
        | Token::GraterEqual(_, _)
        | Token::Less(_, _)
        | Token::LessEqual(_, _) = self.peek()
        {
            let op = self.advance().unwrap().clone();
            rez = {
                let right_hand = self.parse_term()?;
                Expression::Binary(Box::new(BinaryExpression {
                    left: rez,
                    operator: Operator::from(&op),
                    right: right_hand,
                }))
            }
        }
        Ok(rez)
    }

    pub fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_factor()?.clone();
        while let Token::Plus(_, _) | Token::Minus(_, _) = self.peek() {
            let op = self.advance().unwrap().clone();
            rez = {
                let right_hand = self.parse_factor()?;
                Expression::Binary(Box::new(BinaryExpression {
                    left: rez,
                    operator: Operator::from(&op),
                    right: right_hand,
                }))
            }
        }
        Ok(rez)
    }

    pub fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_unary()?;
        while let Token::Star(_, _) | Token::Slash(_, _) = self.peek() {
            let op = self.advance().unwrap().clone();
            rez = {
                let right_hand = self.parse_unary()?;
                Expression::Binary(Box::new(BinaryExpression {
                    left: rez,
                    operator: Operator::from(&op),
                    right: right_hand,
                }))
            }
        }
        Ok(rez)
    }

    pub fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        if let Token::Bang(_, _) | Token::Minus(_, _) = self.peek() {
            let op = self.advance().unwrap().clone();
            let right_hand = self.parse_unary()?;
            return Ok(Expression::Unary(Box::new(UnaryExpression {
                operator: Operator::from(&op),
                expression: right_hand,
            })));
        }
        self.parse_call()
    }

    pub fn parse_call(&mut self) -> Result<Expression, ParseError> {
        let mut rez = self.parse_primary()?;
        while let Token::LeftBrace(_, _) = self.peek() {
            self.advance();
            if let Token::RightBrace(_, _) = self.peek() {
                self.advance();
                return Ok(Expression::CallExpression(Box::new(CallExpression {
                    callee: rez,
                    arguments: vec![],
                })));
            } else {
                let args = self.parse_arguments()?;
                if let Token::RightBrace(_, _) = self.peek() {
                    self.advance();
                    return Ok(Expression::CallExpression(Box::new(CallExpression {
                        callee: rez,
                        arguments: args,
                    })));
                }
            }
        }

        Ok(rez)
    }

    pub fn parse_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut rez = Vec::new();
        let arg = self.parse_expression()?;
        rez.push(arg);
        while let Token::Comma(_, _) = self.peek() {
            self.advance();
            let arg = self.parse_expression()?;
            rez.push(arg);
            if rez.len() > 255 {
                return Err(ParseError {
                    message: "Cannot have more than 255 arguments for a function call".to_string(),
                });
            }
        }

        Ok(rez)
    }

    pub fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.peek() {
            Token::True(_, _) => {
                self.advance();
                return Ok(Expression::BoolLiteral(true));
            }
            Token::False(_, _) => {
                self.advance();
                return Ok(Expression::BoolLiteral(false));
            }
            Token::Number(_, val, _) => {
                if let Token::Number(_, val, _) = self.advance().unwrap() {
                    return Ok(Expression::NumberLiteral(val.clone()));
                } else {
                    panic!("Unreachable code");
                }
            }
            Token::String(_, val, _) => {
                if let Token::String(_, val, _) = self.advance().unwrap() {
                    return Ok(Expression::StringLiteral(val.clone()));
                } else {
                    panic!("Unreachable code");
                }
            }
            Token::Identifier(name, _) => {
                let name = name.clone();
                let raw_token = self.advance().unwrap().clone();
                return Ok(Expression::Identifier(Identifier { name, raw_token }));
            }
            Token::Nil(_, _) => {
                self.advance();
                return Ok(Expression::NilLiteral);
            }
            Token::LeftBrace(_, _) => {
                self.advance();
                let group_expr = self.parse_expression()?;
                if let Token::RightBrace(_, _) = self.peek() {
                    self.advance();
                    return Ok(Expression::Group(Box::new(GroupExpression {
                        expression: group_expr,
                    })));
                } else {
                    Err(ParseError {
                        message: "Expected token ) at the end of expression".to_string(),
                    })
                }
            }
            _ => Err(ParseError {
                message: "Expression expected".to_string(),
            }),
        }
    }

    fn is_at_end(&self) -> bool {
        if let Token::Eof(_, _) = self.tokens.get(self.current as usize).unwrap() {
            return true;
        }
        return false;
        // self.current >= self.tokens.len() as i32
    }

    fn peek(&self) -> &Token {
        return self.tokens.get(self.current as usize).unwrap();
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.is_at_end() {
            return None;
        }
        self.current += 1;
        return Some(self.tokens.get((self.current - 1) as usize).unwrap());
    }
}
