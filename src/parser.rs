use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expression, GroupExpression,
    Identifier, MemberExpression, Operator, SetMemberExpression, ThisExpression, UnaryExpression,
};
use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::{Token, TokenType};
use crate::ErrorHandler;

pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    pub error_handler: &'a ErrorHandler<'a>,
    current: i32,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub line: u128,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, error_handler: &'a ErrorHandler) -> Self {
        Self {
            tokens,
            current: 0,
            error_handler,
        }
    }

    pub fn consume_token(
        &mut self,
        token_type: TokenType,
        error_message: &str,
    ) -> Result<Token, ParseError> {
        if self.peek().typ == token_type {
            return Ok(self.advance().clone().unwrap().clone());
        }
        Err(ParseError {
            message: error_message.to_string(),
            line: self.peek().line,
        })
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
        return match self.peek().typ {
            TokenType::Class => {
                let line = self.advance().clone().unwrap().line;
                if let TokenType::Identifier(name, _) = &self.peek().typ {
                    let class_name = name.to_string();
                    let raw_name_token = self.advance().unwrap().clone();
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
                        if self.is_at_end() {
                            break;
                        }
                        let stmt = self.parse_declaration()?;
                        if let Stmt::FunctionDeclaration(_) = stmt {
                            stmts.push(stmt);
                        } else {
                            fields.push(stmt);
                        }
                    }
                    self.consume_token(
                        TokenType::RightParen,
                        "Expected '}' after class declaration",
                    )?;
                    Ok(Stmt::ClassDeclaration(ClassDeclaration {
                        name: class_name,
                        raw_token: raw_name_token,
                        methods: stmts,
                        fields,
                    }))
                } else {
                    return Err(ParseError {
                        message: "Expected identifier after class keyword".to_string(),
                        line,
                    });
                }
            }
            TokenType::Fun => {
                let line = self.advance().clone().unwrap().line;
                if let TokenType::Identifier(name, _) = self.peek().typ.clone() {
                    let name = name.clone();
                    self.advance();
                    self.consume_token(
                        TokenType::LeftBrace,
                        "Expected left brace after function declaration",
                    )?;
                    let params = self.parse_parameters()?;
                    self.consume_token(
                        TokenType::RightBrace,
                        "Expected right brace after function declaration",
                    )?;
                    let body = self.parse_stmt()?;
                    return Ok(Stmt::FunctionDeclaration(FunctionDeclaration {
                        params,
                        body: Box::new(body),
                        name,
                    }));
                }
                return Err(ParseError {
                    message: "Function must ahve a name".to_string(),
                    line,
                });
            }
            TokenType::Var => {
                let line = self.advance().clone().unwrap().line;
                if let TokenType::Identifier(name, _) = &self.peek().typ {
                    let name = name.clone();
                    self.advance();
                    let mut initializer = Expression::NilLiteral;
                    if let TokenType::Equal = self.peek().typ.clone() {
                        self.advance();
                        initializer = self.parse_expression()?;
                    }
                    self.consume_token(
                        TokenType::Semicolon,
                        "; expected after a variable declaration statement",
                    )?;
                    Ok(Stmt::VarDeclarationStatement(
                        VariableDeclarationStatement { initializer, name },
                    ))
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
            if let TokenType::Comma = self.peek().typ.clone() {
                let line = self.advance().clone().unwrap().line;
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

    pub fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        return match self.peek().typ.clone() {
            TokenType::Print => {
                self.advance();
                let expression = self.parse_expression()?;
                self.consume_token(TokenType::Semicolon, "; expected after a print statement")?;
                Ok(Stmt::PrintStatement(PrintStatement { expression }))
            }
            TokenType::Break => {
                self.advance();
                self.consume_token(TokenType::Semicolon, "; expected after a return statement")?;
                Ok(Stmt::BreakStatement)
            }
            TokenType::Return => {
                self.advance();
                if let TokenType::Semicolon = self.peek().typ.clone() {
                    self.advance();
                    Ok(Stmt::ReturnStatement(ReturnStatement {
                        return_expression: None,
                    }))
                } else {
                    let expr = self.parse_expression()?;
                    self.consume_token(
                        TokenType::Semicolon,
                        "; expected after a return statement",
                    )?;
                    Ok(Stmt::ReturnStatement(ReturnStatement {
                        return_expression: Some(expr),
                    }))
                }
            }
            TokenType::LeftParen => {
                self.advance();
                let mut stmts = vec![];
                loop {
                    match self.peek().typ.clone() {
                        TokenType::RightParen => {
                            if self.is_at_end() {
                                return Err(ParseError {
                                    message: "Expected matching } at the end of statements"
                                        .to_string(),
                                    line: self.peek().line,
                                });
                            }
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
                Ok(Stmt::WhileStatement(WhileStatement {
                    expression: expr,
                    statement: Box::new(stmt),
                }))
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
                let while_stmt = Stmt::WhileStatement(WhileStatement {
                    expression: end_expr.unwrap(),
                    statement: Box::new(Stmt::BlockStatement(BlockStatement {
                        statements: vec![
                            stmt,
                            Stmt::ExpressionStatement(ExpressionStatement {
                                expression: incr_expr.unwrap(),
                            }),
                        ],
                    })),
                });
                if init_expr.is_some() {
                    Ok(Stmt::BlockStatement(BlockStatement {
                        statements: vec![init_expr.unwrap(), while_stmt.clone()],
                    }))
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
                if let TokenType::Else = self.peek().clone().typ {
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
            }
            _ => self.parse_expression_statement(),
        };
    }

    pub fn parse_expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expression = self.parse_expression()?;
        self.consume_token(TokenType::Semicolon, "; expected after a print statement")?;
        Ok(Stmt::ExpressionStatement(ExpressionStatement {
            expression,
        }))
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment()
    }

    pub fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let receiver = self.parse_or_expression()?;
        if let TokenType::Equal = self.peek().clone().typ {
            let line = self.advance().clone().unwrap().line;
            let asignee = self.parse_assignment()?;
            if let Expression::Identifier(ident) = receiver {
                return Ok(Expression::Assignment(Box::new(AssignmentExpression {
                    name: ident.raw_token,
                    asignee,
                })));
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
        while let TokenType::Or = self.peek().clone().typ {
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
        while let TokenType::And = self.peek().typ {
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
            match &self.peek().typ {
                TokenType::EqualEqual | TokenType::BangEqual => {
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
        while let TokenType::Grater
        | TokenType::GraterEqual
        | TokenType::Less
        | TokenType::LessEqual = self.peek().typ
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
        while let TokenType::Plus | TokenType::Minus = self.peek().typ {
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
        while let TokenType::Star | TokenType::Slash = self.peek().typ {
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
        if let TokenType::Bang | TokenType::Minus = self.peek().typ {
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
        loop {
            if let TokenType::LeftBrace = self.peek().typ {
                self.advance();
                if let TokenType::RightBrace = self.peek().typ {
                    self.advance();
                    rez = Expression::CallExpression(Box::new(CallExpression {
                        callee: rez,
                        arguments: vec![],
                    }));
                } else {
                    let args = self.parse_arguments()?;
                    if let TokenType::RightBrace = self.peek().typ {
                        self.advance();
                        rez = Expression::CallExpression(Box::new(CallExpression {
                            callee: rez,
                            arguments: args,
                        }));
                    }
                }
            } else if let TokenType::Dot = self.peek().typ {
                let line = self.advance().unwrap().line;
                if let TokenType::Identifier(_, _) = self.peek().typ {
                    let token = self.advance().unwrap().clone();
                    rez = Expression::Member(Box::new(MemberExpression {
                        name: token,
                        object: rez,
                    }));
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
        match &self.peek().typ {
            TokenType::True => {
                self.advance();
                return Ok(Expression::BoolLiteral(true));
            }
            TokenType::False => {
                self.advance();
                return Ok(Expression::BoolLiteral(false));
            }
            TokenType::Number(val) => {
                let value = val.clone();
                self.advance();
                return Ok(Expression::NumberLiteral(value));
            }
            TokenType::String(val) => {
                let value = val.clone();
                self.advance();
                return Ok(Expression::StringLiteral(value));
            }
            TokenType::This(_) => {
                let keyword = self.advance().unwrap().clone();
                return Ok(Expression::ThisExpression(Box::new(ThisExpression {
                    keyword,
                })));
            }
            TokenType::Identifier(name, _) => {
                let name = name.clone();
                let raw_token = self.advance().unwrap().clone();
                return Ok(Expression::Identifier(Identifier { name, raw_token }));
            }
            TokenType::Nil => {
                self.advance();
                return Ok(Expression::NilLiteral);
            }
            TokenType::LeftBrace => {
                self.advance();
                let group_expr = self.parse_expression()?;
                self.consume_token(
                    TokenType::RightBrace,
                    "Expected token ) at the end of expression",
                )?;
                Ok(Expression::Group(Box::new(GroupExpression {
                    expression: group_expr,
                })))
            }
            _ => Err(ParseError {
                message: "Expression expected".to_string(),
                line: self.peek().line,
            }),
        }
    }

    fn is_at_end(&self) -> bool {
        if let TokenType::Eof = self.tokens.get(self.current as usize).unwrap().typ {
            return true;
        }
        return false;
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
