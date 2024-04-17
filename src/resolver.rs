use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, GroupExpression, Identifier,
    MemberExpression, ThisExpression, UnaryExpression,
};
use crate::environment::Environment;
use crate::errors::ResolverError;
use crate::expression_visitor::Visitor;
use crate::interpreter::Interpreter;
use crate::statement_visitor::StatementVisitor;
use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Statement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::{Location, Token};
use std::collections::HashMap;

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    ln: usize,
    current_function: FunctionType,
    current_type: ClassType,
    is_loop: bool,
    location: Location,
}

#[derive(Clone, PartialEq)]
pub enum FunctionType {
    None,
    Function,
    Method,
}

#[derive(Clone, PartialEq)]
pub enum ClassType {
    None,
    This,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![HashMap::new()],
            ln: 1,
            current_function: FunctionType::None,
            current_type: ClassType::None,
            is_loop: false,
            location: Location { from: 0, to: 0 },
        }
    }

    pub fn resolve(
        &mut self,
        env: &mut Environment,
        stmts: &mut Vec<Statement>,
    ) -> Result<(), ResolverError> {
        for stmt in stmts {
            stmt.accept(self, env)?;
        }

        Ok(())
    }

    pub fn resolve_function(
        &mut self,
        func_dec: &mut FunctionDeclaration,
        env: &mut Environment,
        typ: FunctionType,
    ) -> Result<(), ResolverError> {
        let enc_fun = self.current_function.clone();
        self.current_function = typ;

        self.begin_scope();
        for param in &mut func_dec.params {
            self.declare(param.get_name());
            self.define(param.get_name());
            param.accept(self, env)?;
        }
        func_dec.body.accept(self, env)?;
        self.end_scope();

        self.current_function = enc_fun;

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.ln += 1;
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
        self.ln -= 1;
    }

    fn declare(&mut self, lexeme: &str) {
        if self.ln == 0 {
            return;
        }
        let mut scope = self.scopes.get_mut(self.ln - 1).unwrap();
        scope.insert(lexeme.to_string(), false);
    }

    fn define(&mut self, lexeme: &str) {
        if self.ln == 0 {
            return;
        }
        let mut scope = self.scopes.get_mut(self.ln - 1).unwrap();
        scope.entry(lexeme.to_string()).and_modify(|v| *v = true);
    }

    fn resolve_local(&mut self, expr: &Token) {
        for i in (0..=(self.ln - 1)).rev() {
            if self
                .scopes
                .get(i)
                .unwrap()
                .contains_key(&expr.get_variable().name)
            {
                self.interpreter.resolve(expr, (self.ln - 1 - i) as u32);
                return;
            }
        }
    }
}

impl Visitor<(), ResolverError> for Resolver<'_> {
    fn set_current_location(&mut self, location: Location) {
        self.location = location;
    }

    fn visit_this_expression(
        &mut self,
        expr: &mut ThisExpression,
        _env: &mut Environment,
    ) -> Result<(), ResolverError> {
        if self.current_type == ClassType::None {
            return Err(ResolverError {
                message: "This statement not allowed outside class scope".to_string(),
                from: self.location.from,
                to: self.location.to,
            });
        }
        self.resolve_local(&expr.keyword);
        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        expr: &mut CallExpression,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.callee.accept(self, env)?;
        for arg in &mut expr.arguments {
            arg.accept(self, env)?;
        }
        Ok(())
    }

    fn visit_member_expression(
        &mut self,
        expr: &mut MemberExpression,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.object.accept(self, env)?;
        Ok(())
    }

    fn visit_assignement_expression(
        &mut self,
        expr: &mut AssignmentExpression,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.asignee.accept(self, env)?;
        self.resolve_local(&expr.name);
        Ok(())
    }

    fn visit_binary_expression(
        &mut self,
        expr: &mut BinaryExpression,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.left.accept(self, env)?;
        expr.right.accept(self, env)?;
        Ok(())
    }

    fn visit_identifier_expression(
        &mut self,
        expr: &Identifier,
        _env: &Environment,
    ) -> Result<(), ResolverError> {
        self.resolve_local(&expr.raw_token);
        Ok(())
    }

    fn visit_unary_expression(
        &mut self,
        expr: &mut UnaryExpression,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.expression.accept(self, env)?;
        Ok(())
    }

    fn visit_group_expression(
        &mut self,
        expr: &mut GroupExpression,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.expression.accept(self, env)?;
        Ok(())
    }

    fn visit_nil_literal(&self) -> () {}

    fn visit_string_literal(&self, _expr: &String) -> () {}

    fn visit_number_literal(&self, _expr: &f32) -> () {}

    fn visit_bool_literal(&self, _expr: &bool) -> () {}
}

impl StatementVisitor<(), ResolverError> for Resolver<'_> {
    fn set_current_location(&mut self, location: Location) {
        self.location = location;
    }

    fn visit_break_statement(&mut self, _env: &mut Environment) -> Result<(), ResolverError> {
        if !self.is_loop {
            return Err(ResolverError {
                message: "Forbidden use of break statement".to_string(),
                from: self.location.from,
                to: self.location.to,
            });
        }

        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        class_declaration: &mut ClassDeclaration,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        self.declare(&class_declaration.name);
        self.define(&class_declaration.name);
        let typ = self.current_type.clone();
        self.current_type = ClassType::This;
        self.begin_scope();

        self.declare(&"this".to_string());
        self.define(&"this".to_string());

        for var in &mut class_declaration.fields {
            if let Stmt::VarDeclarationStatement(func_dec) = &mut var.typ {
                self.declare(&func_dec.name);
                func_dec.initializer.accept(self, env)?;
                self.define(&func_dec.name);
            }
        }
        for method in &mut class_declaration.methods {
            if let Stmt::FunctionDeclaration(func_dec) = &mut method.typ {
                self.resolve_function(func_dec, env, FunctionType::Method)?;
            }
        }

        self.end_scope();
        self.current_type = typ;
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        if_statement: &mut IfStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        if_statement.expression.accept(self, env)?;
        if_statement.statement.accept(self, env)?;
        match &mut if_statement.else_statement {
            None => {}
            Some(else_stmt) => else_stmt.accept(self, env)?,
        }

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_dec: &mut FunctionDeclaration,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        self.declare(&func_dec.name);
        self.define(&func_dec.name);
        self.resolve_function(func_dec, env, FunctionType::Function)?;
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        if self.current_function == FunctionType::None {
            return Err(ResolverError {
                message: "Return statement not allowed here".to_string(),
                from: self.location.from,
                to: self.location.to,
            });
        }
        match &mut return_stmt.return_expression {
            Some(rt_expr) => {
                rt_expr.accept(self, env)?;
            }
            None => {}
        }
        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_statement: &mut WhileStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        let init = self.is_loop;
        self.is_loop = true;
        while_statement.expression.accept(self, env)?;
        while_statement.statement.accept(self, env)?;
        self.is_loop = init;
        Ok(())
    }

    fn visit_declaration_statement(
        &mut self,
        decl: &mut VariableDeclarationStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        self.declare(&decl.name);
        decl.initializer.accept(self, env)?;
        self.define(&decl.name);
        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        decl: &mut BlockStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        self.begin_scope();
        for stmt in &mut decl.statements {
            stmt.accept(self, env)?;
        }
        self.end_scope();
        Ok(())
    }

    fn visit_statement_expression(
        &mut self,
        expr: &mut ExpressionStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.expression.accept(self, env)?;
        Ok(())
    }

    fn visit_print_statement(
        &mut self,
        expr: &mut PrintStatement,
        env: &mut Environment,
    ) -> Result<(), ResolverError> {
        expr.expression.accept(self, env)?;
        Ok(())
    }
}
