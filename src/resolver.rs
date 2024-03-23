use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expression, GroupExpression,
    Identifier, MemberExpression, SetMemberExpression, ThisExpression, UnaryExpression,
};
use crate::environment::Environment;
use crate::expression_visitor::Visitor;
use crate::interpreter::Interpreter;
use crate::statement_visitor::StatementVisitor;
use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::Token;
use std::collections::HashMap;

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    ln: usize,
    current_function: FunctionType,
    current_type: ClassType,
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
        }
    }

    pub fn resolve(&mut self, env: &mut Environment, stmts: &mut Vec<Stmt>) {
        stmts.iter_mut().for_each(|mut stmt| {
            stmt.accept(self, env);
        });
    }

    pub fn resolve_function(
        &mut self,
        func_dec: &mut FunctionDeclaration,
        env: &mut Environment,
        typ: FunctionType,
    ) -> Result<(), ()> {
        let enc_fun = self.current_function.clone();
        self.current_function = typ;

        self.begin_scope();
        for mut param in &mut func_dec.params {
            self.declare(&param.get_name());
            self.define(&param.get_name());
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

    fn declare(&mut self, lexeme: &String) {
        if self.ln == 0 {
            return;
        }
        let mut scope = self.scopes.get_mut(self.ln - 1).unwrap();
        scope.insert(lexeme.clone(), false);
    }

    fn define(&mut self, lexeme: &String) {
        if self.ln == 0 {
            return;
        }
        let mut scope = self.scopes.get_mut(self.ln - 1).unwrap();
        scope.entry(lexeme.clone()).and_modify(|v| *v = true);
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

impl Visitor<(), ()> for Resolver<'_> {
    fn visit_this_expression(
        &mut self,
        expr: &mut ThisExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        if self.current_type == ClassType::None {
            println!("This statement not allowed outside class scope");
            return Err(());
        }
        self.resolve_local(&expr.keyword);
        Ok(())
    }

    fn visit_set_member_expression(
        &mut self,
        expr: &mut SetMemberExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.value.accept(self, env)?;
        expr.object.accept(self, env)?;
        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        expr: &mut CallExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.callee.accept(self, env)?;
        expr.arguments.iter_mut().for_each(|mut arg| {
            arg.accept(self, env);
        });
        Ok(())
    }

    fn visit_member_expression(
        &mut self,
        expr: &mut MemberExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.object.accept(self, env)?;
        Ok(())
    }

    fn visit_assignement_expression(
        &mut self,
        expr: &mut AssignmentExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.asignee.accept(self, env)?;
        self.resolve_local(&expr.name);
        Ok(())
    }

    fn visit_binary_expression(
        &mut self,
        expr: &mut BinaryExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.left.accept(self, env)?;
        expr.right.accept(self, env)?;
        Ok(())
    }

    fn visit_identifier_expression(
        &mut self,
        expr: &Identifier,
        env: &Environment,
    ) -> Result<(), ()> {
        // if !self.scopes.is_empty()
        //     && self.scopes.get(self.ln - 1).unwrap().get(&expr.name) == Some(&false)
        // {
        //     return Err(());
        // }
        self.resolve_local(&expr.raw_token);
        Ok(())
    }

    fn visit_unary_expression(
        &mut self,
        expr: &mut UnaryExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.expression.accept(self, env)?;
        Ok(())
    }

    fn visit_group_expression(
        &mut self,
        expr: &mut GroupExpression,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.expression.accept(self, env)?;
        Ok(())
    }

    fn visit_nil_literal(&self) -> () {}

    fn visit_string_literal(&self, expr: &String) -> () {}

    fn visit_number_literal(&self, expr: &f32) -> () {}

    fn visit_bool_literal(&self, expr: &bool) -> () {}
}

impl StatementVisitor<(), ()> for Resolver<'_> {
    fn visit_class_declaration(
        &mut self,
        class_declaration: &mut ClassDeclaration,
        env: &mut Environment,
    ) -> Result<(), ()> {
        self.declare(&class_declaration.name);
        self.define(&class_declaration.name);
        let typ = self.current_type.clone();
        self.current_type = ClassType::This;
        self.begin_scope();

        self.declare(&"this".to_string());
        self.define(&"this".to_string());

        for var in &mut class_declaration.fields {
            if let Stmt::VarDeclarationStatement(func_dec) = var {
                self.declare(&func_dec.name);
                func_dec.initializer.accept(self, env)?;
                self.define(&func_dec.name);
            }
        }
        for method in &mut class_declaration.methods {
            if let Stmt::FunctionDeclaration(func_dec) = method {
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
    ) -> Result<(), ()> {
        if_statement.expression.accept(self, env)?;
        if_statement.statement.accept(self, env)?;
        if if_statement.else_statement.is_some() {
            if_statement
                .else_statement
                .clone()
                .unwrap()
                .accept(self, env)?;
        }

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_dec: &mut FunctionDeclaration,
        env: &mut Environment,
    ) -> Result<(), ()> {
        self.declare(&func_dec.name);
        self.define(&func_dec.name);
        self.resolve_function(func_dec, env, FunctionType::Function);
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
        env: &mut Environment,
    ) -> Result<(), ()> {
        if self.current_function == FunctionType::None {
            println!("Return statement not allowed here");
            return Err(());
        }
        if return_stmt.return_expression.is_some() {
            return_stmt
                .return_expression
                .clone()
                .unwrap()
                .accept(self, env)?;
        }
        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_statement: &mut WhileStatement,
        env: &mut Environment,
    ) -> Result<(), ()> {
        while_statement.expression.accept(self, env)?;
        while_statement.statement.accept(self, env)?;
        Ok(())
    }

    fn visit_declaration_statement(
        &mut self,
        decl: &mut VariableDeclarationStatement,
        env: &mut Environment,
    ) -> Result<(), ()> {
        self.declare(&decl.name);
        decl.initializer.accept(self, env)?;
        self.define(&decl.name);
        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        decl: &mut BlockStatement,
        env: &mut Environment,
    ) -> Result<(), ()> {
        self.begin_scope();
        for mut stmt in &mut decl.statements {
            stmt.accept(self, env)?;
        }
        self.end_scope();
        Ok(())
    }

    fn visit_statement_expression(
        &mut self,
        expr: &mut ExpressionStatement,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.expression.accept(self, env)?;
        Ok(())
    }

    fn visit_print_statement(
        &mut self,
        expr: &mut PrintStatement,
        env: &mut Environment,
    ) -> Result<(), ()> {
        expr.expression.accept(self, env)?;
        Ok(())
    }
}
