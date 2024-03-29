use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expression, GroupExpression,
    Identifier, MemberExpression, Operator, SetMemberExpression, ThisExpression, UnaryExpression,
};
use crate::environment::Environment;
use crate::expression_visitor::Visitor;
use crate::statement_visitor::StatementVisitor;
use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::{Token, Variable};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Add;

trait RecoverFromBreak {
    fn recover(self) -> Self;
}

impl RecoverFromBreak for Result<(), RunTimeError> {
    fn recover(self) -> Self {
        match self {
            Ok(()) => Ok(()),
            Err(e) => {
                if e.loop_break {
                    Ok(())
                } else {
                    Err(e)
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct RunTimeError {
    pub message: String,
    pub return_error: bool,
    pub loop_break: bool,
}

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    String(String),
    Number(f32),
    Nil,
    Callable(Callable),
    Class(RoxClass),
    Instance(RoxClassInstance),
}

#[derive(Debug, Clone)]
pub struct RoxClass {
    pub name: String,
    pub methods: HashMap<String, Callable>,
    pub fields: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct RoxClassInstance {
    fields: HashMap<String, Value>,
    class: RoxClass,
}

impl Display for RoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", self.name.to_string());
    }
}

impl Display for RoxClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{} instance", "TGWT");
    }
}

impl RoxClass {
    pub fn constructor(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> Result<Value, RunTimeError> {
        let method = self.methods.get("constructor");
        if let Some(mut init_method) = method {
            let mut end_class = self.clone();
            for (index, param) in init_method.params.iter().enumerate() {
                if end_class.fields.contains_key(&param.get_name()) {
                    return Err(RunTimeError {
                        message: format!(
                            "Property {} already defined on instance",
                            param.get_name().clone()
                        ),
                        return_error: false,
                        loop_break: false,
                    });
                }
                end_class
                    .fields
                    .insert(param.get_name(), args.get(index).unwrap().clone());
            }
            let instance = RoxClassInstance {
                fields: HashMap::new(),
                class: end_class,
            };
            init_method
                .clone()
                .bind(&instance)
                .clone()
                .call(interpreter, args)?;
            return Ok(Value::Instance(instance));
        }
        Ok(Value::Instance(RoxClassInstance {
            fields: HashMap::new(),
            class: self.clone(),
        }))
    }
}

#[derive(Debug, Clone)]
pub struct Callable {
    pub arity: u8,
    pub body: Option<Stmt>,
    pub params: Vec<Expression>,
    pub closure: Environment,
    pub std_call:
        Option<fn(int: &mut Interpreter, args: Vec<Value>) -> Result<Value, RunTimeError>>,
}

impl Callable {
    pub fn bind(&mut self, inst: &RoxClassInstance) -> Self {
        let mut env = self.closure.extend();
        env.define("this".to_string(), Value::Instance(inst.clone()));
        Self {
            arity: self.arity,
            body: self.body.clone(),
            params: self.params.clone(),
            closure: env,
            std_call: self.std_call,
        }
    }
    pub fn call(&mut self, int: &mut Interpreter, args: Vec<Value>) -> Result<Value, RunTimeError> {
        let mut env = self.closure.extend();
        for (ind, it) in args.iter().enumerate() {
            env.define(self.params[ind].get_identifier_name(), it.clone());
        }
        match self.body {
            Some(ref mut bd) => {
                let rez = bd.accept(int, &mut env);
                match rez {
                    Ok(()) => Ok(Value::Nil),
                    Err(e) => {
                        if e.return_error {
                            let return_value = int.ret_val.clone();
                            int.ret_val = Value::Nil;
                            Ok(return_value)
                        } else {
                            Err(e)
                        }
                    }
                }
            }
            None => {
                return match self.std_call {
                    Some(fun) => Ok(fun(int, args)?),
                    None => Ok(Value::Nil),
                }
            }
        }
    }
    pub fn from_node(func: &mut FunctionDeclaration, env: Environment) -> Self {
        Self {
            arity: func.params.len() as u8,
            body: Some(*func.body.clone()),
            params: func.params.clone(),
            std_call: None,
            closure: env,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Nil => "Nil".to_string(),
            Self::Bool(bl) => bl.to_string(),
            Self::Number(nr) => nr.to_string(),
            Self::String(st) => st.to_string(),
            Self::Callable(_) => "".to_string(),
            Self::Class(class_decl) => class_decl.name.to_string(),
            Self::Instance(class_inst) => class_inst.to_string(),
        };
        return write!(f, "{}", str);
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Bool(el) => el.clone(),
            _ => true,
        }
    }

    pub fn get_number(&self) -> Result<&f32, RunTimeError> {
        match self {
            Self::Number(x) => {
                return Ok(x);
            }
            _ => {
                return Err(RunTimeError {
                    message: format!("Value {self} is not a number").to_string(),
                    return_error: false,
                    loop_break: false,
                });
            }
        }
    }

    pub fn get_string(&self) -> Result<&String, RunTimeError> {
        match self {
            Self::String(x) => {
                return Ok(x);
            }
            _ => {
                return Err(RunTimeError {
                    message: format!("Value {self} is not a string").to_string(),
                    return_error: false,
                    loop_break: false,
                });
            }
        }
    }

    pub fn get_boolean(&self) -> Result<&bool, RunTimeError> {
        return match self {
            Self::Bool(x) => Ok(x),
            _ => Err(RunTimeError {
                message: format!("Value {self} is not a bool").to_string(),
                return_error: false,
                loop_break: false,
            }),
        };
    }

    pub fn is_nil(&self) -> bool {
        return match self {
            Self::Nil => true,
            _ => false,
        };
    }
}

pub struct Interpreter {
    pub ret_val: Value,
    pub locals: HashMap<Variable, u32>,
}

impl Interpreter {
    pub fn interpret(
        &mut self,
        stmts: &mut Vec<Stmt>,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        for stmt in stmts {
            stmt.accept::<Self, (), RunTimeError>(self, env)?;
        }
        Ok(())
    }

    pub fn resolve(&mut self, expr: &Token, count: u32) {
        self.locals.insert(expr.get_variable(), count);
    }
}

impl StatementVisitor<(), RunTimeError> for Interpreter {
    fn visit_break_statement(&mut self, _env: &mut Environment) -> Result<(), RunTimeError> {
        Err(RunTimeError {
            message: "".to_string(),
            return_error: false,
            loop_break: true,
        })
    }

    fn visit_class_declaration(
        &mut self,
        class_declaration: &mut ClassDeclaration,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        let mut methods = HashMap::new();
        let mut fields = HashMap::new();
        for field in &mut class_declaration.fields {
            if let Stmt::VarDeclarationStatement(func_dec) = field {
                fields.insert(
                    func_dec.name.clone(),
                    func_dec.initializer.accept(self, env)?,
                );
            }
        }
        for method in &mut class_declaration.methods {
            if let Stmt::FunctionDeclaration(func_dec) = method {
                methods.insert(
                    func_dec.name.clone(),
                    Callable::from_node(func_dec, env.clone()),
                );
            }
        }
        env.define(
            class_declaration.name.clone(),
            Value::Class(RoxClass {
                name: class_declaration.name.clone(),
                methods,
                fields,
            }),
        );
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        if_statement: &mut IfStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        if if_statement.expression.accept(self, env)?.is_truthy() {
            if_statement.statement.accept(self, env)?;
        } else {
            match if_statement.else_statement {
                Some(ref mut stmt) => {
                    stmt.accept(self, env)?;
                }
                None => {}
            };
        }
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_dec: &mut FunctionDeclaration,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        env.define(
            func_dec.name.clone(),
            Value::Callable(Callable::from_node(func_dec, env.clone())),
        );
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        let value = return_stmt
            .return_expression
            .clone()
            .unwrap()
            .accept(self, env)?;
        self.ret_val = value;
        Err(RunTimeError {
            message: "".to_string(),
            return_error: true,
            loop_break: false,
        })
    }

    fn visit_while_statement(
        &mut self,
        while_statement: &mut WhileStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        loop {
            let cond = while_statement.expression.accept(self, env)?;
            if !cond.is_truthy() {
                break;
            }
            match while_statement.statement.accept(self, env) {
                Ok(()) => {}
                Err(e) => {
                    if e.loop_break {
                        break;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_declaration_statement(
        &mut self,
        decl: &mut VariableDeclarationStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        let init = { decl.initializer.accept(self, env)?.clone() };
        env.define(decl.name.clone(), init);
        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        decl: &mut BlockStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        let mut child_env = env.extend();
        for mut stmt in &mut decl.statements {
            stmt.accept(self, &mut child_env)?;
        }
        return Ok(());
    }

    fn visit_statement_expression(
        &mut self,
        expr: &mut ExpressionStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        expr.expression.accept(self, env)?;
        return Ok(());
    }

    fn visit_print_statement(
        &mut self,
        expr: &mut PrintStatement,
        env: &mut Environment,
    ) -> Result<(), RunTimeError> {
        println!("{}", expr.expression.accept(self, env)?);
        return Ok(());
    }
}

impl Visitor<Value, RunTimeError> for Interpreter {
    fn visit_this_expression(
        &mut self,
        expr: &mut ThisExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        let dist = self.locals.get(&expr.keyword.get_variable());
        if dist.is_some() {
            env.get_at(&"this".to_string(), dist.unwrap())
        } else {
            Ok(env.get(&"this".to_string())?.clone())
        }
    }

    fn visit_set_member_expression(
        &mut self,
        expr: &mut SetMemberExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        let mut obj = expr.object.accept(self, env)?;
        let name = expr.name.get_variable().name;
        let value = expr.value.accept(self, env)?;
        if let Value::Instance(ref mut inst) = obj {
            inst.fields.insert(name, value.clone());
            return Ok(value.clone());
        }
        Err(RunTimeError {
            message: "Undefined variable".to_string(),
            return_error: false,
            loop_break: false,
        })
    }

    fn visit_call_expression(
        &mut self,
        expr: &mut CallExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        let callable = expr.callee.accept(self, env)?;
        if let Value::Callable(mut callable) = callable {
            let mut args = Vec::new();
            for arg in &mut expr.arguments {
                args.push(arg.accept(self, env)?);
            }

            if args.len() != callable.arity as usize {
                return Err(RunTimeError {
                    message: format!(
                        "Expected {} arguments, but found {}",
                        callable.arity,
                        args.len()
                    )
                    .to_string(),
                    return_error: false,
                    loop_break: false,
                });
            }

            return Ok(callable.call(self, args)?);
        }
        if let Value::Class(mut class) = callable {
            let mut args = Vec::new();
            for arg in &mut expr.arguments {
                args.push(arg.accept(self, env)?);
            }
            return Ok(class.constructor(self, args)?);
        }
        return Err(RunTimeError {
            message: "Expression not callable".to_string(),
            return_error: false,
            loop_break: false,
        });
    }

    fn visit_member_expression(
        &mut self,
        expr: &mut MemberExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        let instance = expr.object.accept(self, env)?;
        if let Value::Instance(mut instance) = instance {
            let mut mthd = instance.class.methods.get(&expr.name.get_variable().name);
            if let Some(ref mut method) = mthd {
                let value = method.clone().bind(&instance).clone();
                return Ok(Value::Callable(value));
            }
            let field = instance.class.fields.get(&expr.name.get_variable().name);
            if let Some(value) = field {
                return Ok(value.clone());
            } else {
                return Err(RunTimeError {
                    message: format!("No such field for {}", instance.to_string()).to_string(),
                    return_error: false,
                    loop_break: false,
                });
            }
        }

        return Err(RunTimeError {
            message: "Object is not a class".to_string(),
            return_error: false,
            loop_break: false,
        });
    }

    fn visit_assignement_expression(
        &mut self,
        expr: &mut AssignmentExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        let val = expr.asignee.accept(self, env)?;
        let dist = self.locals.get(&expr.name.get_variable());
        if dist.is_some() {
            env.assign_at(expr.name.get_variable().name, val.clone(), dist.unwrap())?;
        } else {
            env.assign(expr.name.get_variable().name, val.clone())?;
        }
        Ok(val)
    }

    fn visit_binary_expression(
        &mut self,
        expr: &mut BinaryExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        return match expr.operator {
            Operator::Div => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env);
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env);
                Ok(Value::Number(
                    value_left?.get_number()? / value_right?.get_number()?,
                ))
            }
            Operator::Plus => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(el) => Ok(Value::Number(value_right.get_number()? + el)),
                    Value::String(st) => {
                        Ok(Value::String(st.add(value_right.get_string()?.as_str())))
                    }
                    _ => Err(RunTimeError {
                        message: "Cannot add 2 values that are not both strings or numbers"
                            .to_string(),
                        return_error: false,
                        loop_break: false,
                    }),
                }
            }
            Operator::Minus => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env);
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env);
                Ok(Value::Number(
                    value_left?.get_number()? - value_right?.get_number()?,
                ))
            }
            Operator::Mul => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env);
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env);
                Ok(Value::Number(
                    value_left?.get_number()? * value_right?.get_number()?,
                ))
            }
            Operator::EQ => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(nr) => Ok(Value::Bool(nr == value_right.get_number()?.clone())),
                    Value::String(str_l) => {
                        Ok(Value::Bool(str_l == value_right.get_string()?.clone()))
                    }
                    Value::Bool(vl) => Ok(Value::Bool(vl == value_right.get_boolean()?.clone())),
                    Value::Nil => Ok(Value::Bool(value_right.is_nil())),
                    _ => Ok(Value::Bool(false)),
                }
            }
            Operator::NotEqual => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(nr) => Ok(Value::Bool(nr != value_right.get_number()?.clone())),
                    Value::String(str_l) => {
                        Ok(Value::Bool(str_l != value_right.get_string()?.clone()))
                    }
                    Value::Bool(vl) => Ok(Value::Bool(vl != value_right.get_boolean()?.clone())),
                    Value::Nil => Ok(Value::Bool(!value_right.is_nil())),
                    _ => Ok(Value::Bool(false)),
                }
            }
            Operator::GE => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr >= value_right.get_number()?.clone()))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false })
                    }
                }
            }
            Operator::LE => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr <= value_right.get_number()?.clone()))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false })
                    }
                }
            }
            Operator::Less => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr < value_right.get_number()?.clone()))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false })
                    }
                }
            }
            Operator::Greater => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr > value_right.get_number()?.clone()))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false })
                    }
                }
            }
            Operator::And => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                if !value_left.is_truthy() {
                    return Ok(value_left);
                }
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                Ok(value_right)
            }
            Operator::Or => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self, env)?;
                if value_left.is_truthy() {
                    return Ok(value_left);
                }
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self, env)?;
                Ok(value_right)
            }
            _ => Err(RunTimeError {
                message: format!(
                    "Operator {:?} is not a valid operator for a binary expression",
                    expr.operator
                ),
                return_error: false,
                loop_break: false,
            }),
        };
    }

    fn visit_identifier_expression(
        &mut self,
        expr: &Identifier,
        env: &Environment,
    ) -> Result<Value, RunTimeError> {
        let dist = self.locals.get(&expr.raw_token.get_variable());
        if dist.is_some() {
            env.get_at(&expr.name, dist.unwrap())
        } else {
            Ok(env.get(&expr.name)?.clone())
        }
    }

    fn visit_unary_expression(
        &mut self,
        expr: &mut UnaryExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        return match expr.operator {
            Operator::Not => Ok(Value::Bool(
                !expr
                    .expression
                    .accept::<Self, Value, RunTimeError>(self, env)?
                    .is_truthy(),
            )),
            Operator::Minus => {
                let value = expr
                    .expression
                    .accept::<Self, Value, RunTimeError>(self, env)?;
                Ok(Value::Number(-value.get_number()?.clone()))
            }
            _ => Err(RunTimeError {
                message: format!(
                    "Operator {:?} is not a valid operator for a unary expression",
                    expr.operator
                ),
                return_error: false,
                loop_break: false,
            }),
        };
    }

    fn visit_group_expression(
        &mut self,
        expr: &mut GroupExpression,
        env: &mut Environment,
    ) -> Result<Value, RunTimeError> {
        expr.expression.accept(self, env)
    }

    fn visit_nil_literal(&self) -> Value {
        Value::Nil
    }

    fn visit_string_literal(&self, expr: &String) -> Value {
        Value::String(expr.clone())
    }

    fn visit_number_literal(&self, expr: &f32) -> Value {
        Value::Number(expr.clone())
    }

    fn visit_bool_literal(&self, expr: &bool) -> Value {
        Value::Bool(expr.clone())
    }
}
