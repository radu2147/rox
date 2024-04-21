use crate::ast_types::{
    AssignmentExpression, BinaryExpression, CallExpression, Expression, GroupExpression,
    Identifier, MemberExpression, Operator, ThisExpression, UnaryExpression,
};
use crate::environment::Environment;
use crate::errors::RunTimeError;
use crate::expression_visitor::OwnedVisitor;
use crate::statement_visitor::OwnedStatementVisitor;
use crate::stmt::{
    BlockStatement, ClassDeclaration, ExpressionStatement, FunctionDeclaration, IfStatement,
    PrintStatement, ReturnStatement, Statement, Stmt, VariableDeclarationStatement, WhileStatement,
};
use crate::types::{Location, Token, VariableToken};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Add;

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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", self.name);
    }
}

impl Display for RoxClassInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{} instance", self.class.name);
    }
}

impl RoxClass {
    pub fn constructor(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> Result<Value, RunTimeError> {
        let method = self.methods.get("constructor");
        if let Some(init_method) = method {
            let mut end_class = self.clone();
            for (index, arg) in args.iter().enumerate() {
                end_class.fields.insert(
                    init_method.params.get(index).unwrap().get_name().clone(),
                    arg.clone(),
                );
            }
            let instance = RoxClassInstance {
                fields: HashMap::new(),
                class: end_class,
            };
            init_method
                .clone()
                .bind(&instance)
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
    pub body: Option<Statement>,
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
        match self.body {
            Some(ref mut bd) => {
                let mut env = self.closure.extend();
                for (ind, it) in args.into_iter().enumerate() {
                    env.define(self.params[ind].get_identifier_name().to_string(), it);
                }
                let copy_env = int.environment.clone();
                int.environment = env;
                let rez = bd.clone().accept(int);
                int.environment = copy_env;
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
    pub fn from_node(func_params: Vec<Expression>, func_body: Statement, env: Environment) -> Self {
        Self {
            arity: func_params.len() as u8,
            body: Some(func_body),
            params: func_params,
            std_call: None,
            closure: env,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => {
                write!(f, "NIL")
            }
            Self::Bool(bl) => {
                write!(f, "{bl}")
            }
            Self::Number(nr) => {
                write!(f, "{nr}")
            }
            Self::String(st) => {
                write!(f, "{st}")
            }
            Self::Callable(_) => {
                write!(f, "")
            }
            Self::Class(class_decl) => {
                write!(f, "{}", class_decl.name)
            }
            Self::Instance(class_inst) => {
                write!(f, "{}", class_inst)
            }
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Bool(el) => *el,
            _ => true,
        }
    }

    pub fn get_number(&self, interpreter: &Interpreter) -> Result<&f32, RunTimeError> {
        match self {
            Self::Number(x) => {
                return Ok(x);
            }
            _ => {
                return Err(RunTimeError {
                    message: format!("Value {self} is not a number"),
                    return_error: false,
                    loop_break: false,
                    from: interpreter.location.from,
                    to: interpreter.location.to,
                });
            }
        }
    }

    pub fn get_string(&self, interpreter: &Interpreter) -> Result<&String, RunTimeError> {
        match self {
            Self::String(x) => {
                return Ok(x);
            }
            _ => {
                return Err(RunTimeError {
                    message: format!("Value {self} is not a string"),
                    return_error: false,
                    loop_break: false,
                    from: interpreter.location.from,
                    to: interpreter.location.to,
                });
            }
        }
    }

    pub fn get_boolean(&self, interpreter: &Interpreter) -> Result<&bool, RunTimeError> {
        return match self {
            Self::Bool(x) => Ok(x),
            _ => Err(RunTimeError {
                message: format!("Value {self} is not a bool"),
                return_error: false,
                loop_break: false,
                from: interpreter.location.from,
                to: interpreter.location.to,
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
    pub locals: HashMap<VariableToken, u32>,
    pub location: Location,
    pub environment: Environment,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: Vec<Statement>) -> Result<(), RunTimeError> {
        for mut stmt in stmts {
            stmt.accept::<Self, (), RunTimeError>(self)?;
        }
        Ok(())
    }

    pub fn resolve(&mut self, expr: &Token, count: u32) {
        self.locals.insert(expr.get_variable(), count);
    }
}

impl OwnedStatementVisitor<(), RunTimeError> for Interpreter {
    fn set_current_location(&mut self, location: Location) {
        self.location = location;
    }

    fn visit_break_statement(&mut self) -> Result<(), RunTimeError> {
        Err(RunTimeError {
            message: "".to_string(),
            return_error: false,
            loop_break: true,
            from: self.location.from,
            to: self.location.to,
        })
    }

    fn visit_class_declaration(
        &mut self,
        class_declaration: ClassDeclaration,
    ) -> Result<(), RunTimeError> {
        let mut methods = HashMap::new();
        let mut fields = HashMap::new();
        for field in class_declaration.fields {
            if let Stmt::VarDeclarationStatement(func_dec) = field.typ {
                fields.insert(func_dec.name, func_dec.initializer.accept(self)?);
            }
        }
        for method in class_declaration.methods {
            if let Stmt::FunctionDeclaration(func_dec) = method.typ {
                methods.insert(
                    func_dec.name,
                    Callable::from_node(func_dec.params, *func_dec.body, self.environment.clone()),
                );
            }
        }
        self.environment.define(
            class_declaration.name.clone(),
            Value::Class(RoxClass {
                name: class_declaration.name,
                methods,
                fields,
            }),
        );
        Ok(())
    }

    fn visit_if_statement(&mut self, if_statement: IfStatement) -> Result<(), RunTimeError> {
        if if_statement.expression.accept(self)?.is_truthy() {
            if_statement.statement.accept(self)?;
        } else {
            match if_statement.else_statement {
                Some(stmt) => {
                    stmt.accept(self)?;
                }
                None => {}
            };
        }
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_dec: FunctionDeclaration,
    ) -> Result<(), RunTimeError> {
        self.environment.define(
            func_dec.name,
            Value::Callable(Callable::from_node(
                func_dec.params,
                *func_dec.body,
                self.environment.clone(),
            )),
        );
        Ok(())
    }

    fn visit_return_statement(&mut self, return_stmt: ReturnStatement) -> Result<(), RunTimeError> {
        let value = match return_stmt.return_expression {
            Some(vl) => vl.accept(self)?,
            None => Value::Nil,
        };
        self.ret_val = value;
        Err(RunTimeError {
            message: "".to_string(),
            from: self.location.from,
            to: self.location.to,
            return_error: true,
            loop_break: false,
        })
    }

    fn visit_while_statement(
        &mut self,
        while_statement: WhileStatement,
    ) -> Result<(), RunTimeError> {
        loop {
            let expr = while_statement.expression.clone();
            let stmt = while_statement.statement.clone();
            let cond = expr.accept(self)?;
            if !cond.is_truthy() {
                break;
            }
            match stmt.accept(self) {
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
        decl: VariableDeclarationStatement,
    ) -> Result<(), RunTimeError> {
        let init = { decl.initializer.accept(self)? };
        self.environment.define(decl.name, init);
        Ok(())
    }

    fn visit_block_statement(&mut self, decl: BlockStatement) -> Result<(), RunTimeError> {
        let copy = self.environment.clone();
        let mut child_env = self.environment.extend();
        self.environment = child_env;
        for mut stmt in decl.statements {
            stmt.accept(self)?;
        }
        self.environment = copy;
        return Ok(());
    }

    fn visit_statement_expression(
        &mut self,
        expr: ExpressionStatement,
    ) -> Result<(), RunTimeError> {
        expr.expression.accept(self)?;
        return Ok(());
    }

    fn visit_print_statement(&mut self, expr: PrintStatement) -> Result<(), RunTimeError> {
        println!("{}", expr.expression.accept(self)?);
        return Ok(());
    }
}

impl OwnedVisitor<Value, RunTimeError> for Interpreter {
    fn set_current_location(&mut self, location: Location) {
        self.location = location;
    }

    fn visit_this_expression(&mut self, expr: ThisExpression) -> Result<Value, RunTimeError> {
        let dist = self.locals.get(&expr.keyword.get_variable());
        let val = if dist.is_some() {
            self.environment.get_at(&"this".to_string(), dist.unwrap())
        } else {
            self.environment.get(&"this".to_string())
        };

        match val {
            Ok(vl) => Ok(vl),
            Err(e) => Err(RunTimeError {
                message: e.message,
                loop_break: false,
                return_error: false,
                from: self.location.from,
                to: self.location.to,
            }),
        }
    }

    fn visit_call_expression(&mut self, expr: CallExpression) -> Result<Value, RunTimeError> {
        let callable = expr.callee.accept(self)?;
        if let Value::Callable(mut callable) = callable {
            let mut args = Vec::new();
            for arg in expr.arguments {
                args.push(arg.accept(self)?);
            }

            if args.len() != callable.arity as usize {
                return Err(RunTimeError {
                    message: format!(
                        "Expected {} arguments, but found {}",
                        callable.arity,
                        args.len()
                    ),
                    return_error: false,
                    loop_break: false,
                    from: self.location.from,
                    to: self.location.to,
                });
            }

            return Ok(callable.call(self, args)?);
        }
        if let Value::Class(class) = callable {
            let mut args = Vec::new();
            for arg in expr.arguments {
                args.push(arg.accept(self)?);
            }
            return Ok(class.constructor(self, args)?);
        }
        return Err(RunTimeError {
            message: "Expression not callable".to_string(),
            return_error: false,
            loop_break: false,
            from: self.location.from,
            to: self.location.to,
        });
    }

    fn visit_member_expression(&mut self, expr: MemberExpression) -> Result<Value, RunTimeError> {
        let instance = expr.object.accept(self)?;
        if let Value::Instance(instance) = instance {
            let mut mthd = instance.class.methods.get(&expr.name.get_variable().name);
            if let Some(ref mut method) = mthd {
                let value = method.clone().bind(&instance);
                return Ok(Value::Callable(value));
            }
            let field = instance.class.fields.get(&expr.name.get_variable().name);
            if let Some(value) = field {
                return Ok(value.clone());
            } else {
                return Err(RunTimeError {
                    message: format!("No such field for {}", instance.to_string()),
                    return_error: false,
                    loop_break: false,
                    from: self.location.from,
                    to: self.location.to,
                });
            }
        }

        return Err(RunTimeError {
            message: "Object is not a class".to_string(),
            return_error: false,
            loop_break: false,
            from: self.location.from,
            to: self.location.to,
        });
    }

    fn visit_assignement_expression(
        &mut self,
        expr: AssignmentExpression,
    ) -> Result<Value, RunTimeError> {
        let val = expr.asignee.accept(self)?;
        let dist = self.locals.get(&expr.name.get_variable());
        let rez = if dist.is_some() {
            self.environment
                .assign_at(expr.name.get_variable().name, val, dist.unwrap())
        } else {
            self.environment.assign(expr.name.get_variable().name, val)
        };

        match rez {
            Ok(_) => Ok(Value::Nil),
            Err(e) => Err(RunTimeError {
                message: e.message,
                loop_break: false,
                return_error: false,
                from: self.location.from,
                to: self.location.to,
            }),
        }
    }

    fn visit_binary_expression(&mut self, expr: BinaryExpression) -> Result<Value, RunTimeError> {
        return match expr.operator {
            Operator::Div => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self);
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self);
                Ok(Value::Number(
                    value_left?.get_number(self)? / value_right?.get_number(self)?,
                ))
            }
            Operator::Plus => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(el) => Ok(Value::Number(value_right.get_number(self)? + el)),
                    Value::String(st) => Ok(Value::String(
                        st.add(value_right.get_string(self)?.as_str()),
                    )),
                    _ => Err(RunTimeError {
                        message: "Cannot add 2 values that are not both strings or numbers"
                            .to_string(),
                        return_error: false,
                        loop_break: false,
                        from: self.location.from,
                        to: self.location.to,
                    }),
                }
            }
            Operator::Minus => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self);
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self);
                Ok(Value::Number(
                    value_left?.get_number(self)? - value_right?.get_number(self)?,
                ))
            }
            Operator::Mul => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self);
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self);
                Ok(Value::Number(
                    value_left?.get_number(self)? * value_right?.get_number(self)?,
                ))
            }
            Operator::EQ => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(nr) => Ok(Value::Bool(nr == *value_right.get_number(self)?)),
                    Value::String(str_l) => {
                        Ok(Value::Bool(str_l == *value_right.get_string(self)?))
                    }
                    Value::Bool(vl) => Ok(Value::Bool(vl == *value_right.get_boolean(self)?)),
                    Value::Nil => Ok(Value::Bool(value_right.is_nil())),
                    _ => Ok(Value::Bool(false)),
                }
            }
            Operator::NotEqual => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(nr) => Ok(Value::Bool(nr != *value_right.get_number(self)?)),
                    Value::String(str_l) => {
                        Ok(Value::Bool(str_l != *value_right.get_string(self)?))
                    }
                    Value::Bool(vl) => Ok(Value::Bool(vl != *value_right.get_boolean(self)?)),
                    Value::Nil => Ok(Value::Bool(!value_right.is_nil())),
                    _ => Ok(Value::Bool(false)),
                }
            }
            Operator::GE => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr >= *value_right.get_number(self)?))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false, from: self.location.from, to: self.location.to })
                    }
                }
            }
            Operator::LE => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr <= *value_right.get_number(self)?))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false, from: self.location.from, to: self.location.to })
                    }
                }
            }
            Operator::Less => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr < *value_right.get_number(self)?))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false, from: self.location.from, to: self.location.to })
                    }
                }
            }
            Operator::Greater => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                match value_left {
                    Value::Number(nr) => {
                        Ok(Value::Bool(nr > *value_right.get_number(self)?))
                    },
                    _ => {
                        Err(RunTimeError { message: format!("Cannot compare {value_left} with {value_right}. One of them is not a number"), return_error: false,loop_break: false, from: self.location.from, to: self.location.to })
                    }
                }
            }
            Operator::And => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                if !value_left.is_truthy() {
                    return Ok(value_left);
                }
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                Ok(value_right)
            }
            Operator::Or => {
                let value_left = expr.left.accept::<Self, Value, RunTimeError>(self)?;
                if value_left.is_truthy() {
                    return Ok(value_left);
                }
                let value_right = expr.right.accept::<Self, Value, RunTimeError>(self)?;
                Ok(value_right)
            }
            _ => Err(RunTimeError {
                message: format!(
                    "Operator {:?} is not a valid operator for a binary expression",
                    expr.operator
                ),
                return_error: false,
                loop_break: false,
                from: self.location.from,
                to: self.location.to,
            }),
        };
    }

    fn visit_identifier_expression(&mut self, expr: Identifier) -> Result<Value, RunTimeError> {
        let dist = self.locals.get(&expr.raw_token.get_variable());
        let val = if dist.is_some() {
            self.environment.get_at(&expr.name, dist.unwrap())
        } else {
            self.environment.get(&expr.name)
        };
        match val {
            Ok(vl) => Ok(vl),
            Err(e) => Err(RunTimeError {
                message: e.message,
                loop_break: false,
                return_error: false,
                from: self.location.from,
                to: self.location.to,
            }),
        }
    }

    fn visit_unary_expression(&mut self, expr: UnaryExpression) -> Result<Value, RunTimeError> {
        return match expr.operator {
            Operator::Not => Ok(Value::Bool(
                !expr
                    .expression
                    .accept::<Self, Value, RunTimeError>(self)?
                    .is_truthy(),
            )),
            Operator::Minus => {
                let value = expr.expression.accept::<Self, Value, RunTimeError>(self)?;
                Ok(Value::Number(-*value.get_number(self)?))
            }
            _ => Err(RunTimeError {
                message: format!(
                    "Operator {:?} is not a valid operator for a unary expression",
                    expr.operator
                ),
                return_error: false,
                loop_break: false,
                from: self.location.from,
                to: self.location.to,
            }),
        };
    }

    fn visit_group_expression(&mut self, expr: GroupExpression) -> Result<Value, RunTimeError> {
        expr.expression.accept(self)
    }

    fn visit_nil_literal(&self) -> Value {
        Value::Nil
    }

    fn visit_string_literal(&self, expr: String) -> Value {
        Value::String(expr)
    }

    fn visit_number_literal(&self, expr: f32) -> Value {
        Value::Number(expr)
    }

    fn visit_bool_literal(&self, expr: bool) -> Value {
        Value::Bool(expr)
    }
}
