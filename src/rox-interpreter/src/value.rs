use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use rox_errors::RunTimeError;
use rox_parser::expression::Expression;
use rox_parser::statement::Statement;
use crate::environment::Environment;
use crate::Interpreter;

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
    pub(crate) class: RoxClass,
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