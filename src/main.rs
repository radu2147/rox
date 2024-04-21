#[macro_use]
extern crate macro_errors;
mod ast_types;
mod environment;
mod errors;
mod expression_visitor;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod statement_visitor;
mod stmt;
mod types;

use crate::environment::Environment;
use crate::errors::InterpreterError;
use crate::interpreter::{Callable, Interpreter, Value};
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use crate::types::Location;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::Write;
use std::time::{SystemTime, UNIX_EPOCH};

fn run(code: String, env: Environment) {
    let copy_code = code.clone();
    let mut scanner = Scanner::new(code.clone());
    let mut parser;
    match scanner.scan_tokens() {
        Ok(tokens) => {
            parser = Parser::new(tokens);
        }
        Err(e) => {
            log_error!(copy_code, e, e.line);
            return;
        }
    };
    let mut interpreter = Interpreter {
        ret_val: Value::Nil,
        locals: HashMap::new(),
        location: Location { from: 0, to: 0 },
        environment: env,
    };
    let mut resolver = Resolver::new(&mut interpreter);
    match parser.parse_program() {
        Ok(mut t) => match resolver.resolve(&mut t) {
            Ok(()) => match interpreter.interpret(t) {
                Ok(_) => {}
                Err(e) => {
                    log_error!(copy_code, e, e.from, e.to);
                }
            },
            Err(e) => {
                log_error!(copy_code, e, e.from, e.to);
            }
        },
        Err(er) => {
            log_error!(copy_code, er, er.line);
        }
    }
}

fn run_file(filename: &String, env: Environment) {
    let contents = fs::read_to_string(filename).expect("Should have been able to read the file");
    run(contents, env);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut environment = Environment::new();
    environment.define(
        "clock".to_string(),
        Value::Callable(Callable {
            arity: 0,
            params: vec![],
            body: None,
            std_call: Some(|_int, _args| {
                return Ok(Value::Number(
                    SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f32(),
                ));
            }),
            closure: environment.clone(),
        }),
    );
    run_file(&args[1], environment);
}
