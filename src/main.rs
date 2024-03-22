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
use crate::interpreter::{Callable, Interpreter, Value};
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::Write;
use std::process::exit;
use std::time::{SystemTime, UNIX_EPOCH};

fn run(code: String, error_handler: &mut ErrorHandler, env: &mut Environment) {
    let mut scanner = Scanner::new(code, error_handler);
    let mut parser = Parser::new(scanner.scan_tokens(), error_handler);
    let mut interpreter = Interpreter {
        ret_val: Value::Nil,
        locals: HashMap::new(),
    };
    let mut resolver = Resolver::new(&mut interpreter);
    match parser.parse_program() {
        Ok(mut t) => {
            resolver.resolve(env, &mut t);
            match interpreter.interpret(&mut t, env) {
                Ok(_) => {}
                Err(e) => {
                    println!("{:?}", e);
                }
            }
        }
        Err(e) => println!("{:?}", e),
    }
}

struct ErrorHandler {
    had_error: bool,
}

impl ErrorHandler {
    fn error(&mut self, line: u32, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: u32, location: &str, message: &str) {
        println!("[line {line}] Error {location}: {message}");
        self.had_error = true;
    }
}

fn run_prompt(error_handler: &mut ErrorHandler, env: &mut Environment) {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        let length = std::io::stdin().read_line(&mut line).unwrap();
        if length > 0 && line != "".to_string() {
            run(line, error_handler, env);
            if error_handler.had_error {
                exit(65);
            }
        }
    }
}

fn run_file(filename: &String, err: &mut ErrorHandler, env: &mut Environment) {
    let contents = fs::read_to_string(filename).expect("Should have been able to read the file");

    run(contents, err, env);
    if err.had_error {
        exit(65);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut err = ErrorHandler { had_error: false };
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
    match args.len() {
        1 => run_prompt(&mut err, &mut environment),
        2 => run_file(&args[1], &mut err, &mut environment),
        _ => panic!("Wrong use of the interpreter"),
    }
}
