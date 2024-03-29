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

fn run(code: String, env: &mut Environment) {
    let mut error_handler = ErrorHandler {
        had_error: false,
        content: vec![],
    };
    error_handler.set_content(&code);
    let mut scanner = Scanner::new(code.clone(), &error_handler);
    let mut parser = Parser::new(scanner.scan_tokens(), &error_handler);
    let mut interpreter = Interpreter {
        ret_val: Value::Nil,
        locals: HashMap::new(),
    };
    let mut resolver = Resolver::new(&mut interpreter);
    match parser.parse_program() {
        Ok(mut t) => match resolver.resolve(env, &mut t) {
            Ok(()) => match interpreter.interpret(&mut t, env) {
                Ok(_) => {}
                Err(e) => {
                    println!("{:?}", e);
                }
            },
            Err(e) => {
                println!("{:?}", e)
            }
        },
        Err(e) => error_handler.error(e.line, &e.message),
    }
}

struct ErrorHandler<'a> {
    had_error: bool,
    content: Vec<&'a str>,
}

impl<'a> ErrorHandler<'a> {
    fn error(&self, line: u128, message: &str) {
        self.report(line, "", message);
    }

    fn report(&self, line: u128, location: &str, message: &str) {
        let mut error_zone = String::new();
        let start = {
            if line > 1 {
                line - 2
            } else {
                line
            }
        };
        for lin in start..(line + 1) {
            if self.content.len() as u128 > lin {
                error_zone += &self.content[lin as usize];
                error_zone += "\n";
            }
        }
        println!("{error_zone}");
        println!("[line {line}] Error {location}: {message}");
    }

    fn set_content(&mut self, content: &'a String) {
        self.content = content.split("\n").collect::<Vec<&str>>();
    }
}

fn run_prompt(env: &mut Environment) {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        let length = std::io::stdin().read_line(&mut line).unwrap();
        if length > 0 && line != "".to_string() {
            run(line, env);
        }
    }
}

fn run_file(filename: &String, env: &mut Environment) {
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
    match args.len() {
        1 => run_prompt(&mut environment),
        2 => run_file(&args[1], &mut environment),
        _ => panic!("Wrong use of the interpreter"),
    }
}
