pub trait InterpreterError {
    fn error(&self) -> String;
}

#[derive(Debug, InterpreterError)]
pub struct EnvironmentError {
    pub message: String,
}

#[derive(Debug, InterpreterError)]
pub struct ParseError {
    pub message: String,
    pub line: u128,
}

#[derive(Debug, InterpreterError)]
pub struct RunTimeError {
    pub message: String,
    pub return_error: bool,
    pub loop_break: bool,
    pub from: u128,
    pub to: u128,
}

#[derive(Debug, InterpreterError)]
pub struct ResolverError {
    pub message: String,
    pub from: u128,
    pub to: u128,
}

impl EnvironmentError {
    pub fn new(name: &str) -> Self {
        Self {
            message: format!("Undefined variable {name}").to_string(),
        }
    }
}

#[macro_export]
macro_rules! log_error {
    ($content: expr, $e: expr, $($line:expr),*) => {
        let mut error_zone = String::new();
        let code_lines = $content.split("\n").collect::<Vec<&str>>();
        let message = &$e.message;
        let typ = $e.error();
        let mut lines = vec![];
        $(
            let line: u128 = $line;
            lines.push(line);
        )*
        if lines.len() > 2 || lines.len() == 0 {
            panic!("Lines should only have 2 arguments")
        }
        let from = lines.get(0).unwrap();
        let to = {
            if lines.len() == 2 {
                lines.get(1).unwrap()
            } else {
                from
            }
        };
        let start = {
            if *from > 1 {
                *from - 2
            } else {
                *from
            }
        };

        let end = {
            if *to < code_lines.len() as u128 - 1 {
                *to + 1
            } else {
                code_lines.len() as u128
            }
        };

        for lin in (start - 1)..end {
            if code_lines.len() as u128 > lin {
                error_zone += &format!("{}| {}\n", lin + 1, code_lines[lin as usize]);
            }
        }

        println!("{error_zone}");
        if from == to {
            println!("[line {from}] {typ}: {message}");
        } else {
            println!("[line {from}-{to}] {typ}: {message}");
        }
    };
}
