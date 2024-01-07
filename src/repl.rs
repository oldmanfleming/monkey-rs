use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

pub fn start() {
    loop {
        print!(">>");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(err) => {
                println!("parse error: {}", err);
                continue;
            }
        };

        println!("{}", program)
    }
}
