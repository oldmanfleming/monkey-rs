use std::io::{self, Write};

use crate::lexer::Lexer;

pub fn start() {
    loop {
        print!(">>");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let mut lexer = Lexer::new(&input);
        while let Some(token) = lexer.next_token() {
            println!("{}", token);
        }
    }
}
