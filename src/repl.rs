use std::io::{self, Write};

use crate::{evaluator, lexer::Lexer, object::Environment, parser::Parser};

const MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start() {
    let mut env = Environment::new();

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
                println!("{}", MONKEY_FACE);
                println!("Woops! We ran into some monkey business here!");
                println!("parse error: {}", err);
                continue;
            }
        };

        match evaluator::eval(program, &mut env) {
            Ok(evaluated) => println!("{}", evaluated),
            Err(err) => {
                println!("{}", MONKEY_FACE);
                println!("Woops! We ran into some monkey business here!");
                println!("eval error: {}", err);
                continue;
            }
        }
    }
}
