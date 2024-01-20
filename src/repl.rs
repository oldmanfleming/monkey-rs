use std::io::{self, Write};

use monkey_rs::{new_compiler, new_interpreter};

use crate::EngineType;

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

pub fn start(engine_type: EngineType) {
    let mut engine = match engine_type {
        EngineType::Interpreter => new_interpreter(),
        EngineType::Compiler => new_compiler(),
    };

    loop {
        print!(">>");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        match engine.run(&input) {
            Ok(output) => println!("{}", output),
            Err(err) => {
                println!("{}", MONKEY_FACE);
                println!("Woops! We ran into some monkey business here!");
                println!("eval error: {}", err);
                continue;
            }
        }
    }
}
