use std::io::{self, Write};

use monkey_rs::{Compiler, Evaluator, Lexer, Parser, VirtualMachine};

use crate::Engine;

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

pub fn start(engine: Engine) {
    let mut evaluator = Evaluator::new();
    let mut vm = VirtualMachine::new();

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

        let result = match engine {
            Engine::Interpreter => evaluator.eval(program).map(|obj| obj.to_string()),
            Engine::Compiler => {
                let mut compiler = Compiler::new();

                match compiler.compile(program) {
                    Ok(_) => {}
                    Err(err) => {
                        println!("{}", MONKEY_FACE);
                        println!("Woops! We ran into some monkey business here!");
                        println!("compile error: {}", err);
                        continue;
                    }
                }

                let bytecode = compiler.bytecode();

                vm.run(bytecode).map(|obj| obj.to_string())
            }
        };

        match result {
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
