use anyhow::{Context, Result};
use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use monkey_rs::{new_compiler, new_interpreter, Engine};
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    #[arg(short, long, value_enum)]
    engine: Option<EngineType>,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        #[arg(name = "FILE")]
        path: PathBuf,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, ValueEnum)]
enum EngineType {
    Interpreter,
    Compiler,
}

fn main() {
    let cli = Cli::parse();

    let engine = match cli.engine {
        Some(engine_type) => match engine_type {
            EngineType::Interpreter => new_interpreter(),
            EngineType::Compiler => new_compiler(),
        },
        None => new_compiler(),
    };

    match cli.command {
        Some(Commands::Run { path }) => {
            execute_file(path, engine).unwrap_or_else(|err| {
                println!("{}", err);
                std::process::exit(1);
            });
        }
        None => {
            start_repl(engine);
        }
    }
}

fn execute_file(path: PathBuf, mut engine: Box<dyn Engine>) -> Result<()> {
    let input = fs::read_to_string(&path).context(format!("Failed to read {}", path.display()))?;

    engine.run(&input)?;

    Ok(())
}

fn start_repl(mut engine: Box<dyn Engine>) {
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
