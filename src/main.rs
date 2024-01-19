mod repl;

use anyhow::{Context, Result};
use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use monkey_rs::{Compiler, Evaluator, Lexer, Parser, VirtualMachine};
use std::{fs, path::PathBuf};

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    #[arg(short, long, value_enum)]
    engine: Option<Engine>,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        #[arg(name = "FILE")]
        path: PathBuf,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, ValueEnum)]
enum Engine {
    Interpreter,
    Compiler,
}

fn main() {
    let cli = Cli::parse();
    let engine = match cli.engine {
        Some(engine) => engine,
        None => Engine::Compiler,
    };

    match cli.command {
        Some(Commands::Run { path }) => {
            execute_file(path, engine).unwrap_or_else(|err| {
                println!("{}", err);
                std::process::exit(1);
            });
        }
        None => {
            repl::start(engine);
        }
    }
}

fn execute_file(path: PathBuf, engine: Engine) -> Result<()> {
    let input = fs::read_to_string(&path).context(format!("Failed to read {}", path.display()))?;

    let lexer = Lexer::new(&input);

    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;

    match engine {
        Engine::Interpreter => {
            let mut evaluator = Evaluator::new();

            let _ = evaluator.eval(program)?;
        }
        Engine::Compiler => {
            let mut compiler = Compiler::new();

            compiler.compile(program)?;

            let bytecode = compiler.bytecode();

            let mut vm = VirtualMachine::new();

            vm.run(bytecode)?;
        }
    }

    Ok(())
}
