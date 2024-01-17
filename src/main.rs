mod repl;

use anyhow::{Context, Result};
use clap::{Parser as ClapParser, Subcommand};
use monkey_rs::{Evaluator, Lexer, Parser};
use std::{fs, path::PathBuf};

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        #[arg(name = "FILE")]
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Some(Commands::Run { path }) => {
            execute_file(path).unwrap_or_else(|err| {
                println!("{}", err);
                std::process::exit(1);
            });
        }
        None => {
            repl::start();
        }
    }
}

fn execute_file(path: PathBuf) -> Result<()> {
    let input = fs::read_to_string(&path).context(format!("Failed to read {}", path.display()))?;

    let lexer = Lexer::new(&input);

    let mut parser = Parser::new(lexer);

    let program = parser.parse_program()?;

    let mut evaluator = Evaluator::new();

    let _ = evaluator.eval(program)?;

    Ok(())
}
