mod repl;

use anyhow::{Context, Result};
use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use monkey_rs::{new_compiler, new_interpreter};
use std::{fs, path::PathBuf};

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
        Some(engine) => engine,
        None => EngineType::Compiler,
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

fn execute_file(path: PathBuf, engine_type: EngineType) -> Result<()> {
    let input = fs::read_to_string(&path).context(format!("Failed to read {}", path.display()))?;

    let mut engine = match engine_type {
        EngineType::Interpreter => new_interpreter(),
        EngineType::Compiler => new_compiler(),
    };

    engine.run(&input)?;

    Ok(())
}
