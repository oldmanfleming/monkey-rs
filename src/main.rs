mod lexer;
mod repl;
mod token;

use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// run a file
    Run {
        #[arg(name = "FILE")]
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Some(Commands::Run { file }) => {
            println!("Running file: {:?}", file);
        }
        None => {
            repl::start();
        }
    }
}
