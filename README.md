<p align="center">
  <img src="https://interpreterbook.com/img/monkey_logo-d5171d15.png" width="200" height="200" />
</p>

# monkey-rs

A rust implementation of the monkey language from the books
[Writing an Interpreter in Go](https://interpreterbook.com/) &
[Writing a Compiler in Go](https://compilerbook.com/)

## Installation

#### Shell script

```
$ curl --proto '=https' --tlsv1.2 -LsSf https://github.com/oldmanfleming/monkey-rs/releases/download/v0.2.0/monkey-rs-installer.sh | sh
```

#### Homebrew

```
$ brew install monkey-rs
```

#### Powershell

```
$ irm https://github.com/oldmanfleming/monkey-rs/releases/download/v0.2.0/monkey-rs-installer.ps1 | iex
```

## Run the REPL

```
$ monkey
>> "Hello, " + "world!"
Hello, world!
>> let x = 5
 null
>> let add = fn (y) { x + y }
null
>> add(5)
10
```

## Run a file

```
$ echo 'print("Hello, world!")' > hello.mk
$ monkey run hello.mk
Hello, world!
```

## Using the Interpreter

This implementation supports both the interpreter and compiler engines from the
respective books. You can switch between engines using the engine flag. By
default the engine used is the compiler, as it benchmarks at roughly 50% faster
for the fibonacci example.

```
monkey -e interpreter
```

## Full CLI Options

```
A Rust implementation of the Monkey programming language.

Usage: monkey [OPTIONS] [COMMAND]

Commands:
  run
  help  Print this message or the help of the given subcommand(s)

Options:
  -e, --engine <ENGINE>  [possible values: interpreter, compiler]
  -h, --help             Print help
  -V, --version          Print version
```

## Examples

See the examples directory for further examples
