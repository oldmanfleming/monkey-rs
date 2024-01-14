<p align="center">
  <img src="https://interpreterbook.com/img/monkey_logo-d5171d15.png" width="200" height="200" />
</p>

# monkey-rs

A rust implementation of the monkey language from the book
[Writing an Interpreter in Go](https://interpreterbook.com/)

## Getting Started

### Run the REPL

```
$ cargo run
>> "Hello, " + "world!"
Hello, world!
>> let x = 5
 null
>> let add = fn (y) { x + y }
null
>> add(5)
10
```

### Run a script

```
$ cargo build --release
$ echo 'print("Hello, world!")' > hello.mk
$ ./target/release/monkey-rs run hello.mk
Hello, world!
```

### Examples

See the examples directory for further examples
