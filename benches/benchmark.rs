use criterion::{black_box, criterion_group, criterion_main, Criterion};
use monkey_rs::{new_compiler, new_interpreter};

const INPUT: &str = r#"
let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};

fibonacci(20);
"#;

fn fib_benchmark(c: &mut Criterion) {
    c.bench_function("interpteter", |b| {
        b.iter(|| {
            let mut engine = new_interpreter();
            engine.run(&black_box(INPUT)).unwrap();
        })
    });

    c.bench_function("compiler", |b| {
        b.iter(|| {
            let mut engine = new_compiler();
            engine.run(&black_box(INPUT)).unwrap();
        })
    });
}

criterion_group!(benches, fib_benchmark);
criterion_main!(benches);
