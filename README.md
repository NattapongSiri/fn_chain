# fn_trait library
This trait provide helper function and macro called `chain` to ease chaining of function where output of first function piped directly as input to another function and so forth.

## Use case 1 - Macro `chain`
```rust
fn simple_add(a : i32, b : i32, c : i32) -> i32 {
    a + b + c
}

fn pass_through(v : f64) -> f64 {
    v
}

assert_eq!(
    6f64, 
    chain!(
        simple_add(1, 2, 3), 
        |result: i32| {(result as f64).powi(2)}, 
        |sqr: f64| {sqr.powf(0.5)},
        pass_through,
        pass_through
    )
);
```

## Use case 2 - function `chain`
```rust
use fn_chain::chain;

fn simple_add(a : i32, b : i32, c : i32) -> i32 {
    a + b + c
}

fn pass_through(v : f64) -> f64 {
    v
}

assert_eq!(6f64, *chain(simple_add(1, 2, 3))
                        .chain(|result| {(result as f64).powi(2)})
                        .chain(|sqr| sqr.powf(0.5))
                        .chain(pass_through)
                        .chain(pass_through));
```