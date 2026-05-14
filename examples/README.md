# tulisp examples

## Rust embedding

Each `examples/*.rs` is a self-contained program. Run with
`cargo run --example NAME`.

| example           | shows                                                 |
|-------------------|-------------------------------------------------------|
| `hello`           | minimal `defun` + `eval_string` round-trip            |
| `plist_args`      | `AsPlist!` for keyword-argument conversion            |
| `callbacks`       | `ctx.funcall` / `ctx.map` / `ctx.reduce` calling Lisp |
| `embed_rust_type` | opaque Rust struct round-tripping through Lisp        |

## Lisp scripts

Standalone `.lisp` programs under [`scripts/`](scripts/). Run with
the CLI binary:

```
cargo run -- examples/scripts/fib.lisp
```

| script                | shows                                   |
|-----------------------|-----------------------------------------|
| `fib.lisp`            | naive recursive `fib`                   |
| `fib-fast.lisp`       | iterative `fib` with `while` and `setq` |
| `fizz-buzz.lisp`      | classic, using `mod` and nested `if`    |
| `tail-recursion.lisp` | TCO building a 1M-element list          |
