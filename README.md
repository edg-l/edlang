# edlang

An experimental statically-typed compiled programming language made with LLVM and Rust.

Syntax is subject to change any time right now. It has a rusty style for now.

```rust
mod Main {
    pub fn main() -> i32 {
        let b: i32 = factorial(4);
        return b;
    }

    pub fn factorial(n: i32) -> i32 {
        if n == 1 {
            return n;
        } else {
            return n * factorial(n - 1);
        }
    }
}
```

## Dependencies

- Rust
- LLVM 17
