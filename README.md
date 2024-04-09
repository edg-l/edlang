# edlang

An experimental statically-typed compiled programming language made with LLVM and Rust.

Syntax is subject to change any time right now. It has a rusty style for now.

```rust
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

mod hello {
  pub fn world(ptr: *const u8) -> u8 {
    return *ptr;
  }
}
```

## edb: The edlang builder

`edb` is a tool like cargo but for edlang:

```
edlang builder

Usage: edlang <COMMAND>

Commands:
  new    Initialize a project
  build  Build a project
  help   Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

## Dependencies

- Rust
- LLVM 18
