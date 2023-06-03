# edlang

A toy language I'm making to learn LLVM and compilers.

Syntax is subject to change any time right now. It has a rusty style for now.

```
struct Hello {
    x: i32,
    y: i32,
}

fn test(x: Hello) {
    return;
}

fn works(x: i64) -> i64 {
    let z = 0;
    if 2 == x {
        z = x * 2;
    } else {
        z = x * 3;
    }
    return z;
}

fn main() -> i64 {
    let y: i64 = 2;
    let z = y;
    return works(z);
}

```
