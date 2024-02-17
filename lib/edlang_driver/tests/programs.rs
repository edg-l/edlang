use crate::common::{compile_program, run_program};
use test_case::test_case;

mod common;

#[test_case(include_str!("programs/simple.ed"), "simple", false, 0, &["1"] ; "simple.ed 1")]
#[test_case(include_str!("programs/simple.ed"), "simple", false, 1, &["a", "b"] ; "simple.ed 3")]
#[test_case(include_str!("programs/basic_ifs.ed"), "basic_ifs", false, 9, &[] ; "basic_ifs")]
#[test_case(include_str!("programs/while.ed"), "while", false, 10, &[] ; "r#while")]
#[test_case(include_str!("programs/factorial.ed"), "factorial", false, 24, &[] ; "factorial")]
#[test_case(include_str!("programs/refs.ed"), "refs", false, 2, &[] ; "refs")]
#[test_case(TEST_ADD, "TEST_ADD", false, 2, &[] ; "TEST_ADD")]
#[test_case(TEST_SUB, "TEST_SUB", false, 1, &[] ; "TEST_SUB")]
#[test_case(TEST_MUL, "TEST_MUL", false, 4, &[] ; "TEST_MUL")]
#[test_case(TEST_DIV, "TEST_DIV", false, 2, &[] ; "TEST_DIV")]
#[test_case(TEST_REM, "TEST_REM", false, 0, &[] ; "TEST_REM")]
#[test_case(TEST_IF_BOTH, "TEST_IF_BOTH", false, 1, &[] ; "TEST_IF_BOTH")]
#[test_case(TEST_IF_BOTH, "TEST_IF_BOTH", false, 2, &["a"] ; "TEST_IF_BOTH args")]
#[test_case(TEST_IF_NO_ELSE, "TEST_IF_NO_ELSE", false, 1, &[] ; "TEST_IF_NO_ELSE")]
#[test_case(TEST_IF_NO_ELSE, "TEST_IF_NO_ELSE", false, 2, &["a"] ; "TEST_IF_NO_ELSE args")]
fn example_tests(source: &str, name: &str, is_library: bool, status_code: i32, args: &[&str]) {
    dbg!(source);
    let program = compile_program(source, name, is_library).unwrap();

    assert!(program.binary_file.exists(), "program not compiled");
    let mut result = run_program(&program.binary_file, args).unwrap();
    let status = result.wait().unwrap();
    assert_eq!(
        status.code().unwrap(),
        status_code,
        "Program {} returned a unexpected status code",
        name
    );
}

const TEST_ADD: &str = r#"
mod Main {
    pub fn main() -> i32 {
        let b: i32 = 1 + 1;
        return b;
    }
}
"#;
const TEST_SUB: &str = r#"
mod Main {
    pub fn main() -> i32 {
        let b: i32 = 2 - 1;
        return b;
    }
}
"#;
const TEST_MUL: &str = r#"
mod Main {
    pub fn main() -> i32 {
        let b: i32 = 2 * 2;
        return b;
    }
}
"#;
const TEST_DIV: &str = r#"
mod Main {
    pub fn main() -> i32 {
        let b: i32 = 4 / 2;
        return b;
    }
}
"#;
const TEST_REM: &str = r#"
mod Main {
    pub fn main() -> i32 {
        let b: i32 = 4 % 2;
        return b;
    }
}
"#;
const TEST_IF_BOTH: &str = r#"
mod Main {
    pub fn main(argc: i32) -> i32 {
        if argc == 1 {
            return 1;
        } else {
            return 2;
        }
    }
}
"#;
const TEST_IF_NO_ELSE: &str = r#"
mod Main {
    pub fn main(argc: i32) -> i32 {
        if argc == 1 {
            return 1;
        }
        return 2;
    }
}
"#;
