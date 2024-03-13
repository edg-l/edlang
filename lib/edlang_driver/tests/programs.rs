use crate::common::{compile_program, run_program};
use test_case::test_case;

mod common;

#[test_case(include_str!("programs/simple.ed"), "simple", false, 0, &["1"] ; "simple.ed 1")]
#[test_case(include_str!("programs/simple.ed"), "simple", false, 1, &["a", "b"] ; "simple.ed 3")]
#[test_case(include_str!("programs/basic_ifs.ed"), "basic_ifs", false, 9, &[] ; "basic_ifs")]
#[test_case(include_str!("programs/while.ed"), "while", false, 10, &[] ; "r#while")]
#[test_case(include_str!("programs/factorial.ed"), "factorial", false, 24, &[] ; "factorial")]
#[test_case(include_str!("programs/refs.ed"), "refs", false, 2, &[] ; "refs")]
#[test_case(include_str!("programs/struct.ed"), "struct", false, 5, &[] ; "r#struct")]
#[test_case(include_str!("programs/casts.ed"), "casts", false, 2, &[] ; "casts")]
#[test_case(TEST_ADD, "test_add", false, 2, &[] ; "test_add")]
#[test_case(TEST_SUB, "test_sub", false, 1, &[] ; "test_sub")]
#[test_case(TEST_MUL, "test_mul", false, 4, &[] ; "TEST_MUL")]
#[test_case(TEST_DIV, "test_div", false, 2, &[] ; "TEST_DIV")]
#[test_case(TEST_REM, "test_rem", false, 0, &[] ; "TEST_REM")]
#[test_case(TEST_IF_BOTH, "test_if_both", false, 1, &[] ; "test_if_both")]
#[test_case(TEST_IF_BOTH, "test_if_both", false, 2, &["a"] ; "test_if_both_args")]
#[test_case(TEST_IF_NO_ELSE, "test_if_no_else", false, 1, &[] ; "test_if_no_else")]
#[test_case(TEST_IF_NO_ELSE, "test_if_no_else", false, 2, &["a"] ; "test_if_no_else_args")]
fn example_tests(source: &str, name: &str, is_library: bool, status_code: i32, args: &[&str]) {
    let program = compile_program(source, name, is_library).unwrap();

    dbg!(&program);
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

    pub fn main() -> i32 {
        let b: i32 = 1 + 1;
        return b;
    }

"#;
const TEST_SUB: &str = r#"

    pub fn main() -> i32 {
        let b: i32 = 2 - 1;
        return b;
    }

"#;
const TEST_MUL: &str = r#"

    pub fn main() -> i32 {
        let b: i32 = 2 * 2;
        return b;
    }

"#;
const TEST_DIV: &str = r#"

    pub fn main() -> i32 {
        let b: i32 = 4 / 2;
        return b;
    }

"#;
const TEST_REM: &str = r#"

    pub fn main() -> i32 {
        let b: i32 = 4 % 2;
        return b;
    }

"#;
const TEST_IF_BOTH: &str = r#"

    pub fn main(argc: i32) -> i32 {
        if argc == 1 {
            return 1;
        } else {
            return 2;
        }
    }

"#;
const TEST_IF_NO_ELSE: &str = r#"

    pub fn main(argc: i32) -> i32 {
        if argc == 1 {
            return 1;
        }
        return 2;
    }

"#;
