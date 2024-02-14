use crate::common::{compile_program, run_program};
use test_case::test_case;

mod common;

#[test_case(include_str!("programs/simple.ed"), "simple", false, 0, &["1"] ; "simple.ed 1")]
#[test_case(include_str!("programs/simple.ed"), "simple", false, 1, &["a", "b"] ; "simple.ed 3")]
#[test_case(include_str!("programs/basic_ifs.ed"), "basic_ifs", false, 9, &[] ; "basic_ifs")]
fn example_tests(source: &str, name: &str, is_library: bool, status_code: i32, args: &[&str]) {
    let program = compile_program(source, name, is_library).unwrap();

    assert!(program.binary_file.exists(), "program not compiled");

    let result = run_program(&program.binary_file, args).unwrap();
    assert_eq!(
        result.status.code().unwrap(),
        status_code,
        "Program {} returned a unexpected status code",
        name
    );
}
