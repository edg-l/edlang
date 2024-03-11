use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    Ok(edlang_driver::main()?)
}
