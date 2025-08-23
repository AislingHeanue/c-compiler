use std::{error::Error, fs};

use first_pass::do_first_pass;
use interpreter::interpret;
use lexer::lex;

mod first_pass;
mod interpreter;
mod lexer;

pub struct PreprocessorConfig {}

pub fn preprocess(
    filename: &str,
    preprocessed_filename: &str,
    _config: PreprocessorConfig,
) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(filename)?;

    let after_first_pass = do_first_pass(&contents)?;

    let lexed = lex(after_first_pass)?;

    let code = interpret(lexed)?;

    // println!("writing to {}", asm_filename);
    fs::write(preprocessed_filename, &code)?;

    Ok(())
}
