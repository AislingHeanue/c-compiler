use std::{collections::HashMap, error::Error, fs};

use first_pass::do_first_pass;
use interpreter::{interpret, Macro};
use lexer::lex;

mod first_pass;
mod interpreter;
mod lexer;

pub struct PreprocessorConfig {
    // pub extra_system_directories: Vec<String>,
}

pub fn preprocess(
    filename: &str,
    preprocessed_filename: &str,
    _config: PreprocessorConfig,
) -> Result<(), Box<dyn Error>> {
    let (code, _new_map) = internal_preprocess(filename)?;
    println!("{}", code);

    fs::write(preprocessed_filename, &code)?;

    Ok(())
}

fn internal_preprocess(
    filename: &str,
    // _config: PreprocessorConfig,
) -> Result<(String, HashMap<String, Macro>), Box<dyn Error>> {
    let contents = fs::read_to_string(filename)?;

    let after_first_pass = do_first_pass(&contents)?;

    let lexed = lex(after_first_pass)?;

    interpret(filename.to_string(), lexed)
}
