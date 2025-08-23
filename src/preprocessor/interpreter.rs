use std::error::Error;

use super::lexer::Tokens;

pub fn interpret(tokens: Tokens) -> Result<String, Box<dyn Error>> {
    Ok(format!("{}", tokens))
}
