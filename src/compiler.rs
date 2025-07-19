use codegen::codegen;
use lexer::lex;
use parser::parse;
use std::{error::Error, fs};

pub mod codegen;
pub mod lexer;
pub mod parser;

#[derive(Clone, Debug)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    SemiColon,
    Keyword(Keyword),
    Identifier(String),
    Constant(Type),
}

#[derive(Clone, Debug)]
pub enum Type {
    Integer(i32),
}

#[derive(Clone, Debug)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

pub fn compile(
    filename: &str,
    asm_filename: &str,
    only_lex: bool,
    only_parse: bool,
    only_codegen: bool,
) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(filename)?;

    let lexed = lex(&contents)?;
    if only_lex {
        return Ok(());
    }

    let parsed = parse(lexed)?;
    if only_parse {
        return Ok(());
    }

    let code = codegen(parsed)?;
    if only_codegen {
        return Ok(());
    }

    fs::write(asm_filename, code)?;

    Ok(())
}
