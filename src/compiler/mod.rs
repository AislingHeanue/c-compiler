use birds::birds;
use codegen::codegen;
use lexer::lex;
use parser::{parse, validate};
use std::{error::Error, fs};

use crate::CompileConfig;

pub mod birds;
pub mod codegen;
pub mod lexer;
pub mod parser;

static IS_LINUX: bool = true;
static IS_MAC: bool = false;

trait IndentDisplay {
    fn fmt_indent(&self, indent: usize, comments: bool) -> String;
}

pub fn compile(
    filename: &str,
    asm_filename: &str,
    config: CompileConfig,
) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(filename)?;

    let lexed = lex(&contents)?;
    if config.only_lex {
        println!("{:?}", lexed);
        return Ok(());
    }

    let parsed = parse(lexed, config.only_parse)?;
    if config.only_parse {
        println!("{}", parsed);
        return Ok(());
    }

    let parsed = validate(parsed)?;
    if config.only_validate {
        println!("{}", parsed);
        return Ok(());
    }

    let birds_output = birds(parsed)?;
    if config.only_birds {
        println!("{:?}", birds_output);
        return Ok(());
    }

    let code = codegen(birds_output, config.add_comments, IS_LINUX, IS_MAC)?;
    if config.only_codegen {
        return Ok(());
    }

    // println!("writing to {}", asm_filename);
    fs::write(asm_filename, code.to_string())?;

    Ok(())
}
