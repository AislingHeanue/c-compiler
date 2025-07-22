use birds::birds;
use codegen::codegen;
use lexer::lex;
use parser::parse;
use std::{error::Error, fs};

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
    only_lex: bool,
    only_parse: bool,
    only_birds: bool,
    only_codegen: bool,
    add_comments: bool,
) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(filename)?;

    let lexed = lex(&contents)?;
    if only_lex {
        println!("{:?}", lexed);
        return Ok(());
    }

    let parsed = parse(lexed)?;
    if only_parse {
        println!("{}", parsed);
        return Ok(());
    }

    let birds_output = birds(parsed)?;
    if only_birds {
        println!("{:?}", birds_output);
        return Ok(());
    }

    let code = codegen(birds_output, add_comments, IS_LINUX, IS_MAC)?;
    if only_codegen {
        return Ok(());
    }

    // println!("writing to {}", asm_filename);
    fs::write(asm_filename, code.to_string())?;

    Ok(())
}
