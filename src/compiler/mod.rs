use birds::{birds, optimize};
use codegen::codegen;
use lexer::lex;
use parser::{parse, validate};
use std::{error::Error, fs};

use crate::CompileConfig;

pub mod birds;
pub mod codegen;
pub mod flow_graph;
pub mod lexer;
pub mod parser;
pub mod types;

static IS_LINUX: bool = true;
static IS_MAC: bool = false;

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

    let mut parsed = parse(lexed, config.only_parse)?;
    if config.only_parse {
        println!("{}", parsed);
        return Ok(());
    }

    let (symbols, structs) = validate(&mut parsed)?;
    if config.only_validate {
        println!("{}", parsed);
        return Ok(());
    }

    let (birds_output, symbols, structs) =
        birds(parsed, config.ignore_stack_gaps, symbols, structs)?;

    let (optimized, symbols, structs) =
        optimize(birds_output, symbols, structs, config.optimize_config)?;
    if config.only_birds {
        println!("{:#?}", optimized);
        return Ok(());
    }

    let code = codegen(
        optimized,
        config.add_comments,
        IS_LINUX,
        IS_MAC,
        symbols,
        structs,
    )?;
    if config.only_codegen {
        return Ok(());
    }

    // println!("writing to {}", asm_filename);
    fs::write(asm_filename, code.to_string())?;

    Ok(())
}
