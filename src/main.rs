use compiler::compile;
use std::{env, fs, process::Command};

mod compiler;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut filename = "".to_string();
    let mut only_lex = false;
    let mut only_parse = false;
    let mut only_codegen = false;
    let mut assembly_out = false;
    for arg in &args[1..] {
        match arg.as_str() {
            "--lex" => only_lex = true,
            "--parse" => only_parse = true,
            "--codegen" => only_codegen = true,
            "-S" => assembly_out = true,
            _ => {
                if !filename.is_empty() {
                    panic!("Unrecognised flag, or more than one filename set")
                }
                filename = arg.clone()
            }
        }
    }
    if filename.is_empty() {
        panic!("must specify a file name")
    }
    let stripped_filename = filename.strip_suffix(".c").unwrap_or(&filename).to_owned();
    let preprocessed_filename = stripped_filename.clone() + ".i";
    let asm_filename = stripped_filename.clone() + ".s";

    println!("Preprocessing...");
    let res = Command::new("gcc")
        .args(["-E", "-P", &filename, "-o", &preprocessed_filename])
        .output();
    if res.is_err() {
        panic!("Preprocessor failed: {:?}", res);
    }

    println!("Compiling...");
    let res = compile(
        &preprocessed_filename,
        &asm_filename,
        only_lex,
        only_parse,
        only_codegen,
    );
    if res.is_err() {
        panic!("Compiler failed: {:?}", res);
    }

    let _ = fs::remove_file(&preprocessed_filename);

    if only_lex || only_parse || only_codegen || assembly_out {
        // a flag signalling an early exit was passed, so exit here without an error
        return;
    }

    println!("Assembling and Linking...");
    let res = Command::new("gcc")
        .args([&asm_filename, "-o", &stripped_filename])
        .output();
    if res.is_err() {
        panic!("Linker failed: {:?}", res);
    }

    let _ = fs::remove_file(&asm_filename);
}
