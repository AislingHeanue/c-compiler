use compiler::compile;
use std::{env, fs, process::Command};

mod compiler;

struct CompileConfig {
    only_lex: bool,
    only_parse: bool,
    only_validate: bool,
    only_birds: bool,
    only_codegen: bool,
    add_comments: bool,
    ignore_stack_gaps: bool,
    optimize_config: OptimizeConfig,
}

#[derive(Debug)]
struct OptimizeConfig {
    fold_constants: bool,
    propagate_copies: bool,
    eliminate_dead_code: bool,
    eliminate_dead_stores: bool,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut filenames = Vec::new();
    let mut asm_input_names = Vec::new();
    let mut linker_args = Vec::new();
    let mut preprocessor_args = Vec::new();
    let mut only_lex = false;
    let mut only_parse = false;
    let mut only_validate = false;
    let mut only_birds = false;
    let mut only_codegen = false;
    let mut assembly_out = false;
    let mut add_comments = false;
    let mut to_object_file = false;
    let mut fold_constants = false;
    let mut propagate_copies = false;
    let mut eliminate_dead_code = false;
    let mut eliminate_dead_stores = false;
    let mut ignore_stack_gaps = false;
    for arg in &args[1..] {
        match arg.as_str() {
            "--lex" => only_lex = true,
            "--parse" => only_parse = true,
            "--validate" => only_validate = true,
            "--tacky" => only_birds = true,
            "--codegen" => only_codegen = true,
            "-S" => assembly_out = true,
            "-s" => assembly_out = true,
            "--comments" => add_comments = true,
            "-c" => to_object_file = true,
            "--fold-constants" => fold_constants = true,
            "--propagate-copies" => propagate_copies = true,
            "--eliminate-unreachable-code" => eliminate_dead_code = true,
            "--eliminate-dead-stores" => eliminate_dead_stores = true,
            "--ignore-stack-gaps" => ignore_stack_gaps = true,
            "--optimize" => {
                fold_constants = true;
                propagate_copies = true;
                eliminate_dead_code = true;
                eliminate_dead_stores = true;
            }

            t if t.len() > 1 && matches!(&t[..2], "-l" | "-L") => linker_args.push(arg.clone()),
            t if t.len() > 1 && matches!(&t[..2], "-i" | "-I") => {
                preprocessor_args.push(arg.clone())
            }
            t if t.len() > 1 && t.ends_with(".s") => asm_input_names.push(arg.clone()),
            t if t.starts_with("-") => panic!("unrecognised flag: {}", t),
            _ => filenames.push(arg.clone()),
        }
    }
    if filenames.is_empty() {
        panic!("must specify a file name")
    }

    let stripped_filename = filenames[0]
        .strip_suffix(".c")
        .unwrap_or(&filenames[0])
        .to_string();
    let object_filename = stripped_filename.clone() + ".o";
    let mut asm_filenames: Vec<String> = Vec::new();
    for filename in filenames {
        let stripped_filename = filename.strip_suffix(".c").unwrap_or(&filename).to_owned();
        let preprocessed_filename = stripped_filename.clone() + ".i";
        let asm_filename = stripped_filename.clone() + ".s";
        asm_filenames.push(asm_filename.clone());
        // println!("Preprocessing...");
        let mut args = vec![
            "-E",
            "-P",
            &filename,
            "-std=c11",
            "-o",
            &preprocessed_filename,
        ];
        args.append(&mut preprocessor_args.iter().map(|s| s.as_str()).collect());
        let res = Command::new("gcc").args(args.clone()).output().unwrap();
        if res.status.code() != Some(0) {
            panic!("Preprocessor failed: with args: '{:?}': {:?}", args, res);
        }
        println!(
            "{}",
            fs::read_to_string(preprocessed_filename.clone()).unwrap()
        );

        // println!("Compiling...");
        let res = compile(
            &preprocessed_filename,
            &asm_filename,
            CompileConfig {
                only_lex,
                only_parse,
                only_validate,
                only_birds,
                only_codegen,
                add_comments,
                ignore_stack_gaps,
                optimize_config: OptimizeConfig {
                    fold_constants,
                    propagate_copies,
                    eliminate_dead_code,
                    eliminate_dead_stores,
                },
            },
        );
        if res.is_err() {
            panic!("Compiler failed: {:?}", res);
        }

        let _ = fs::remove_file(&preprocessed_filename);
    }
    if only_lex || only_parse || only_validate || only_birds || only_codegen {
        // a flag signalling an early exit was passed, so exit here without an error
        return;
    }

    if assembly_out {
        for asm_filename in asm_filenames.iter() {
            println!("{}", fs::read_to_string(asm_filename).unwrap());
        }
        return;
    }

    // println!("Outputting to {}", stripped_filename);
    let res = Command::new("gcc")
        .args(if to_object_file {
            let mut args = vec!["-c", "-o", &object_filename];
            args.append(&mut asm_filenames.iter().map(|s| s.as_str()).collect());
            args.append(&mut asm_input_names.iter().map(|s| s.as_str()).collect());
            args.append(&mut linker_args.iter().map(|s| s.as_str()).collect());
            args
        } else {
            let mut args = vec!["-o", &stripped_filename];
            args.append(&mut asm_filenames.iter().map(|s| s.as_str()).collect());
            args.append(&mut asm_input_names.iter().map(|s| s.as_str()).collect());
            args.append(&mut linker_args.iter().map(|s| s.as_str()).collect());
            args
        })
        .output()
        .unwrap();

    for asm_filename in asm_filenames {
        if res.status.code() != Some(0) {
            // println!("{}", fs::read_to_string(&asm_filename).unwrap());
            let _ = fs::remove_file(&asm_filename);
        }
    }

    if res.status.code() != Some(0) {
        panic!("Linker failed: {:?}", res);
    }
}
