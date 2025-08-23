use std::error::Error;

use itertools::Itertools;

pub fn do_first_pass(contents: &str) -> Result<Vec<String>, Box<dyn Error>> {
    let mut out = Vec::new();
    let mut in_a_string = false;
    let mut in_comment = false;
    let mut lines = contents.lines();

    while let Some(mut line) = lines.next().map(|l| l.to_string()) {
        // join lines that end in \
        while line.ends_with("\\") {
            let next_line = lines.next();
            if let Some(next_line_unwrapped) = next_line {
                // pop the \ character
                line.pop();
                line += next_line_unwrapped;
            } else {
                return Err("Unexpected '\\' at end of file".into());
            }
        }

        let mut line_out = "".to_string();
        let first_char = line.chars().next();
        let chars = line.chars().peekable();
        let mut character_windows = chars.tuple_windows().peekable();
        // one character lines
        if !in_comment && character_windows.peek().is_none() && first_char.is_some() {
            if let Some(c) = first_char {
                line_out += c.to_string().as_str();
            }
        }
        while let Some((character, next_character)) = character_windows.next() {
            if next_character == '\"' && character != '\\' {
                in_a_string = !in_a_string;
            }

            match (character, next_character) {
                ('/', '/') => {
                    line_out += " ";
                    // skip the rest of the line
                    break;
                }
                ('/', '*') => {
                    in_comment = true;
                    // don't scan the next 2 characters to avoid /*/ comment case
                    character_windows.next();
                    continue;
                }
                ('*', '/') if in_comment => {
                    // comment blocks are replaced with a space (to prevent tokens smushing together)
                    line_out += " ";
                    in_comment = false;
                    character_windows.next();
                    continue;
                }
                _ => {}
            }
            if !in_comment {
                line_out += character.to_string().as_str();
                if character_windows.peek().is_none() {
                    line_out += next_character.to_string().as_str();
                }
            }
        }

        // clear lines that are only spaces
        if line_out.chars().all(|c| c.is_whitespace()) {
            line_out = "".to_string();
        };
        out.push(line_out)
    }

    Ok(out)
}
