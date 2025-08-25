use std::error::Error;

use itertools::Itertools;
use regex::Regex;

pub fn do_first_pass(contents: &str) -> Result<Vec<String>, Box<dyn Error>> {
    // this is used to check if an instance of "<" is the beginning of a header filename.
    let include_regex = Regex::new(r"^\s*#\s*include\s*$").unwrap();

    let mut out = Vec::new();
    let mut in_a_string = false;
    let mut backslash_was_escaped = true;
    let mut in_angle_bracket_string = false;
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
            if !in_comment && !backslash_was_escaped && next_character == '\"' && character != '\\'
            {
                in_a_string = !in_a_string;
            }

            match (character, next_character) {
                ('/', '/') if !in_a_string && !in_angle_bracket_string => {
                    line_out += " ";
                    // skip the rest of the line
                    break;
                }
                ('/', '*') if !in_a_string && !in_angle_bracket_string => {
                    in_comment = true;
                    // don't scan the next 2 characters to avoid /*/ comment case
                    character_windows.next();
                    continue;
                }
                ('*', '/') if in_comment && !in_a_string && !in_angle_bracket_string => {
                    // comment blocks are replaced with a space (to prevent tokens smushing together)
                    line_out += " ";
                    in_comment = false;
                    character_windows.next();
                    continue;
                }
                ('<', '>') => {}
                ('<', _) if include_regex.is_match(&line_out) => {
                    in_angle_bracket_string = true;
                }
                (_, '>') if in_angle_bracket_string => {
                    in_angle_bracket_string = false;
                }
                // Deal with some tricky (evil) edge cases to verify if a \" is really an escaped
                // quote or just a part of \\" with is not an escaped quote
                //
                // This alters back and forth about accepting the \ as a real backslash or not
                ('\\', '\\') if in_a_string && !in_comment && !backslash_was_escaped => {
                    backslash_was_escaped = true;
                }
                (_, '\\') if backslash_was_escaped && !in_comment && in_a_string => {
                    backslash_was_escaped = false;
                }
                _ => {}
            }
            if !in_comment {
                // print!("{}", character);
                line_out += character.to_string().as_str();
                if character_windows.peek().is_none() {
                    // print!("{}", next_character);
                    line_out += next_character.to_string().as_str();
                }
            }
        }

        // clear lines that are only spaces
        // if line_out.chars().all(|c| c.is_whitespace()) {
        //     line_out = "".to_string();
        // };
        // println!();

        // remove trailing spaces
        out.push(line_out.trim_end().to_string());
    }

    Ok(out)
}
