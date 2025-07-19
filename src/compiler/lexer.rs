use super::Keyword;
use super::Token;
use super::Type;
use regex::Regex;
use std::error::Error;

pub fn lex(mut contents: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let break_character: Regex = Regex::new(r"\W").unwrap();
    let identifier: Regex = Regex::new(r"^[a-zA-Z_]\w*$").unwrap();
    let constant_integer: Regex = Regex::new(r"^[0-9]+$").unwrap();

    let mut tokens: Vec<Token> = Vec::new();

    while !contents.is_empty() {
        contents = contents.trim_start();
        if contents.is_empty() {
            break;
        }

        let substring;
        let mut is_break_character = false;
        if let Some(m) = break_character.find(contents) {
            let start = m.start();
            // println!("start: {}", start);
            if start == 0 {
                // println!("break");
                is_break_character = true;
                let (left, right) = contents.split_at(1); // include the match (just one character)

                // println!("{} + {}", left, right);
                substring = left;
                contents = right;
            } else {
                // println!("not break");
                let (left, right) = contents.split_at(start); // do not include the match

                // println!("{} + {}", left, right);
                substring = left;
                contents = right;
            }
        } else {
            substring = contents;
            contents = "";
        }

        let token = if is_break_character {
            match substring {
                "{" => Token::OpenBrace,
                "}" => Token::CloseBrace,
                "(" => Token::OpenParen,
                ")" => Token::CloseParen,
                ";" => Token::SemiColon,
                _ => return Err(format!("Unrecognised token: {}", substring).into()),
            }
        } else if identifier.is_match(substring) {
            match substring {
                "return" => Token::Keyword(Keyword::Return),
                "void" => Token::Keyword(Keyword::Void),
                "int" => Token::Keyword(Keyword::Int),
                _ => Token::Identifier(substring.to_string()),
            }
        } else if constant_integer.is_match(substring) {
            Token::Constant(Type::Integer(substring.parse()?))
        } else {
            return Err(format!("Unrecognised token character sequence: {}", substring).into());
        };

        tokens.push(token)
    }
    // println!("{:?}", tokens);

    Ok(tokens)
}
