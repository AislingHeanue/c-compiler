use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::VecDeque, error::Error, fmt::Display};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Debug, PartialEq, EnumIter)]
pub enum PreprocessorToken {
    Identifier(String),

    Number(String),

    CharacterConstant(i8),
    StringLiteral(Vec<i8>),
    HeaderFilename(String),
    HeaderFilenameAngleBrackets(String),

    Punctuator(String),
    WhiteSpace(String),

    KeywordDefined,

    DirectiveDefine,
    DirectiveInclude,
    DirectiveIfdef,
    DirectiveIfndef,
    DirectiveIf,
    DirectiveElse,
    DirectiveElif,
    DirectiveUndef,
    DirectiveLine,
    DirectiveError,
    DirectivePragma,
    DirectiveWarning,
}

pub struct Tokens(Vec<VecDeque<PreprocessorToken>>);

lazy_static! {
    // format is (Token, Regex, Precedence). Higher precedence regexes are taken over lower ones.
    // Needed for keywords vs identifiers.
    static ref ALL_TOKENS: Vec<(PreprocessorToken, Regex)> = {
        let mut v = Vec::new();
        for token in PreprocessorToken::iter() {
            let entry:&str = match token {
                PreprocessorToken::Identifier(_) => r"^[a-zA-Z_]\w*\b",
                PreprocessorToken::Number(_) => r#"\.?[0-9][a-zA-Z0-9(?:[eEpP][+-])]*"#,
                PreprocessorToken::CharacterConstant(_) => r#"'(?:[^'\\\n]|\\['"?\\abfnrtv])'"#,
                PreprocessorToken::StringLiteral(_) => r#""(?:[^"\\\n]|\\['"?\\abfnrtv])*""#,
                PreprocessorToken::HeaderFilename(_) => r#""([^"\n]*)""#,
                PreprocessorToken::HeaderFilenameAngleBrackets(_) => r#"<([^"\n]*)>"#,
                PreprocessorToken::KeywordDefined => r"defined\b",

                // every punctuation character in ASCII except `, \, $ and @
                // maybe # should also be moved out of here (preprocessor needs if for
                // concatenation)
                PreprocessorToken::Punctuator(_) => r#"[!"%#&'\(\)\*\+,\-\./:;<=>\[\]\?\^_\{\|\}~]"#,
                PreprocessorToken::WhiteSpace(_) => r#"\s+"#,

                PreprocessorToken::DirectiveDefine => r"define\b",
                PreprocessorToken::DirectiveInclude => r"#\s*include\b",
                PreprocessorToken::DirectiveIfdef => r"#\s*ifdef\b",
                PreprocessorToken::DirectiveIfndef => r"#\s*ifndef\b",
                PreprocessorToken::DirectiveIf => r"#\s*if\b",
                PreprocessorToken::DirectiveElse => r"#\s*else\b",
                PreprocessorToken::DirectiveElif => r"#\s*elif\b",
                PreprocessorToken::DirectiveUndef => r"#\s*undef\b",
                PreprocessorToken::DirectiveLine => r"#\s*line\b",
                PreprocessorToken::DirectiveError => r"#\s*error\b",
                PreprocessorToken::DirectivePragma => r"#\s*pragma\b",
                PreprocessorToken::DirectiveWarning => r"#\s*warning\b",
            };
            if !entry.is_empty() {
                let entry = "^".to_string() + entry;
                v.push((token, Regex::new(&entry).unwrap()))
            }
        }
        v
    };
}

impl PreprocessorToken {
    fn instantiate(&self, text: &str) -> PreprocessorToken {
        // fill in values for identifiers and integer constants, since they need to read the actual
        // contents of the matching field
        match self {
            PreprocessorToken::Identifier(_) => PreprocessorToken::Identifier(text.to_string()),
            PreprocessorToken::Number(_) => PreprocessorToken::Number(text.to_string()),
            PreprocessorToken::CharacterConstant(_) => {
                // strip first and last characters (')
                PreprocessorToken::CharacterConstant(PreprocessorToken::parse_character(
                    &text[1..text.len() - 1],
                ))
            }
            PreprocessorToken::StringLiteral(_) => PreprocessorToken::StringLiteral(
                PreprocessorToken::parse_string(text[1..text.len() - 1].to_string()),
            ),
            PreprocessorToken::HeaderFilename(_) => {
                PreprocessorToken::HeaderFilename(text.to_string())
            }
            PreprocessorToken::HeaderFilenameAngleBrackets(_) => {
                PreprocessorToken::HeaderFilenameAngleBrackets(text.to_string())
            }
            PreprocessorToken::Punctuator(_) => PreprocessorToken::Punctuator(text.to_string()),
            PreprocessorToken::WhiteSpace(_) => PreprocessorToken::WhiteSpace(text.to_string()),

            _ => self.clone(),
        }
    }

    fn parse_character(text: &str) -> i8 {
        let code: u64 = match text {
            _ if text.len() == 1 => text.chars().next().unwrap().into(),
            r"\'" => '\''.into(),
            r#"\""# => '\"'.into(),
            r"\?" => '?'.into(),
            r"\\" => '\\'.into(),
            r"\a" => 7,  // audible bell
            r"\b" => 8,  // backspace
            r"\f" => 12, // form feed
            r"\n" => '\n'.into(),
            r"\r" => '\r'.into(),
            r"\t" => '\t'.into(),
            r"\v" => 11, //vertical tab
            _ => panic!("Char must be a valid ASCII 7-bit value"),
        };
        code as i8
    }

    fn display_character(c: &i8) -> String {
        char::from_u32(*c as u32)
            .map(|c| match c {
                '\n' => r"\n".to_string(),
                '\r' => r"\r".to_string(),
                '\\' => r"\\".to_string(),
                '\"' => r#"\""#.to_string(),
                _ => match c as u32 {
                    7 => r"\a".to_string(),
                    8 => r"\b".to_string(),
                    11 => r"\f".to_string(),
                    12 => r"\v".to_string(),
                    _ => c.to_string(),
                },
            })
            .unwrap_or("NULL".to_string())
    }

    fn parse_string(mut text: String) -> Vec<i8> {
        let mut out = Vec::new();
        while !text.is_empty() {
            if text.starts_with(r"\") {
                let (substring, new_text) = text.split_at(2);
                out.push(Self::parse_character(substring));
                text = new_text.to_string();
            } else {
                let (substring, new_text) = text.split_at(1);
                out.push(Self::parse_character(substring));
                text = new_text.to_string();
            }
        }
        out
    }

    fn find_next(text: &str) -> Result<(PreprocessorToken, usize), Box<dyn Error>> {
        ALL_TOKENS
            .iter()
            .map(|(possible_token, regex)| (possible_token, regex.captures(text)))
            // filter out non-matching patterns
            .filter_map(|(possible_token, maybe_captures)| {
                maybe_captures.map(|captures| {
                    (
                        possible_token.clone(),
                        captures.get(captures.len() - 1).unwrap().end(),
                    ) // read the last capture group
                })
            })
            .max_by(|(token_a, end_a), (token_b, end_b)| {
                end_a.cmp(end_b).then_with(|| {
                    let token_a_precedence: usize =
                        (!matches!(token_a, PreprocessorToken::Identifier(_))).into();
                    let token_b_precedence: usize =
                        (!matches!(token_b, PreprocessorToken::Identifier(_))).into();
                    token_a_precedence.cmp(&token_b_precedence)
                })
            })
            .ok_or::<Box<dyn Error>>(format!("Invalid token detected: {:?}", text).into())
    }
}

pub fn lex(lines: Vec<String>) -> Result<Tokens, Box<dyn Error>> {
    let mut tokens = Vec::new();

    for mut contents in lines.into_iter() {
        let mut tokens_for_line = VecDeque::new();
        // contents = contents.trim_start().into();
        while !contents.is_empty() {
            let (possible_token, end): (PreprocessorToken, usize) =
                PreprocessorToken::find_next(&contents)?;
            let (substring, remainder) = contents.split_at(end);
            let substring = substring.to_string();
            contents = remainder.to_string();

            tokens_for_line.push_back(possible_token.instantiate(&substring));
            // contents = contents.trim_start().to_string();
        }
        tokens.push(tokens_for_line);
    }

    Ok(Tokens(tokens))
}

impl Display for Tokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.0.iter() {
            for token in line.iter() {
                let str = match token {
                    PreprocessorToken::Identifier(ref s) | PreprocessorToken::Number(ref s) => {
                        s.to_string()
                    }
                    PreprocessorToken::CharacterConstant(ref c) => {
                        PreprocessorToken::display_character(c)
                    }
                    PreprocessorToken::StringLiteral(ref cs) => {
                        cs.iter().map(PreprocessorToken::display_character).join("")
                    }
                    PreprocessorToken::Punctuator(ref s) => s.to_string(),
                    PreprocessorToken::WhiteSpace(ref s) => s.to_string(),
                    PreprocessorToken::KeywordDefined => "defined".to_string(),
                    PreprocessorToken::HeaderFilename(_)
                    | PreprocessorToken::HeaderFilenameAngleBrackets(_)
                    | PreprocessorToken::DirectiveDefine
                    | PreprocessorToken::DirectiveInclude
                    | PreprocessorToken::DirectiveIfdef
                    | PreprocessorToken::DirectiveIfndef
                    | PreprocessorToken::DirectiveIf
                    | PreprocessorToken::DirectiveElse
                    | PreprocessorToken::DirectiveElif
                    | PreprocessorToken::DirectiveUndef
                    | PreprocessorToken::DirectiveLine
                    | PreprocessorToken::DirectiveError
                    | PreprocessorToken::DirectivePragma
                    | PreprocessorToken::DirectiveWarning => {
                        panic!("Invalid token in output: {:?}", token);
                    }
                };
                write!(f, "{}", str)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
