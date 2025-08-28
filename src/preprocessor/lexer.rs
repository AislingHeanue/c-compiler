use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::{error::Error, fmt::Display};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Debug, PartialEq, EnumIter)]
pub enum PreprocessorToken {
    Identifier(String),

    Number(String),

    CharacterConstant(i8),
    StringLiteral(Vec<i8>),

    Punctuator(String),
    WidePunctuator(String),
    WhiteSpace(String),

    KeywordDefined,

    DirectiveDefine,
    DirectiveUndef,
    // "#include" lexer also parses the <> or "" after it.
    // filename, uses_angle_brackets
    DirectiveInclude(String, bool),
    // if #include is not followed by a < or ", then it must be followed by a macro which needs to
    // be expanded at some point later. Not the "string" or <string> contents of a macro are
    // treated like normal StringLiterals or Punctuation-Identifier-Punctuation etc, which may lead
    // to inconsistencies in some C code.
    DirectiveIncludeWithMacro,
    DirectiveIfdef,
    DirectiveIfndef,
    DirectiveIf,
    DirectiveElse,
    DirectiveElif,
    DirectiveEndif,
    DirectiveLine,
    DirectiveError,
    DirectiveWarning,
    DirectivePragma,
}

#[derive(Debug)]
pub struct Tokens(pub Vec<Vec<PreprocessorToken>>);

lazy_static! {
    // format is (Token, Regex, Precedence). Higher precedence regexes are taken over lower ones.
    // Needed for keywords vs identifiers.
    static ref ALL_TOKENS: Vec<(PreprocessorToken, Regex)> = {
        let mut v = Vec::new();
        for token in PreprocessorToken::iter() {
            let entry:&str = match token {
                PreprocessorToken::Identifier(_) => r"^[a-zA-Z_]\w*\b",
                PreprocessorToken::Number(_) => r#"\.?[0-9](?:[0-9a-zA-Z_\.]|[eEpP][\+\-])*(?:[lL]?[lL]?[uU]?|[uU][lL]?[lL]?)"#,
                PreprocessorToken::CharacterConstant(_) => r#"'(?:[^'\\\n]|\\['"?\\abfnrtv0-9]+)'"#,
                PreprocessorToken::StringLiteral(_) => r#""(?:[^"\\\n]|\\['"?\\abfnrtv])*""#,
                PreprocessorToken::KeywordDefined => r"defined\b",

                // every punctuation character in ASCII except `, \, $ and @
                PreprocessorToken::Punctuator(_) => r#"[!"%#&'\(\)\*\+,\-\./:;<=>\[\]\?\^_\{\|\}~]"#,
                PreprocessorToken::WidePunctuator(_) => r"##|^==|^!=|^>=|^<=|^>>|^<<|^>>=|^<<=|^\->|^\+=|^\-=|^\*=|^/=|^%=|^\&=|^\|=|^\^=|^\+\+|^\-\-|^&&|^\|\|",
                PreprocessorToken::WhiteSpace(_) => r#"[^\S\n]+"#,

                PreprocessorToken::DirectiveDefine => r"\s*#\s*define\s*\b",
                PreprocessorToken::DirectiveUndef => r"#\s*undef\b",
                // interpret the full include line here, accounting for the fact that escape
                // sequences and comments in "" and <> are not processed here.
                PreprocessorToken::DirectiveInclude(_,_) => r#"#\s*include\s+("[^\n"]*"|<[^\n>]*>)\s*$"#,
                PreprocessorToken::DirectiveIncludeWithMacro => r#"#\s*include\s*"#,
                PreprocessorToken::DirectiveIfdef => r"\s*#\s*ifdef\s*",
                PreprocessorToken::DirectiveIfndef => r"\s*#\s*ifndef\s*",
                PreprocessorToken::DirectiveIf => r"\s*#\s*if\s*",
                PreprocessorToken::DirectiveElse => r"\s*#\s*else\s*",
                PreprocessorToken::DirectiveElif => r"\s*#\s*elif\s*",
                PreprocessorToken::DirectiveEndif => r"\s*#\s*endif\s*",
                PreprocessorToken::DirectiveLine => r"\s*#\s*line\s*",
                PreprocessorToken::DirectiveError => r"\s*#\s*error\s*",
                PreprocessorToken::DirectivePragma => r"\s*#\s*pragma\s*",
                PreprocessorToken::DirectiveWarning => r"\s*#\s*warning\s*",
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
            PreprocessorToken::Punctuator(_) => PreprocessorToken::Punctuator(text.to_string()),
            PreprocessorToken::WidePunctuator(_) => {
                PreprocessorToken::WidePunctuator(text.to_string())
            }
            PreprocessorToken::WhiteSpace(_) => PreprocessorToken::WhiteSpace(text.to_string()),
            PreprocessorToken::DirectiveInclude(_, _) => {
                let inner_text = &text
                    .trim_start()
                    .trim_start_matches("#")
                    .trim_start()
                    .trim_start_matches("include")
                    .trim();
                let uses_angle_brackets = inner_text.starts_with("<");
                let inner_text = &inner_text[1..inner_text.len() - 1];
                PreprocessorToken::DirectiveInclude(inner_text.to_string(), uses_angle_brackets)
            }

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
            _ => {
                if let Ok(num) = text[1..].parse::<i8>() {
                    num.try_into().unwrap()
                } else {
                    panic!("Char must be a valid ASCII 7-bit value")
                }
            }
        };
        code as i8
    }

    fn display_character(c: &i8) -> String {
        char::from_u32(*c as u32)
            .map(|c| match c {
                '\'' => r#"\'"#.to_string(),
                '\"' => r#"\""#.to_string(),
                '?' => r#"\?"#.to_string(),
                '\\' => r"\\".to_string(),
                '\r' => r"\r".to_string(),
                '\n' => r"\n".to_string(),
                '\t' => r#"\t"#.to_string(),
                _ => match c as u32 {
                    7 => r"\a".to_string(),
                    8 => r"\b".to_string(),
                    11 => r"\v".to_string(),
                    12 => r"\f".to_string(),
                    _ => c.to_string(),
                },
            })
            .unwrap_or("NULL".to_string())
    }

    pub fn parse_string(mut text: String) -> Vec<i8> {
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

    fn find_next(
        text: &str,
        current_line_can_have_directive: bool,
    ) -> Result<(PreprocessorToken, usize), Box<dyn Error>> {
        ALL_TOKENS
            .iter()
            .map(|(possible_token, regex)| (possible_token, regex.captures(text)))
            // filter out non-matching patterns
            .filter_map(|(possible_token, maybe_captures)| {
                maybe_captures
                    .map(|captures| {
                        (
                            possible_token.clone(),
                            captures.get(captures.len() - 1).unwrap().end(),
                        ) // read the last capture group
                    })
                    // only capture directives if they are allowed on this line.
                    // This means they must be on a line preceded only by whitespace.
                    .filter(|(t, _)| current_line_can_have_directive || !t.is_directive())
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
            .ok_or::<Box<dyn Error>>(format!("Invalid token detected: {}", text).into())
    }

    fn is_directive(&self) -> bool {
        matches!(
            self,
            PreprocessorToken::DirectiveIf
                | PreprocessorToken::DirectiveIncludeWithMacro
                | PreprocessorToken::DirectiveInclude(_, _)
                | PreprocessorToken::DirectiveElse
                | PreprocessorToken::DirectiveElif
                | PreprocessorToken::DirectiveLine
                | PreprocessorToken::DirectiveIfdef
                | PreprocessorToken::DirectiveIfndef
                | PreprocessorToken::DirectiveUndef
                | PreprocessorToken::DirectiveError
                | PreprocessorToken::DirectiveWarning
                | PreprocessorToken::DirectiveDefine
                | PreprocessorToken::DirectivePragma
        )
    }
}

pub fn lex(lines: Vec<String>) -> Result<Tokens, Box<dyn Error>> {
    let mut tokens = Vec::new();

    for mut contents in lines.into_iter() {
        let mut current_line_can_have_directive = true;
        let mut tokens_for_line = Vec::new();
        while !contents.is_empty() {
            let (possible_token, end): (PreprocessorToken, usize) =
                PreprocessorToken::find_next(&contents, current_line_can_have_directive)?;
            let (substring, remainder) = contents.split_at(end);
            let substring = substring.to_string();
            contents = remainder.to_string();

            tokens_for_line.push(possible_token.instantiate(&substring));
            // directives can only be the first tokens on a line
            current_line_can_have_directive = false;
        }
        tokens.push(tokens_for_line);
    }

    // println!("{:?}", tokens);
    Ok(Tokens(tokens))
}

impl Display for Tokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.0.iter() {
            for token in line.iter() {
                write!(f, "{}", token)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for PreprocessorToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            PreprocessorToken::Identifier(ref s) | PreprocessorToken::Number(ref s) => {
                s.to_string()
            }
            PreprocessorToken::CharacterConstant(ref c) => {
                format!("\'{}\'", PreprocessorToken::display_character(c))
            }
            PreprocessorToken::StringLiteral(ref cs) => {
                format!(
                    "\"{}\"",
                    cs.iter().map(PreprocessorToken::display_character).join("")
                )
            }
            PreprocessorToken::Punctuator(ref s) => s.to_string(),
            PreprocessorToken::WidePunctuator(ref s) => s.to_string(),
            PreprocessorToken::WhiteSpace(ref s) => s.to_string(),
            PreprocessorToken::KeywordDefined => "defined".to_string(),
            PreprocessorToken::DirectiveDefine
            | PreprocessorToken::DirectiveInclude(_, _)
            | PreprocessorToken::DirectiveIncludeWithMacro
            | PreprocessorToken::DirectiveIfdef
            | PreprocessorToken::DirectiveIfndef
            | PreprocessorToken::DirectiveIf
            | PreprocessorToken::DirectiveElse
            | PreprocessorToken::DirectiveElif
            | PreprocessorToken::DirectiveEndif
            | PreprocessorToken::DirectiveUndef
            | PreprocessorToken::DirectiveLine
            | PreprocessorToken::DirectiveError
            | PreprocessorToken::DirectivePragma
            | PreprocessorToken::DirectiveWarning => {
                // format!("{:?}", self)
                panic!("Invalid token in output: {:?}", self);
            }
        };
        write!(f, "{}", str)
    }
}
