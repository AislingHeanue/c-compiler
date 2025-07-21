use lazy_static::lazy_static;
use regex::{Match, Regex};
use std::{collections::VecDeque, error::Error};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use super::IndentDisplay;

#[derive(Clone, Debug, PartialEq, Eq, EnumIter)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    SemiColon,
    Tilde,
    Hyphen,
    Decrement,
    Plus,
    Slash,
    Star,
    Percent,
    Keyword(Keyword),
    Identifier(String),
    Constant(Type),
}

#[derive(Clone, Debug, Eq, PartialEq, EnumIter)]
pub enum Type {
    Integer(i32),
}

impl Default for Type {
    fn default() -> Self {
        Self::Integer(0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default, EnumIter)]
pub enum Keyword {
    Int,
    Return,
    #[default]
    Void,
}

lazy_static! {
    // format is (Token, Regex, Precedence). Higher precedence regexes are taken over lower ones.
    // Needed for keywords vs identifiers.
    static ref ALL_TOKENS: Vec<(Token, Regex, usize)> = {
        let mut v = Vec::new();
        for token in Token::iter() {
            let entry = match token {
                Token::OpenParen => Regex::new(r"^\("),
                Token::CloseParen => Regex::new(r"^\)"),
                Token::OpenBrace => Regex::new(r"^\{"),
                Token::CloseBrace => Regex::new(r"^\}"),
                Token::SemiColon => Regex::new(r"^;"),
                Token::Tilde => Regex::new(r"^~"),
                Token::Hyphen => Regex::new(r"^-"),
                Token::Decrement => Regex::new(r"^--"),
                Token::Plus => Regex::new(r"^\+"),
                Token::Slash => Regex::new(r"^/"),
                Token::Star => Regex::new(r"^\*"),
                Token::Percent => Regex::new(r"^%"),
                Token::Identifier(_) => Regex::new(r"^[a-zA-Z_]\w*\b"),
                // small hack to avoid having to use Option<> in this block, hijack an existing
                // error struct in the regex package.
                Token::Constant(_) => Err(regex::Error::Syntax("Constant regex defined elsewhere".into())),
                Token::Keyword(_) => Err(regex::Error::Syntax("Keyword regex defined elsewhere".into())),
            };
            if let Ok(regex) = entry {
                v.push((token, regex, 0))
            }
        }
        for keyword in Keyword::iter() {
            let keyword_entry = match keyword {
                Keyword::Int => Regex::new("^int"),
                Keyword::Return => Regex::new("^return"),
                Keyword::Void => Regex::new("^void"),
            };
            if let Ok(regex) = keyword_entry {
                v.push((Token::Keyword(keyword), regex, 1))
            }
        }
        for a_type in Type::iter() {
            let type_entry = match a_type {
                Type::Integer(_) =>Regex::new(r"^[0-9]+\b"),
            };
            if let Ok(regex) = type_entry {
                v.push((Token::Constant(a_type), regex, 0))
            }
        }
        v
    };
}

impl Token {
    fn instantiate(&self, text: &str) -> Token {
        // fill in values for identifiers and integer constants, since they need to read the actual
        // contents of the matching field
        match self {
            Token::Identifier(_) => Token::Identifier(text.to_string()),
            Token::Constant(Type::Integer(_)) => {
                Token::Constant(Type::Integer(text.parse::<i32>().unwrap()))
            }
            _ => self.clone(),
        }
    }

    fn find_next(text: &str) -> Result<(Token, Match), Box<dyn Error>> {
        ALL_TOKENS
            .iter()
            .map(|(possible_token, regex, precedence)| {
                (possible_token, regex.find(text), precedence)
            })
            // filter out non-matching patterns
            .filter_map(|(possible_token, maybe_matched, precedence)| {
                maybe_matched.map(|matched| (possible_token.clone(), matched, precedence))
            })
            .max_by(
                |(_, matched_a, precedence_a), (_, matched_b, precedence_b)| {
                    matched_a
                        .end()
                        .cmp(&matched_b.end())
                        // need to order by precedence because keywords and identifiers may match
                        // results with the same length, and in this case the keyword is always
                        // preferred.
                        .then(precedence_a.cmp(precedence_b))
                },
            )
            .map(|(possible_token, matched, _)| (possible_token, matched))
            .ok_or::<Box<dyn Error>>("Invalid token detected".into())
    }
}

pub fn lex(mut contents: &str) -> Result<VecDeque<Token>, Box<dyn Error>> {
    let mut tokens: VecDeque<Token> = VecDeque::new();

    contents = contents.trim_start();
    while !contents.is_empty() {
        let (possible_token, matched): (Token, Match) = Token::find_next(contents)?;
        let (substring, remainder) = contents.split_at(matched.end());
        contents = remainder;

        tokens.push_back(possible_token.instantiate(substring));
        contents = contents.trim_start();
    }

    Ok(tokens)
}

impl IndentDisplay for Type {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
        }
    }
}
