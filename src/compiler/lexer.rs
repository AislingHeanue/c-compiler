use lazy_static::lazy_static;
use regex::{Match, Regex};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

use super::IndentDisplay;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    SemiColon,
    Tilde,
    Hyphen,
    Decrement,
    Keyword(Keyword),
    Identifier(String),
    Constant(Type),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Integer(i32),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

lazy_static! {
    static ref ALL_TOKENS: Vec<(Token, Regex)> = vec![
        (Token::OpenParen, Regex::new(r"^\(").unwrap()),
        (Token::CloseParen, Regex::new(r"^\)").unwrap()),
        (Token::OpenBrace, Regex::new(r"^\{").unwrap()),
        (Token::CloseBrace, Regex::new(r"^\}").unwrap()),
        (Token::SemiColon, Regex::new(r"^;").unwrap()),
        (Token::Tilde, Regex::new(r"^~").unwrap()),
        (Token::Hyphen, Regex::new(r"^-").unwrap()),
        (Token::Decrement, Regex::new(r"^--").unwrap()),
        (
            Token::Identifier(String::new()),
            Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
        ),
        (
            Token::Constant(Type::Integer(0)),
            Regex::new(r"^[0-9]+\b").unwrap(),
        ),
        // keyword tokens omitted, they are only checked against tokens which match the
        // identifier regex.
    ];
    static ref KEYWORD_TOKENS: HashMap<String, Token> = {
        let mut m = HashMap::new();
        m.insert("int".to_string(), Token::Keyword(Keyword::Int));
        m.insert("void".to_string(), Token::Keyword(Keyword::Void));
        m.insert("return".to_string(), Token::Keyword(Keyword::Return));
        m
    };
}

impl Token {
    fn instantiate(&self, text: &str) -> Token {
        match self {
            Token::Identifier(_) => KEYWORD_TOKENS
                .get(text)
                .unwrap_or(&Token::Identifier(text.to_string()))
                .clone(),

            Token::Constant(_) => Token::Constant(Type::Integer(text.parse::<i32>().unwrap())),
            _ => self.clone(),
        }
    }

    fn find_next(text: &str) -> Result<(Token, Match), Box<dyn Error>> {
        ALL_TOKENS
            .iter()
            .map(|(possible_token, regex)| (possible_token, regex.find(text)))
            // filter out non-matching patterns
            .filter_map(|(possible_token, maybe_matched)| {
                maybe_matched.map(|matched| (possible_token.clone(), matched))
            })
            .max_by(|(_, matched_a), (_, matched_b)| matched_a.end().cmp(&matched_b.end()))
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
    // println!("{:?}", tokens);

    Ok(tokens)
}

impl IndentDisplay for Type {
    fn fmt_indent(&self, _indent: usize, _comments: bool) -> String {
        match self {
            Self::Integer(value) => {
                format!("Integer({})", value)
            }
        }
    }
}
