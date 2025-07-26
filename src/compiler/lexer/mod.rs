use lazy_static::lazy_static;
use regex::{Match, Regex};
use std::{collections::VecDeque, error::Error};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Debug, PartialEq, Eq, EnumIter)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    SemiColon,
    Tilde,
    Plus,
    Hyphen,
    Star,
    Slash,
    Percent,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
    Not,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Assignment,
    Decrement,
    Increment,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModAssign,
    AndAssign,
    XorAssign,
    OrAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    Question,
    Colon,
    Comma,
    Identifier(String),
    IntegerConstant(usize),
    KeywordInt,
    KeywordVoid,
    KeywordReturn,
    KeywordIf,
    KeywordElse,
    KeywordGoto,
    KeywordDo,
    KeywordWhile,
    KeywordFor,
    KeywordBreak,
    KeywordContinue,
    KeywordSwitch,
    KeywordCase,
    KeywordDefault,
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
                Token::OpenParen => r"\(",
                Token::CloseParen => r"\)",
                Token::OpenBrace => r"\{",
                Token::CloseBrace => r"\}",
                Token::SemiColon => r";",
                Token::Tilde => r"~",
                Token::Plus => r"\+",
                Token::Hyphen => r"-",
                Token::Star => r"\*",
                Token::Slash => r"/",
                Token::Percent => r"%",
                Token::BitwiseAnd => r"&",
                Token::BitwiseXor => r"\^",
                Token::BitwiseOr => r"\|",
                Token::ShiftLeft => r"<<",
                Token::ShiftRight => r">>",
                Token::Not => r"!",
                Token::And => r"&&",
                Token::Or => r"\|\|",
                Token::Equal => r"==",
                Token::NotEqual => "!=",
                Token::Less => r"<",
                Token::Greater => r">",
                Token::LessEqual => r"<=",
                Token::GreaterEqual => r">=",
                Token::Assignment => r"=",
                Token::Increment => r"\+\+",
                Token::Decrement => r"--",
                Token::AddAssign => r"\+=",
                Token::SubtractAssign => r"-=",
                Token::MultiplyAssign => r"\*=",
                Token::DivideAssign => r"/=",
                Token::AndAssign => r"&=",
                Token::XorAssign => r"\^=",
                Token::OrAssign => r"\|=",
                Token::ShiftLeftAssign => r"<<=",
                Token::ShiftRightAssign => r">>=",
                Token::ModAssign => r"%=",
                Token::Question => r"\?",
                Token::Colon => r":",
                Token::Comma => r",",
                Token::Identifier(_) => r"^[a-zA-Z_]\w*\b",
                Token::IntegerConstant(_) =>r"[0-9]+\b",
                // small hack to avoid having to use Option<> in this block, use
                // an empty string and then filter those out below
                Token::KeywordInt => "",
                Token::KeywordVoid => "",
                Token::KeywordReturn => "",
                Token::KeywordIf => "",
                Token::KeywordElse => "",
                Token::KeywordGoto => "",
                Token::KeywordDo => "",
                Token::KeywordWhile => "",
                Token::KeywordFor => "",
                Token::KeywordBreak => "",
                Token::KeywordContinue => "",
                Token::KeywordSwitch => "",
                Token::KeywordCase => "",
                Token::KeywordDefault => "",
            };
            if !entry.is_empty(){
                let entry = "^".to_string() + entry;
                v.push((token, Regex::new(&entry).unwrap(), 0))
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
            Token::Identifier(_) => match text {
                "int" => Token::KeywordInt,
                "return" => Token::KeywordReturn,
                "void" => Token::KeywordVoid,
                "if" => Token::KeywordIf,
                "else" => Token::KeywordElse,
                "goto" => Token::KeywordGoto,
                "do" => Token::KeywordDo,
                "while" => Token::KeywordWhile,
                "for" => Token::KeywordFor,
                "break" => Token::KeywordBreak,
                "continue" => Token::KeywordContinue,
                "switch" => Token::KeywordSwitch,
                "case" => Token::KeywordCase,
                "default" => Token::KeywordDefault,
                _ => Token::Identifier(text.to_string()),
            },
            Token::IntegerConstant(_) => Token::IntegerConstant(text.parse::<usize>().unwrap()),
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
