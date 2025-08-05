use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::VecDeque, error::Error};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Debug, PartialEq, EnumIter)]
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
    BitwiseAndAssign,
    BitwiseXorAssign,
    BitwiseOrAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    Question,
    Colon,
    Comma,
    OpenSquareBracket,
    CloseSquareBracket,
    Identifier(String),
    IntegerConstant(i64),
    LongConstant(i64),
    DoubleConstant(f64),
    UnsignedIntegerConstant(u64),
    UnsignedLongConstant(u64),
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
    KeywordExtern,
    KeywordStatic,
    KeywordLong,
    KeywordSigned,
    KeywordUnsigned,
    KeywordDouble,
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
    static ref ALL_TOKENS: Vec<(Token, Regex)> = {
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
                Token::BitwiseAndAssign => r"&=",
                Token::BitwiseXorAssign => r"\^=",
                Token::BitwiseOrAssign => r"\|=",
                Token::ShiftLeftAssign => r"<<=",
                Token::ShiftRightAssign => r">>=",
                Token::ModAssign => r"%=",
                Token::Question => r"\?",
                Token::Colon => r":",
                Token::Comma => r",",
                Token::OpenSquareBracket => r"\[",
                Token::CloseSquareBracket => r"\]",
                Token::Identifier(_) => r"^[a-zA-Z_]\w*\b",
                Token::IntegerConstant(_) => r"([0-9]+)[^\w.]",
                Token::LongConstant(_) => r"([0-9]+[lL])[^\w.]",
                Token::UnsignedIntegerConstant(_) => r"([0-9]+[uU])[^\w.]",
                Token::UnsignedLongConstant(_) => r"([0-9]+([lL][uU]|[uU][lL]))[^\w.]",
                Token::DoubleConstant(_) => r"((?:[0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]",
                // Token::IntegerConstant(_) => r"([0-9]+)\b",
                // Token::LongConstant(_) => r"([0-9]+[lL])\b",
                // Token::UnsignedIntegerConstant(_) => r"([0-9]+[uU])\b",
                // Token::UnsignedLongConstant(_) => r"([0-9]+([lL][uU]|[uU][lL]))\b",
                // Token::DoubleConstant(_) => r"(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)\b",
                // small hack to avoid having to use Option<> in this block, use
                // an empty string and then filter those out below
                Token::KeywordInt => r"int\b",
                Token::KeywordVoid =>r"void\b",
                Token::KeywordReturn =>r"return\b",
                Token::KeywordIf =>r"if\b",
                Token::KeywordElse =>r"else\b",
                Token::KeywordGoto =>r"goto\b",
                Token::KeywordDo =>r"do\b",
                Token::KeywordWhile =>r"while\b",
                Token::KeywordFor =>r"for\b",
                Token::KeywordBreak =>r"break\b",
                Token::KeywordContinue =>r"continue\b",
                Token::KeywordSwitch => r"switch\b",
                Token::KeywordCase =>r"case\b",
                Token::KeywordDefault =>r"default\b",
                Token::KeywordExtern =>r"extern\b",
                Token::KeywordStatic =>r"static\b",
                Token::KeywordLong =>r"long\b",
                Token::KeywordSigned =>r"signed\b",
                Token::KeywordUnsigned =>r"unsigned\b",
                Token::KeywordDouble =>r"double\b",
            };
            if !entry.is_empty(){
                let entry = "^".to_string() + entry;
                v.push((token, Regex::new(&entry).unwrap()))
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
            Token::IntegerConstant(_) => Token::IntegerConstant(text.parse::<i64>().unwrap()),
            Token::LongConstant(_) => {
                Token::LongConstant(text.trim_end_matches(['l', 'L']).parse::<i64>().unwrap())
            }
            Token::UnsignedIntegerConstant(_) => Token::UnsignedIntegerConstant(
                text.trim_end_matches(['u', 'U']).parse::<u64>().unwrap(),
            ),
            Token::UnsignedLongConstant(_) => Token::UnsignedLongConstant(
                text.trim_end_matches(['l', 'L', 'u', 'U'])
                    .parse::<u64>()
                    .unwrap(),
            ),
            Token::DoubleConstant(_) => Token::DoubleConstant(text.parse::<f64>().unwrap()),
            _ => self.clone(),
        }
    }

    fn find_next(text: &str) -> Result<(Token, usize), Box<dyn Error>> {
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
                        (!matches!(token_a, Token::Identifier(_))).into();
                    let token_b_precedence: usize =
                        (!matches!(token_b, Token::Identifier(_))).into();
                    token_a_precedence.cmp(&token_b_precedence)
                })
            })
            .ok_or::<Box<dyn Error>>("Invalid token detected".into())
    }
}

pub fn lex(mut contents: &str) -> Result<VecDeque<Token>, Box<dyn Error>> {
    let mut tokens: VecDeque<Token> = VecDeque::new();

    contents = contents.trim_start();
    while !contents.is_empty() {
        let (possible_token, end): (Token, usize) = Token::find_next(contents)?;
        let (substring, remainder) = contents.split_at(end);
        contents = remainder;

        tokens.push_back(possible_token.instantiate(substring));
        contents = contents.trim_start();
    }

    Ok(tokens)
}
