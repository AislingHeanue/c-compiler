use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::VecDeque, error::Error, mem::discriminant};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Debug, PartialEq, EnumIter)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenSquareBracket,
    CloseSquareBracket,
    Comma,

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
    Question,
    Colon,
    Arrow,
    Dot,
    Ellipses,

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

    Identifier(String),
    // u64 value and sign
    IntegerConstant(u64),
    LongConstant(u64),
    LongLongConstant(u64),
    FloatConstant(f32),
    DoubleConstant(f64),
    LongDoubleConstant(f64),
    UnsignedIntegerConstant(u64),
    UnsignedLongConstant(u64),
    UnsignedLongLongConstant(u64),
    CharacterConstant(i8),
    StringLiteral(Vec<i8>),

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
    KeywordFloat,
    KeywordDouble,
    KeywordChar,
    KeywordShort,
    KeywordTypedef,
    KeywordSizeof,
    KeywordStruct,
    KeywordUnion,
    KeywordEnum,
    KeywordConst,
    KeywordRestrict,
    KeywordInline,
    KeywordVolatile,
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
                Token::Dot => r"(\.)[^0-9.]",
                Token::Arrow => r"->",
                Token::OpenSquareBracket => r"\[",
                Token::CloseSquareBracket => r"\]",
                Token::Ellipses => r"\.\.\.",
                Token::Identifier(_) => r"^[a-zA-Z_]\w*\b",
                Token::IntegerConstant(_) => r"((?:0x[0-9a-fA-F]+)|(?:[0-9]+))[^\w.]",
                Token::LongConstant(_) => r"((?:(?:0x[0-9a-fA-F]+)|(?:[0-9]+))[lL])[^\w.]",
                Token::LongLongConstant(_) => r"((?:(?:0x[0-9a-fA-F]+)|(?:[0-9]+))(?:ll|LL))[^\w.]",
                Token::UnsignedIntegerConstant(_) => r"((?:(?:0x[0-9a-fA-F]+)|(?:[0-9]+))[uU])[^\w.]",
                Token::UnsignedLongConstant(_) => r"((?:(?:0x[0-9a-fA-F]+)|(?:[0-9]+))([lL][uU]|[uU][lL]))[^\w.]",
                Token::UnsignedLongLongConstant(_) => r"((?:(?:0x[0-9a-fA-F]+)|(?:[0-9]+))((?:ll|LL)[uU]|[uU](?:ll|LL)))[^\w.]",
                Token::FloatConstant(_) => r"((?:(?:[0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[fF])[^\w.]",
                Token::DoubleConstant(_) => r"((?:[0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]",
                Token::LongDoubleConstant(_) => r"((?:(?:[0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[lL])[^\w.]",
                Token::CharacterConstant(_) => r#"'(?:[^'\\\n]|\\['"?\\abfnrtv])'"#,
                Token::StringLiteral(_) => r#""(?:[^"\\\n]|\\['"?\\abfnrtv])*""#,
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
                Token::KeywordFloat =>r"float\b",
                Token::KeywordDouble =>r"double\b",
                Token::KeywordChar =>r"char\b",
                Token::KeywordShort =>r"short\b",
                Token::KeywordTypedef =>r"typedef\b",
                Token::KeywordSizeof => r"sizeof\b",
                Token::KeywordStruct => r"struct\b",
                Token::KeywordUnion => r"union\b",
                Token::KeywordEnum => r"enum\b",
                Token::KeywordConst => r"const\b",
                Token::KeywordRestrict => r"restrict\b",
                Token::KeywordInline => r"inline\b",
                Token::KeywordVolatile => r"volatile\b",
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
            Token::CharacterConstant(_) => {
                // strip first and last characters (')
                Token::CharacterConstant(Token::parse_character(&text[1..text.len() - 1]))
            }
            Token::StringLiteral(_) => {
                Token::StringLiteral(Token::parse_string(text[1..text.len() - 1].to_string()))
            }
            Token::IntegerConstant(_) => {
                if text.starts_with("0x") {
                    Token::IntegerConstant(
                        u64::from_str_radix(text.strip_prefix("0x").unwrap(), 16).unwrap(),
                    )
                } else if text.starts_with("0") {
                    Token::IntegerConstant(u64::from_str_radix(text, 8).unwrap())
                } else {
                    Token::IntegerConstant(text.parse::<u64>().unwrap())
                }
            }
            Token::LongConstant(_) => {
                let text = text.trim_end_matches(['l', 'L']);
                if text.starts_with("0x") {
                    Token::LongConstant(
                        u64::from_str_radix(text.strip_prefix("0x").unwrap(), 16).unwrap(),
                    )
                } else if text.starts_with("0") {
                    Token::LongConstant(u64::from_str_radix(text, 8).unwrap())
                } else {
                    Token::LongConstant(text.parse::<u64>().unwrap())
                }
            }
            Token::LongLongConstant(_) => {
                let text = text.trim_end_matches(['l', 'L']);
                if text.starts_with("0x") {
                    Token::LongLongConstant(
                        u64::from_str_radix(text.strip_prefix("0x").unwrap(), 16).unwrap(),
                    )
                } else if text.starts_with("0") {
                    Token::LongLongConstant(u64::from_str_radix(text, 8).unwrap())
                } else {
                    Token::LongLongConstant(text.parse::<u64>().unwrap())
                }
            }
            Token::UnsignedIntegerConstant(_) => {
                let text = text.trim_end_matches(['u', 'U']);
                if text.starts_with("0x") {
                    Token::UnsignedIntegerConstant(
                        u64::from_str_radix(text.strip_prefix("0x").unwrap(), 16).unwrap(),
                    )
                } else if text.starts_with("0") {
                    Token::UnsignedIntegerConstant(u64::from_str_radix(text, 8).unwrap())
                } else {
                    Token::UnsignedIntegerConstant(text.parse::<u64>().unwrap())
                }
            }
            Token::UnsignedLongConstant(_) => {
                let text = text.trim_end_matches(['u', 'U', 'l', 'L']);
                if text.starts_with("0x") {
                    Token::UnsignedLongConstant(
                        u64::from_str_radix(text.strip_prefix("0x").unwrap(), 16).unwrap(),
                    )
                } else if text.starts_with("0") {
                    Token::UnsignedLongConstant(u64::from_str_radix(text, 8).unwrap())
                } else {
                    Token::UnsignedLongConstant(text.parse::<u64>().unwrap())
                }
            }
            Token::UnsignedLongLongConstant(_) => {
                let text = text.trim_end_matches(['u', 'U', 'l', 'L']);
                if text.starts_with("0x") {
                    Token::UnsignedLongLongConstant(
                        u64::from_str_radix(text.strip_prefix("0x").unwrap(), 16).unwrap(),
                    )
                } else if text.starts_with("0") {
                    Token::UnsignedLongLongConstant(u64::from_str_radix(text, 8).unwrap())
                } else {
                    Token::UnsignedLongLongConstant(text.parse::<u64>().unwrap())
                }
            }
            Token::FloatConstant(_) => {
                Token::FloatConstant(text.trim_end_matches(['f', 'F']).parse::<f32>().unwrap())
            }
            Token::DoubleConstant(_) => Token::DoubleConstant(text.parse::<f64>().unwrap()),
            Token::LongDoubleConstant(_) => {
                Token::LongDoubleConstant(text.trim_end_matches(['l', 'L']).parse::<f64>().unwrap())
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
            _ => panic!("Char must be a valid ASCII 7-bit value"),
        };
        code as i8
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
            .ok_or::<Box<dyn Error>>(format!("Invalid token detected: {:?}", text).into())
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

impl Token {
    pub fn is_unary_operator(&self) -> bool {
        matches!(
            self,
            Token::Hyphen
                | Token::Tilde
                | Token::Not
                | Token::Increment
                | Token::Decrement
                | Token::Plus
        )
    }

    pub fn is_suffix_operator(&self) -> bool {
        matches!(
            self,
            Token::Increment
                | Token::Decrement
                | Token::OpenSquareBracket
                | Token::Dot
                | Token::Arrow
                | Token::OpenParen
        )
    }

    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Token::IntegerConstant(_)
                | Token::LongConstant(_)
                | Token::LongLongConstant(_)
                | Token::UnsignedIntegerConstant(_)
                | Token::UnsignedLongConstant(_)
                | Token::UnsignedLongLongConstant(_)
                | Token::FloatConstant(_)
                | Token::DoubleConstant(_)
                | Token::LongDoubleConstant(_)
                | Token::CharacterConstant(_)
        )
    }

    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            Token::Assignment
                | Token::AddAssign
                | Token::SubtractAssign
                | Token::MultiplyAssign
                | Token::DivideAssign
                | Token::ModAssign
                | Token::BitwiseAndAssign
                | Token::BitwiseXorAssign
                | Token::BitwiseOrAssign
                | Token::ShiftLeftAssign
                | Token::ShiftRightAssign
        )
    }
}

pub trait TokenVector {
    fn expect(&mut self, expected: Token) -> Result<(), Box<dyn Error>>;
    fn read(&mut self) -> Result<Token, Box<dyn Error>>;
    fn peek(&mut self) -> Result<Token, Box<dyn Error>>;
}

impl TokenVector for VecDeque<Token> {
    fn expect(&mut self, expected: Token) -> Result<(), Box<dyn Error>> {
        // println!("expecting {:?}", self.front());
        let token = self.read()?;
        if discriminant(&expected) != discriminant(&token) {
            return Err(format!(
                "Unexpected token, got {:?}, expecting {:?}",
                token, expected
            )
            .into());
        }
        Ok(())
    }

    fn read(&mut self) -> Result<Token, Box<dyn Error>> {
        // println!("popping {:?}", self.front());
        self.pop_front()
            .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())
    }

    fn peek(&mut self) -> Result<Token, Box<dyn Error>> {
        // println!("peeking {:?}", self.front());
        Ok(self
            .front()
            .ok_or::<Box<dyn Error>>("Unexpected end of tokens".into())?
            .clone())
    }
}
