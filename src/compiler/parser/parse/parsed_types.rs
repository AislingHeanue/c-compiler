use super::{peek, read, Parse, ParseContext, Type};
use crate::compiler::lexer::Token;
use std::{collections::VecDeque, error::Error};

impl Parse for Type {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        let mut out = Vec::new();
        while !tokens.is_empty() && peek(tokens)?.is_type() {
            out.push(read(tokens)?)
        }
        if out.is_empty() {
            return Err("No type tokens found".into());
        }
        let mut seen = Vec::new();
        for i in out.iter() {
            if seen.contains(i) {
                return Err("Repeated token in type definition".into());
            }
            seen.push(i.clone());
        }
        if out.contains(&Token::KeywordDouble) {
            if out.len() == 1 {
                return Ok(Type::Double);
            } else {
                return Err("Double cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordSigned) && out.contains(&Token::KeywordUnsigned) {
            return Err("Type specified as both signed and unsigned".into());
        }
        if out.contains(&Token::KeywordUnsigned) && out.contains(&Token::KeywordLong) {
            return Ok(Type::UnsignedLong);
        }
        if out.contains(&Token::KeywordChar) {
            if out.iter().any(|t| {
                !matches!(
                    t,
                    Token::KeywordChar | Token::KeywordSigned | Token::KeywordUnsigned
                )
            }) {
                return Err("Char type definition contained another type".into());
            }
            if out.contains(&Token::KeywordUnsigned) {
                return Ok(Type::UnsignedChar);
            }
            if out.contains(&Token::KeywordSigned) {
                return Ok(Type::SignedChar);
            }
            return Ok(Type::Char);
        }
        if out.contains(&Token::KeywordUnsigned) {
            return Ok(Type::UnsignedInteger);
        }
        if out.contains(&Token::KeywordLong) {
            return Ok(Type::Long);
        }
        Ok(Type::Integer)
    }
}
