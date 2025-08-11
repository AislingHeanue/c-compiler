use super::{Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::ExpressionWithoutType,
};
use std::{collections::VecDeque, error::Error};

impl Parse<Type> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<Type, Box<dyn Error>> {
        let mut out = Vec::new();
        while !self.is_empty() && self.peek()?.is_type(context) {
            out.push(self.read()?)
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
        if let Some(Token::Identifier(s)) = out
            .iter()
            .find(|token| matches!(token, Token::Identifier(_)))
        {
            if out.len() == 1 {
                return ExpressionWithoutType::resolve_type_alias(s, context);
            } else {
                return Err("Aliased type cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordDouble) {
            if out.len() == 1 {
                return Ok(Type::Double);
            } else {
                return Err("Double cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordVoid) {
            if out.len() == 1 {
                return Ok(Type::Void);
            } else {
                return Err("Void cannot be used with other type specifiers".into());
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

impl Token {
    pub fn is_type(&self, context: &mut ParseContext) -> bool {
        if let Token::Identifier(s) = self {
            ExpressionWithoutType::resolve_type_alias(s, context).is_ok()
        } else {
            matches!(
                self,
                Token::KeywordLong
                    | Token::KeywordInt
                    | Token::KeywordUnsigned
                    | Token::KeywordSigned
                    | Token::KeywordDouble
                    | Token::KeywordChar
                    | Token::KeywordVoid
            )
        }
    }

    pub fn is_specifier(&self, context: &mut ParseContext) -> bool {
        self.is_type(context) || matches!(self, Token::KeywordStatic | Token::KeywordExtern)
    }

    pub fn is_start_of_declaration(&self, context: &mut ParseContext) -> bool {
        self.is_specifier(context) || matches!(self, Token::KeywordTypedef)
    }
}
