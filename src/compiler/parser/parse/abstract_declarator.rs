use super::{expect, peek, AbstractDeclarator, Parse, ParseContext, Type};
use crate::compiler::{lexer::Token, types::Constant};
use std::{collections::VecDeque, error::Error};

impl Parse for AbstractDeclarator {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::Star => {
                expect(tokens, Token::Star)?;
                Ok(AbstractDeclarator::Pointer(Box::new(
                    AbstractDeclarator::parse(tokens, context)?,
                )))
            }
            _ => Ok(AbstractDeclarator::parse_direct(tokens, context)?),
        }
    }
}

impl AbstractDeclarator {
    fn parse_direct(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::OpenParen => {
                expect(tokens, Token::OpenParen)?;
                let mut a = AbstractDeclarator::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                while matches!(peek(tokens)?, Token::OpenSquareBracket) {
                    a = AbstractDeclarator::parse_array(tokens, a, context)?;
                }
                Ok(a)
            }
            Token::OpenSquareBracket => {
                let mut a = AbstractDeclarator::Base;
                while matches!(peek(tokens)?, Token::OpenSquareBracket) {
                    a = AbstractDeclarator::parse_array(tokens, a, context)?;
                }
                Ok(a)
            }
            _ => Ok(AbstractDeclarator::Base),
        }
    }

    fn parse_array(
        tokens: &mut VecDeque<Token>,
        mut a_declarator: AbstractDeclarator,
        context: &mut ParseContext,
    ) -> Result<AbstractDeclarator, Box<dyn Error>> {
        expect(tokens, Token::OpenSquareBracket)?;
        let c = Constant::parse(tokens, context)?;
        let maybe_i: Option<u64> = match c {
            Constant::Integer(i) => i.try_into().ok(),
            Constant::Long(i) => i.try_into().ok(),
            Constant::UnsignedInteger(i) => Some(i.into()),
            Constant::UnsignedLong(i) => Some(i),
            _ => None,
        };
        if let Some(i) = maybe_i {
            if i < 1 {
                return Err("Array dimension must be at least 1".into());
            }

            a_declarator = AbstractDeclarator::Array(Box::new(a_declarator), i);
        } else {
            return Err("Constant value in array type must be an integer".into());
        }
        expect(tokens, Token::CloseSquareBracket)?;
        Ok(a_declarator)
    }

    pub fn apply_to_type(
        self,
        base_type: Type,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<Type, Box<dyn Error>> {
        match self {
            AbstractDeclarator::Pointer(a) => {
                Ok(a.apply_to_type(Type::Pointer(Box::new(base_type)))?)
            }
            AbstractDeclarator::Array(a, size) => {
                Ok(a.apply_to_type(Type::Array(Box::new(base_type), size))?)
            }
            AbstractDeclarator::Base => Ok(base_type),
        }
    }
}
