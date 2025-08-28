use super::{AbstractDeclarator, Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    types::Constant,
};
use std::{collections::VecDeque, error::Error};

impl Parse<AbstractDeclarator> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<AbstractDeclarator, Box<dyn Error>> {
        match self.peek()? {
            Token::Star => {
                self.expect(Token::Star)?;
                match self.peek()? {
                    Token::KeywordRestrict => {
                        self.expect(Token::KeywordRestrict)?;
                        Ok(AbstractDeclarator::Pointer(
                            Box::new(self.parse(context)?),
                            true,
                        ))
                    }
                    _ => Ok(AbstractDeclarator::Pointer(
                        Box::new(self.parse(context)?),
                        false,
                    )),
                }
            }
            _ => Ok(self.parse_direct(context)?),
        }
    }
}

trait ParseAbstractDeclarator {
    fn parse_direct(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<AbstractDeclarator, Box<dyn Error>>;

    fn parse_array(
        &mut self,
        a_declarator: AbstractDeclarator,
        context: &mut ParseContext,
    ) -> Result<AbstractDeclarator, Box<dyn Error>>;
}

impl ParseAbstractDeclarator for VecDeque<Token> {
    fn parse_direct(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<AbstractDeclarator, Box<dyn Error>> {
        match self.peek()? {
            Token::OpenParen => {
                self.expect(Token::OpenParen)?;
                let mut a = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                while matches!(self.peek()?, Token::OpenSquareBracket) {
                    a = self.parse_array(a, context)?;
                }
                Ok(a)
            }
            Token::OpenSquareBracket => {
                let mut a = AbstractDeclarator::Base;
                while matches!(self.peek()?, Token::OpenSquareBracket) {
                    a = self.parse_array(a, context)?;
                }
                Ok(a)
            }
            _ => Ok(AbstractDeclarator::Base),
        }
    }

    fn parse_array(
        &mut self,
        mut a_declarator: AbstractDeclarator,
        context: &mut ParseContext,
    ) -> Result<AbstractDeclarator, Box<dyn Error>> {
        self.expect(Token::OpenSquareBracket)?;
        let c = self.parse(context)?;
        let maybe_i: Option<u64> = match c {
            Constant::Integer(i) => i.try_into().ok(),
            Constant::Long(i) => i.try_into().ok(),
            Constant::UnsignedInteger(i) => Some(i.into()),
            Constant::UnsignedLong(i) => Some(i),
            Constant::Char(i) => Some(i as u64),
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
        self.expect(Token::CloseSquareBracket)?;
        Ok(a_declarator)
    }
}

impl AbstractDeclarator {
    pub fn apply_to_type(
        self,
        base_type: Type,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<Type, Box<dyn Error>> {
        match self {
            AbstractDeclarator::Pointer(a, is_restricted) => {
                Ok(a.apply_to_type(Type::Pointer(Box::new(base_type), is_restricted))?)
            }
            AbstractDeclarator::Array(a, size) => {
                Ok(a.apply_to_type(Type::Array(Box::new(base_type), size))?)
            }
            AbstractDeclarator::Base => Ok(base_type),
        }
    }
}
