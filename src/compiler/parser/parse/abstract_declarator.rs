use super::{AbstractDeclarator, Parse, ParseContext, Type};
use crate::compiler::lexer::{Token, TokenVector};
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

        let e = self.parse(context)?;
        a_declarator = AbstractDeclarator::Array(Box::new(a_declarator), e);

        self.expect(Token::CloseSquareBracket)?;
        Ok(a_declarator)
    }
}

impl AbstractDeclarator {
    pub fn apply_to_type(
        self,
        base_type: Type,
        context: &mut ParseContext,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<Type, Box<dyn Error>> {
        match self {
            AbstractDeclarator::Pointer(a, is_restricted) => {
                Ok(a.apply_to_type(Type::Pointer(Box::new(base_type), is_restricted), context)?)
            }
            AbstractDeclarator::Array(a, size_expression) => {
                let size = size_expression.fold_to_constant(context)?.value_unsigned();
                Ok(a.apply_to_type(Type::Array(Box::new(base_type), size), context)?)
            }
            AbstractDeclarator::Base => Ok(base_type),
        }
    }
}
