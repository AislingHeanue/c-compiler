use itertools::process_results;

use super::{Declarator, Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    types::Constant,
};
use std::{collections::VecDeque, error::Error};

impl Parse<Declarator> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<Declarator, Box<dyn Error>> {
        match self.peek()? {
            Token::Star => {
                self.expect(Token::Star)?;
                Ok(Declarator::Pointer(Box::new(self.parse(context)?)))
            }
            _ => {
                let mut simple_declarator = self.parse_simple_declarator(context)?;
                match self.peek() {
                    // function declaration type!
                    Ok(Token::OpenParen) => {
                        let param_list = self.parse_param_list(context)?;
                        Ok(Declarator::Function(
                            Box::new(simple_declarator),
                            param_list,
                        ))
                    }
                    Ok(Token::OpenSquareBracket) => {
                        while !self.is_empty() && matches!(self.peek()?, Token::OpenSquareBracket) {
                            simple_declarator = self.parse_array(simple_declarator, context)?;
                        }
                        Ok(simple_declarator)
                    }
                    Ok(_) => Ok(simple_declarator),
                    // no further tokens found after the declarator, this is okay
                    Err(_) => Ok(simple_declarator),
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct DeclaratorApplicationOutput {
    pub name: String,
    pub out_type: Type,
    pub param_names: Option<Vec<String>>,
}

trait ParseDeclarator {
    fn parse_simple_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>>;

    fn parse_param_list(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Vec<(Type, Declarator)>, Box<dyn Error>>;
    fn parse_param(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Declarator), Box<dyn Error>>;
    fn parse_array(
        &mut self,
        declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>>;
}

impl ParseDeclarator for VecDeque<Token> {
    fn parse_simple_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        match self.read()? {
            Token::Identifier(name) => {
                // do not process the scope of 'name' here, because it complicates parameters
                Ok(Declarator::Name(name))
            }
            Token::OpenParen => {
                let declarator = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                Ok(declarator)
            }
            _ => Err("Invalid declarator".into()),
        }
    }

    fn parse_param_list(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Vec<(Type, Declarator)>, Box<dyn Error>> {
        self.expect(Token::OpenParen)?;
        let mut param_list = Vec::new();

        if self.peek()? == Token::KeywordVoid {
            self.expect(Token::KeywordVoid)?;
            self.expect(Token::CloseParen)?;
            return Ok(param_list);
        }

        param_list.push(self.parse_param(context)?);
        while self.peek()? != Token::CloseParen {
            self.expect(Token::Comma)?;
            param_list.push(self.parse_param(context)?);
        }
        self.expect(Token::CloseParen)?;
        Ok(param_list)
    }

    fn parse_param(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Declarator), Box<dyn Error>> {
        let out_type = self.parse(context)?;
        // function params never have static or extern storage
        let declarator = self.parse(context)?;
        Ok((out_type, declarator))
    }

    fn parse_array(
        &mut self,
        mut declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
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

            declarator = Declarator::Array(Box::new(declarator), i);
        } else {
            return Err("Constant value in array type must be an integer".into());
        }
        self.expect(Token::CloseSquareBracket)?;
        Ok(declarator)
    }
}

impl Declarator {
    pub fn apply_to_type(
        self,
        base_type: Type,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<DeclaratorApplicationOutput, Box<dyn Error>> {
        match self {
            Declarator::Name(name) => Ok(DeclaratorApplicationOutput {
                out_type: base_type,
                name,
                param_names: None,
            }),
            Declarator::Pointer(declarator) => {
                // discard param names, function pointers aren't real
                declarator.apply_to_type(Type::Pointer(Box::new(base_type)))
            }
            Declarator::Function(declarator, params) => {
                let name = if let Declarator::Name(name) = *declarator {
                    name
                } else {
                    return Err("Cannot apply additional declarators to a function type (function pointers aren't real)".into());
                };
                let (param_types, param_names): (Vec<Type>, Vec<String>) = process_results(
                    params
                        .into_iter()
                        .map(|(t, declarator)| declarator.apply_to_type(t)),
                    |iter| iter.map(|o| (o.out_type, o.name)).unzip(),
                )?;

                Ok(DeclaratorApplicationOutput {
                    out_type: Type::Function(Box::new(base_type), param_types),
                    name,
                    param_names: Some(param_names),
                })
            }
            Declarator::Array(declarator, size) => {
                declarator.apply_to_type(Type::Array(Box::new(base_type), size))
            }
        }
    }
}
