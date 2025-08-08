use itertools::process_results;

use super::{expect, peek, read, Declarator, Parse, ParseContext, Type};
use crate::compiler::{lexer::Token, types::Constant};
use std::{collections::VecDeque, error::Error};

impl Parse for Declarator {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        match peek(tokens)? {
            Token::Star => {
                expect(tokens, Token::Star)?;
                Ok(Declarator::Pointer(Box::new(Declarator::parse(
                    tokens, context,
                )?)))
            }
            _ => {
                let mut simple_declarator = Declarator::parse_simple_declarator(tokens, context)?;
                match peek(tokens)? {
                    // function declaration type!
                    Token::OpenParen => {
                        let param_list = Declarator::parse_param_list(tokens, context)?;
                        Ok(Declarator::Function(
                            Box::new(simple_declarator),
                            param_list,
                        ))
                    }
                    Token::OpenSquareBracket => {
                        while matches!(peek(tokens)?, Token::OpenSquareBracket) {
                            simple_declarator =
                                Declarator::parse_array(tokens, simple_declarator, context)?;
                        }
                        Ok(simple_declarator)
                    }
                    _ => Ok(simple_declarator),
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

impl Declarator {
    fn parse_simple_declarator(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        match read(tokens)? {
            Token::Identifier(name) => {
                // do not process the scope of 'name' here, because it complicates parameters
                Ok(Declarator::Name(name))
            }
            Token::OpenParen => {
                let declarator = Declarator::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Ok(declarator)
            }
            _ => Err("Invalid declarator".into()),
        }
    }

    fn parse_param_list(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Vec<(Type, Declarator)>, Box<dyn Error>> {
        expect(tokens, Token::OpenParen)?;
        let mut param_list = Vec::new();

        if peek(tokens)? == Token::KeywordVoid {
            expect(tokens, Token::KeywordVoid)?;
            expect(tokens, Token::CloseParen)?;
            return Ok(param_list);
        }

        param_list.push(Declarator::parse_param(tokens, context)?);
        while peek(tokens)? != Token::CloseParen {
            expect(tokens, Token::Comma)?;
            param_list.push(Declarator::parse_param(tokens, context)?);
        }
        expect(tokens, Token::CloseParen)?;
        Ok(param_list)
    }

    fn parse_param(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<(Type, Declarator), Box<dyn Error>> {
        let out_type = Type::parse(tokens, context)?;
        // function params never have static or extern storage
        let declarator = Declarator::parse(tokens, context)?;
        Ok((out_type, declarator))
    }

    fn parse_array(
        tokens: &mut VecDeque<Token>,
        mut declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
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

            declarator = Declarator::Array(Box::new(declarator), i);
        } else {
            return Err("Constant value in array type must be an integer".into());
        }
        expect(tokens, Token::CloseSquareBracket)?;
        Ok(declarator)
    }

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
