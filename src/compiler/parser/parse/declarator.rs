use itertools::process_results;

use super::{parsed_types::PopForType, Declarator, Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::StructDeclaration,
};
use std::{collections::VecDeque, error::Error};

impl Parse<Declarator> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<Declarator, Box<dyn Error>> {
        let declarator = self.parse_full_declarator(context)?;

        Ok(declarator)
    }
}

#[derive(Debug)]
pub struct DeclaratorApplicationOutput {
    pub name: String,
    pub out_type: Type,
    pub param_names: Option<Vec<String>>,
    pub struct_declarations: Vec<StructDeclaration>,
}

pub type OutputWithStruct = (Type, Declarator, Option<StructDeclaration>);

trait ParseDeclarator {
    fn parse_full_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>>;
    fn parse_simple_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>>;

    fn parse_param_list(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Vec<OutputWithStruct>, Box<dyn Error>>;
    fn parse_param(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<OutputWithStruct, Box<dyn Error>>;
    fn parse_array(
        &mut self,
        declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>>;
}

impl ParseDeclarator for VecDeque<Token> {
    fn parse_full_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        match self.peek()? {
            Token::Star => {
                self.expect(Token::Star)?;
                match self.peek()? {
                    Token::KeywordRestrict => {
                        self.expect(Token::KeywordRestrict)?;
                        Ok(Declarator::Pointer(Box::new(self.parse(context)?), true))
                    }
                    _ => Ok(Declarator::Pointer(Box::new(self.parse(context)?), false)),
                }
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
    ) -> Result<Vec<OutputWithStruct>, Box<dyn Error>> {
        self.expect(Token::OpenParen)?;
        let mut param_list = Vec::new();

        if self.peek()? == Token::KeywordVoid {
            self.expect(Token::KeywordVoid)?;
            if matches!(self.peek()?, Token::CloseParen) {
                self.expect(Token::CloseParen)?;
                return Ok(param_list);
            } else {
                // void wasn't in parentheses by itself, so keep scanning param list
                self.push_front(Token::KeywordVoid)
            }
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
    ) -> Result<OutputWithStruct, Box<dyn Error>> {
        // check whether there is a declarator present in this param
        let mut copy_remaining_tokens_after_type = self.clone();
        copy_remaining_tokens_after_type.pop_tokens_for_type(context)?;

        let is_anonymous = matches!(
            copy_remaining_tokens_after_type.front(),
            None | Some(Token::Comma | Token::CloseParen)
        );

        if !is_anonymous {
            self.parse(context)
        } else {
            let (t, s): (Type, Option<StructDeclaration>) = self.parse(context)?;
            context.num_anonymous_params += 1;
            let declarator = Declarator::Name(format!(
                "anonymous.parameter.{}",
                context.num_anonymous_params
            ));
            Ok((t, declarator, s))
        }
        // function params never have static or extern storage
    }

    fn parse_array(
        &mut self,
        mut declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        self.expect(Token::OpenSquareBracket)?;
        let e = self.parse(context)?;
        declarator = Declarator::Array(Box::new(declarator), e);
        self.expect(Token::CloseSquareBracket)?;
        Ok(declarator)
    }
}

impl Declarator {
    pub fn apply_to_type(
        self,
        base_type: Type,
        context: &mut ParseContext,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<DeclaratorApplicationOutput, Box<dyn Error>> {
        match self {
            Declarator::Name(name) => Ok(DeclaratorApplicationOutput {
                out_type: base_type,
                name,
                param_names: None,
                struct_declarations: Vec::new(),
            }),
            Declarator::Pointer(declarator, is_restricted) => {
                // discard param names, function pointers aren't real
                declarator.apply_to_type(Type::Pointer(Box::new(base_type), is_restricted), context)
            }
            Declarator::Function(declarator, params) => {
                let name = if let Declarator::Name(name) = *declarator {
                    name
                } else {
                    return Err("Cannot apply additional declarators to a function type (function pointers aren't real)".into());
                };
                let struct_declarations: Vec<StructDeclaration> = params
                    .iter()
                    .filter_map(|(_, _, struct_declaration)| struct_declaration.clone())
                    .collect();
                let (param_types, param_names): (Vec<Type>, Vec<String>) = process_results(
                    params
                        .into_iter()
                        .map(|(t, declarator, _struct_declaration)| {
                            declarator.apply_to_type(t, context)
                        }),
                    |iter| iter.map(|o| (o.out_type, o.name)).unzip(),
                )?;

                Ok(DeclaratorApplicationOutput {
                    out_type: Type::Function(Box::new(base_type), param_types),
                    name,
                    param_names: Some(param_names),
                    struct_declarations,
                })
            }
            Declarator::Array(declarator, size_expression) => {
                let size = size_expression.fold_to_constant(context)?.value_unsigned();
                declarator.apply_to_type(Type::Array(Box::new(base_type), size), context)
            }
        }
    }
}
