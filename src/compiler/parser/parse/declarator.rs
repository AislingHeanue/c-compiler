use itertools::process_results;

use super::{Declarator, Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{BlockItemNode, DeclaratorWithInline, InlineDeclaration},
};
use std::{collections::VecDeque, error::Error};

impl Parse<DeclaratorWithInline> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<DeclaratorWithInline, Box<dyn Error>> {
        let declarator = self.parse_full_declarator(context)?;

        Ok(declarator)
    }
}

#[derive(Debug)]
pub struct DeclaratorApplicationOutput {
    pub name: Option<String>,
    pub out_type: Type,
    pub param_names: Option<Vec<String>>,
}

pub type ParamList = (Vec<(Type, Declarator)>, Vec<InlineDeclaration>);

trait ParseDeclarator {
    fn parse_full_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Declarator, Vec<InlineDeclaration>), Box<dyn Error>>;
    fn parse_simple_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Declarator, Vec<InlineDeclaration>), Box<dyn Error>>;
    fn parse_param_list(&mut self, context: &mut ParseContext)
        -> Result<ParamList, Box<dyn Error>>;
    fn parse_param(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Declarator, Vec<InlineDeclaration>), Box<dyn Error>>;
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
    ) -> Result<(Declarator, Vec<InlineDeclaration>), Box<dyn Error>> {
        if self.is_empty() {
            return Ok((Declarator::Base, Vec::new()));
        }
        match self.peek()? {
            Token::Star => {
                self.expect(Token::Star)?;
                match self.peek() {
                    Ok(Token::KeywordRestrict) => {
                        self.expect(Token::KeywordRestrict)?;
                        if self.is_empty() {
                            Ok((
                                Declarator::Pointer(Box::new(Declarator::Base), true),
                                Vec::new(),
                            ))
                        } else {
                            let (declarator, structs) = self.parse(context)?;
                            Ok((Declarator::Pointer(Box::new(declarator), true), structs))
                        }
                    }
                    Ok(Token::KeywordConst) => {
                        self.expect(Token::KeywordConst)?;
                        if self.is_empty() {
                            Ok((
                                Declarator::ConstPointer(Box::new(Declarator::Base)),
                                Vec::new(),
                            ))
                        } else {
                            let (declarator, structs) = self.parse(context)?;
                            Ok((Declarator::ConstPointer(Box::new(declarator)), structs))
                        }
                    }
                    _ => {
                        if self.is_empty() {
                            Ok((
                                Declarator::Pointer(Box::new(Declarator::Base), false),
                                Vec::new(),
                            ))
                        } else {
                            let (declarator, structs) = self.parse(context)?;
                            Ok((Declarator::Pointer(Box::new(declarator), false), structs))
                        }
                    }
                }
            }
            _ => {
                let (mut simple_declarator, mut structs) = self.parse_simple_declarator(context)?;
                match self.peek() {
                    // function declaration type!
                    Ok(Token::OpenParen) => {
                        if matches!(simple_declarator, Declarator::Function(_, _)) {
                            return Err("A function cannot return another function".into());
                        }

                        // enter a new scope for params!
                        let scopes = BlockItemNode::enter_scope(context);

                        let (param_list, mut structs_from_params) =
                            self.parse_param_list(context)?;

                        BlockItemNode::leave_scope(scopes, context);

                        structs.append(&mut structs_from_params);
                        Ok((
                            Declarator::Function(Box::new(simple_declarator), param_list),
                            structs,
                        ))
                    }
                    Ok(Token::OpenSquareBracket) => {
                        while !self.is_empty() && matches!(self.peek()?, Token::OpenSquareBracket) {
                            simple_declarator = self.parse_array(simple_declarator, context)?;
                        }
                        Ok((simple_declarator, structs))
                    }
                    Ok(_) => Ok((simple_declarator, structs)),
                    // no further tokens found after the declarator, this is okay
                    Err(_) => Ok((simple_declarator, structs)),
                }
            }
        }
    }
    fn parse_simple_declarator(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Declarator, Vec<InlineDeclaration>), Box<dyn Error>> {
        match self.peek() {
            Ok(Token::Identifier(name)) => {
                // do not process the scope of 'name' here, because it complicates parameters
                self.expect(Token::Identifier("".to_string()))?;
                Ok((Declarator::Name(name), Vec::new()))
            }
            Ok(Token::OpenParen) => {
                self.expect(Token::OpenParen)?;
                let (declarator, structs) = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                Ok((declarator, structs))
            }
            // empty list or some other kind of token both mean that this is an abstract declarator
            _ => Ok((Declarator::Base, Vec::new())),
        }
    }

    fn parse_param_list(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Vec<(Type, Declarator)>, Vec<InlineDeclaration>), Box<dyn Error>> {
        self.expect(Token::OpenParen)?;
        let mut param_list = Vec::new();
        let mut structs = Vec::new();

        if self.peek()? == Token::KeywordVoid {
            self.expect(Token::KeywordVoid)?;
            if matches!(self.peek()?, Token::CloseParen) {
                self.expect(Token::CloseParen)?;
                return Ok((param_list, structs));
            } else {
                // void wasn't in parentheses by itself, so keep scanning param list
                self.push_front(Token::KeywordVoid)
            }
        }

        let (t, param, mut structs_from_param) = self.parse_param(context)?;
        param_list.push((t, param));
        structs.append(&mut structs_from_param);
        while self.peek()? != Token::CloseParen {
            self.expect(Token::Comma)?;
            let (t, param, mut structs_from_param) = self.parse_param(context)?;
            param_list.push((t, param));
            structs.append(&mut structs_from_param);
        }
        self.expect(Token::CloseParen)?;
        Ok((param_list, structs))
    }

    fn parse_param(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Declarator, Vec<InlineDeclaration>), Box<dyn Error>> {
        self.parse(context)
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
                name: Some(name),
                param_names: None,
            }),
            Declarator::Base => Ok(DeclaratorApplicationOutput {
                out_type: base_type,
                name: None,
                param_names: None,
            }),
            Declarator::Pointer(declarator, is_restricted) => {
                declarator.apply_to_type(Type::Pointer(Box::new(base_type), is_restricted), context)
            }
            Declarator::ConstPointer(declarator) => {
                // TODO: this const is thrown out the window. Once there's a mechanism to const-ify
                // a type, it needs to be added here
                declarator.apply_to_type(Type::Pointer(Box::new(base_type), false), context)
            }
            Declarator::Function(declarator, params) => {
                let mut num_anonymous_params = 0;
                let (param_types, param_names): (Vec<Type>, Vec<String>) = process_results(
                    params
                        .into_iter()
                        .map(|(t, declarator)| declarator.apply_to_type(t, context)),
                    |iter| {
                        iter.map(|o| {
                            (
                                o.out_type,
                                o.name.unwrap_or_else(|| {
                                    num_anonymous_params += 1;
                                    format!("anonymous.param.{}", num_anonymous_params)
                                }),
                            )
                        })
                        .unzip()
                    },
                )?;

                if let Declarator::Name(name) = *declarator {
                    Ok(DeclaratorApplicationOutput {
                        out_type: Type::Function(Box::new(base_type), param_types),
                        name: Some(name),
                        param_names: Some(param_names),
                    })
                } else {
                    // discard param names, function pointers will never use them directly
                    // because we know this isn't a function definition with a body.
                    declarator
                        .apply_to_type(Type::Function(Box::new(base_type), param_types), context)
                }
            }
            Declarator::Array(declarator, size_expression) => {
                if let Some(expression) = size_expression {
                    let size = expression.0.fold_to_constant(context)?.value_unsigned();
                    declarator.apply_to_type(Type::Array(Box::new(base_type), Some(size)), context)
                } else {
                    declarator.apply_to_type(Type::Array(Box::new(base_type), None), context)
                }
            }
        }
    }
}
