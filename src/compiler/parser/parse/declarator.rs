use itertools::{process_results, Itertools};

use super::{expression_node::PopForExpression, Declarator, Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{BlockItemNode, DeclaratorsWithAssignment, DeclaratorsWithInline, InlineDeclaration},
};
use std::{collections::VecDeque, error::Error};

impl Parse<DeclaratorsWithInline> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<DeclaratorsWithInline, Box<dyn Error>> {
        let (declarator, mut inlines) = self.parse_full_declarator(context)?;
        let mut declarators = DeclaratorsWithAssignment(vec![(declarator, VecDeque::new())]);

        while !self.is_empty()
            && matches!(
                self.peek()?,
                Token::Comma | Token::Assignment | Token::Colon
            )
        {
            match self.peek()? {
                Token::Comma => {
                    self.expect(Token::Comma)?;
                    let (declarator, mut new_inlines) = self.parse_full_declarator(context)?;
                    declarators.0.push((declarator, VecDeque::new()));
                    inlines.append(&mut new_inlines);
                }
                Token::Assignment if declarators.0.last().unwrap().1.is_empty() => {
                    self.expect(Token::Assignment)?;
                    let init_tokens = self.pop_tokens_for_expression(false, true, context)?;
                    declarators.0.last_mut().unwrap().1 = init_tokens;
                }
                // ignore bit fields
                Token::Colon => {
                    self.expect(Token::Colon)?;
                    self.expect(Token::IntegerConstant(0))?;
                }
                _ => unreachable!(),
            }
        }

        Ok((declarators, inlines))
    }
}

#[derive(Debug)]
pub struct DeclaratorApplicationOutput {
    pub name: Option<String>,
    pub out_type: Type,
    pub param_names: Option<Vec<String>>,
    pub init: VecDeque<Token>,
}

pub type ParamList = (Vec<(Type, Declarator)>, Vec<InlineDeclaration>, bool);

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
    ) -> Result<(Type, DeclaratorsWithAssignment, Vec<InlineDeclaration>), Box<dyn Error>>;
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
                            let (declarator, structs) = self.parse_full_declarator(context)?;
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
                            let (declarator, structs) = self.parse_full_declarator(context)?;
                            Ok((Declarator::ConstPointer(Box::new(declarator)), structs))
                        }
                    }
                    Ok(Token::KeywordVolatile) => {
                        // NOTE: Volatile discarded
                        self.expect(Token::KeywordVolatile)?;
                        if self.is_empty() {
                            Ok((
                                Declarator::Pointer(Box::new(Declarator::Base), false),
                                Vec::new(),
                            ))
                        } else {
                            let (declarator, structs) = self.parse_full_declarator(context)?;
                            Ok((Declarator::Pointer(Box::new(declarator), false), structs))
                        }
                    }
                    _ => {
                        if self.is_empty() {
                            Ok((
                                Declarator::Pointer(Box::new(Declarator::Base), false),
                                Vec::new(),
                            ))
                        } else {
                            let (declarator, structs) = self.parse_full_declarator(context)?;
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
                        if matches!(simple_declarator, Declarator::Function(_, _, _)) {
                            return Err("A function cannot return another function".into());
                        }

                        // enter a new scope for params!
                        let scopes = BlockItemNode::enter_scope(context);

                        let (param_list, mut structs_from_params, is_variadic) =
                            self.parse_param_list(context)?;

                        BlockItemNode::leave_scope(scopes, context);

                        structs.append(&mut structs_from_params);
                        Ok((
                            Declarator::Function(
                                Box::new(simple_declarator),
                                param_list,
                                is_variadic,
                            ),
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
                let (declarator, structs) = self.parse_full_declarator(context)?;
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
    ) -> Result<(Vec<(Type, Declarator)>, Vec<InlineDeclaration>, bool), Box<dyn Error>> {
        self.expect(Token::OpenParen)?;
        let mut param_list = Vec::new();
        let mut structs = Vec::new();

        if self.peek()? == Token::KeywordVoid {
            self.expect(Token::KeywordVoid)?;
            if matches!(self.peek()?, Token::CloseParen) {
                self.expect(Token::CloseParen)?;
                return Ok((param_list, structs, false));
            } else {
                // void wasn't in parentheses by itself, so keep scanning param list
                self.push_front(Token::KeywordVoid)
            }
        }

        let mut is_variadic = false;
        // let (t, param, mut structs_from_param) = self.parse_param(context)?;
        // param_list.push((t, param));
        // structs.append(&mut structs_from_param);
        let mut found_arg = false;
        while self.peek()? != Token::CloseParen {
            if found_arg {
                self.expect(Token::Comma)?;
            }
            if matches!(self.peek()?, Token::Ellipses) {
                self.expect(Token::Ellipses)?;
                is_variadic = true;
                break;
            }
            let (t, params, mut structs_from_param) = self.parse_param(context)?;
            for param in params.0.into_iter() {
                if !param.1.is_empty() {
                    return Err("Function parameter cannot have an assignment".into());
                }
                param_list.push((t.clone(), param.0));
            }
            structs.append(&mut structs_from_param);
            found_arg = true
        }
        self.expect(Token::CloseParen)?;
        Ok((param_list, structs, is_variadic))
    }

    fn parse_param(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, DeclaratorsWithAssignment, Vec<InlineDeclaration>), Box<dyn Error>> {
        context.parsing_param = true;
        let out = self.parse(context);
        context.parsing_param = false;
        out
    }

    fn parse_array(
        &mut self,
        mut declarator: Declarator,
        context: &mut ParseContext,
    ) -> Result<Declarator, Box<dyn Error>> {
        self.expect(Token::OpenSquareBracket)?;
        if context.parsing_param {
            let mut is_restrict = false;
            let mut is_const = false;
            let mut _is_volatile = false;
            let mut _is_static = false;
            while !self.is_empty()
                && matches!(
                    self.peek()?,
                    Token::KeywordRestrict
                        | Token::KeywordConst
                        | Token::KeywordVolatile
                        | Token::KeywordStatic
                )
            {
                match self.read()? {
                    Token::KeywordRestrict if !is_restrict => is_restrict = true,
                    Token::KeywordConst if !is_const => is_const = true,
                    Token::KeywordVolatile if !_is_volatile => _is_volatile = true,
                    Token::KeywordStatic if !_is_static => _is_static = true,
                    _ => return Err("Invalid specifiers in array declarator in parameter".into()),
                }
            }
            // let _e: Option<ExpressionNode> = self.parse(context)?;
            // if is_const {
            //     declarator = Declarator::ConstPointer(Box::new(declarator))
            // } else {
            //     declarator = Declarator::Pointer(Box::new(declarator), is_restrict)
            // }
            // self.expect(Token::CloseSquareBracket)?;
            // } else {
        }
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
        init: VecDeque<Token>,
        _context: &mut ParseContext,
        // returns the type, name and list of parameter names associated with the type
    ) -> Result<DeclaratorApplicationOutput, Box<dyn Error>> {
        match self {
            Declarator::Name(name) => Ok(DeclaratorApplicationOutput {
                out_type: base_type,
                name: Some(name),
                param_names: None,
                init,
            }),
            Declarator::Base => Ok(DeclaratorApplicationOutput {
                out_type: base_type,
                name: None,
                param_names: None,
                init,
            }),
            Declarator::Pointer(declarator, is_restricted) => declarator.apply_to_type(
                Type::Pointer(Box::new(base_type), is_restricted),
                init,
                _context,
            ),
            Declarator::ConstPointer(declarator) => {
                // TODO: this const is thrown out the window. Once there's a mechanism to const-ify
                // a type, it needs to be added here
                declarator.apply_to_type(Type::Pointer(Box::new(base_type), false), init, _context)
            }
            Declarator::Function(declarator, params, is_variadic) => {
                let mut num_anonymous_params = 0;
                let (param_types, param_names): (Vec<Type>, Vec<String>) = process_results(
                    params.into_iter().map(|(t, declarator)| {
                        declarator.apply_to_type(t, VecDeque::new(), _context)
                    }),
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
                    if !init.is_empty() {
                        return Err("Function declarator may not have an assigned value".into());
                    }
                    Ok(DeclaratorApplicationOutput {
                        out_type: Type::Function(Box::new(base_type), param_types, is_variadic),
                        name: Some(name),
                        param_names: Some(param_names),
                        init: VecDeque::new(),
                    })
                } else {
                    // discard param names, function pointers will never use them directly
                    // because we know this isn't a function definition with a body.
                    declarator.apply_to_type(
                        Type::Function(Box::new(base_type), param_types, is_variadic),
                        init,
                        _context,
                    )
                }
            }
            Declarator::Array(declarator, size_expression) => {
                if let Some(expression) = size_expression {
                    let size = expression.0.fold_to_constant(&None)?.value_unsigned();
                    declarator.apply_to_type(
                        Type::Array(Box::new(base_type), Some(size)),
                        init,
                        _context,
                    )
                } else {
                    declarator.apply_to_type(Type::Array(Box::new(base_type), None), init, _context)
                }
            }
        }
    }
}

impl DeclaratorsWithAssignment {
    pub fn apply_to_type(
        self,
        base_type: Type,
        context: &mut ParseContext,
    ) -> Result<Vec<DeclaratorApplicationOutput>, Box<dyn Error>> {
        process_results(
            self.0.into_iter().map(|(declarator, init)| {
                declarator.apply_to_type(base_type.clone(), init, context)
            }),
            |a| a.collect_vec(),
        )
    }
}
