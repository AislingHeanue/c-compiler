use super::{
    expect, peek, read, AbstractDeclarator, BinaryOperatorNode, ExpressionNode,
    ExpressionWithoutType, Parse, ParseContext, Type, UnaryOperatorNode,
};
use crate::compiler::{lexer::Token, types::Constant};
use std::{collections::VecDeque, error::Error};

impl Parse for ExpressionNode {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(ExpressionWithoutType::parse(tokens, context)?.into())
    }
}

impl Parse for Option<ExpressionNode> {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(
            ExpressionWithoutType::parse_with_level(tokens, context, 0, true)?
                .map(|res| res.into()),
        )
    }
}

impl ExpressionNode {
    pub fn is_string_literal(&self) -> bool {
        matches!(self.0, ExpressionWithoutType::String(_))
    }
}

impl From<ExpressionWithoutType> for ExpressionNode {
    fn from(val: ExpressionWithoutType) -> Self {
        ExpressionNode(val, None)
    }
}
impl Parse for ExpressionWithoutType {
    fn parse(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(ExpressionWithoutType::parse_with_level(tokens, context, 0, false)?.unwrap())
    }
}

impl ExpressionWithoutType {
    fn parse_with_level(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        level: usize,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let maybe_left = ExpressionWithoutType::parse_unary(tokens, context, allow_empty)?;
        if maybe_left.is_none() {
            // if allow_empty is false, parse_unary will throw the relevant error for here
            return Ok(None);
        }
        let mut left = maybe_left.unwrap();
        while let Some(precedence) = BinaryOperatorNode::precedence(tokens)? {
            if precedence < level {
                break;
            }
            if peek(tokens)?.is_assignment() {
                let operator_token = read(tokens)?;
                let right =
                    ExpressionWithoutType::parse_with_level(tokens, context, precedence, false)?;
                left = if let Token::Assignment = operator_token {
                    ExpressionWithoutType::Assignment(
                        Box::new(left.into()),
                        Box::new(right.unwrap().into()),
                    )
                } else {
                    let operator = match operator_token {
                        Token::AddAssign => BinaryOperatorNode::Add,
                        Token::SubtractAssign => BinaryOperatorNode::Subtract,
                        Token::MultiplyAssign => BinaryOperatorNode::Multiply,
                        Token::DivideAssign => BinaryOperatorNode::Divide,
                        Token::ModAssign => BinaryOperatorNode::Mod,
                        Token::BitwiseAndAssign => BinaryOperatorNode::BitwiseAnd,
                        Token::BitwiseXorAssign => BinaryOperatorNode::BitwiseXor,
                        Token::BitwiseOrAssign => BinaryOperatorNode::BitwiseOr,
                        Token::ShiftLeftAssign => BinaryOperatorNode::ShiftLeft,
                        Token::ShiftRightAssign => BinaryOperatorNode::ShiftRight,
                        _ => {
                            unreachable!("Can't use {:?} as an assignment operator", operator_token)
                        }
                    };
                    ExpressionWithoutType::Compound(
                        operator,
                        Box::new(left.into()),
                        Box::new(right.unwrap().into()),
                    )
                };
            } else {
                match peek(tokens)? {
                    Token::Question => {
                        expect(tokens, Token::Question)?;
                        // parse this expression with precedence level reset
                        let middle = ExpressionWithoutType::parse(tokens, context)?;
                        expect(tokens, Token::Colon)?;
                        let end = ExpressionWithoutType::parse_with_level(
                            tokens, context, precedence, false,
                        )?;
                        left = ExpressionWithoutType::Ternary(
                            Box::new(left.into()),
                            Box::new(middle.into()),
                            Box::new(end.unwrap().into()),
                        )
                    }
                    _ => {
                        left = ExpressionWithoutType::Binary(
                            BinaryOperatorNode::parse(tokens, context)?,
                            Box::new(left.into()),
                            Box::new(
                                ExpressionWithoutType::parse_with_level(
                                    tokens,
                                    context,
                                    precedence + 1,
                                    false,
                                )?
                                .unwrap()
                                .into(),
                            ),
                        );
                    }
                }
            }
        }

        Ok(Some(left))
    }

    fn parse_unary(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let expression = {
            match peek(tokens)? {
                Token::OpenParen => {
                    // casting
                    expect(tokens, Token::OpenParen)?;
                    if peek(tokens)?.is_type() {
                        let cast_type = Type::parse(tokens, context)?;
                        let abstract_declarator = AbstractDeclarator::parse(tokens, context)?;
                        let real_cast_type = abstract_declarator.apply_to_type(cast_type)?;
                        expect(tokens, Token::CloseParen)?;
                        let factor =
                            ExpressionWithoutType::parse_unary(tokens, context, false)?.unwrap();
                        Some(ExpressionWithoutType::Cast(
                            real_cast_type,
                            Box::new(factor.into()),
                        ))
                    } else {
                        // baited, not actually a cast, go further down the chain...
                        tokens.push_front(Token::OpenParen);
                        let expression =
                            ExpressionWithoutType::parse_postfix(tokens, context, allow_empty)?
                                .unwrap();
                        Some(expression)
                    }
                }
                // address-of
                Token::BitwiseAnd => {
                    expect(tokens, Token::BitwiseAnd)?;
                    let precedence = UnaryOperatorNode::precedence();
                    let inner_expression = ExpressionWithoutType::parse_with_level(
                        tokens, context, precedence, false,
                    )?;
                    Some(ExpressionWithoutType::AddressOf(Box::new(
                        inner_expression.unwrap().into(),
                    )))
                }
                // deref
                Token::Star => {
                    expect(tokens, Token::Star)?;
                    let precedence = UnaryOperatorNode::precedence();
                    let inner_expression = ExpressionWithoutType::parse_with_level(
                        tokens, context, precedence, false,
                    )?;
                    Some(ExpressionWithoutType::Dereference(Box::new(
                        inner_expression.unwrap().into(),
                    )))
                }
                _ if peek(tokens)?.is_unary_operator() => {
                    let operator = UnaryOperatorNode::parse(tokens, context)?;
                    let precedence = UnaryOperatorNode::precedence(); // all unary operators have the
                                                                      // same precedence
                    let expression = ExpressionWithoutType::parse_with_level(
                        tokens, context, precedence, false,
                    )?;
                    Some(ExpressionWithoutType::Unary(
                        operator,
                        Box::new(expression.unwrap().into()),
                    ))
                }
                _ => ExpressionWithoutType::parse_postfix(tokens, context, allow_empty)?,
            }
        };
        Ok(expression)
    }

    fn parse_postfix(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let mut left = ExpressionWithoutType::parse_primary(tokens, context, allow_empty)?;
        if left.is_none() {
            return Ok(left);
        }
        while peek(tokens)?.is_suffix_operator() {
            match peek(tokens)? {
                Token::Increment | Token::Decrement => {
                    left = Some(ExpressionWithoutType::Unary(
                        UnaryOperatorNode::parse_as_suffix(tokens, context)?,
                        Box::new(left.unwrap().into()),
                    ));
                }
                Token::OpenSquareBracket => {
                    expect(tokens, Token::OpenSquareBracket)?;
                    let inner = ExpressionWithoutType::parse(tokens, context)?;
                    expect(tokens, Token::CloseSquareBracket)?;
                    left = Some(ExpressionWithoutType::Subscript(
                        Box::new(left.unwrap().into()),
                        Box::new(inner.into()),
                    ))
                }
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_primary(
        tokens: &mut VecDeque<Token>,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<Self>, Box<dyn Error>> {
        let expression = match peek(tokens)? {
            Token::Identifier(name) => {
                expect(tokens, Token::Identifier("".to_string()))?;
                match peek(tokens)? {
                    // this is a function call !!
                    Token::OpenParen => {
                        let (new_name, _external_link) =
                            ExpressionWithoutType::resolve_identifier(&name, context)?;

                        expect(tokens, Token::OpenParen)?;
                        let mut arguments: Vec<ExpressionWithoutType> = Vec::new();
                        if !matches!(peek(tokens)?, Token::CloseParen) {
                            arguments.push(ExpressionWithoutType::parse(tokens, context)?);
                        }
                        while matches!(peek(tokens)?, Token::Comma) {
                            expect(tokens, Token::Comma)?;
                            arguments.push(ExpressionWithoutType::parse(tokens, context)?);
                        }
                        expect(tokens, Token::CloseParen)?;

                        Some(ExpressionWithoutType::FunctionCall(
                            new_name,
                            arguments.into_iter().map(|a| a.into()).collect(),
                        ))
                    }
                    _ => {
                        // VALIDATION STEP: Check the variable has been declared
                        let (new_name, _external_link) =
                            ExpressionWithoutType::resolve_identifier(&name, context)?;

                        Some(ExpressionWithoutType::Var(new_name))
                    }
                }
            }
            Token::OpenParen => {
                expect(tokens, Token::OpenParen)?;
                let expression = ExpressionWithoutType::parse(tokens, context)?;
                expect(tokens, Token::CloseParen)?;
                Some(expression)
            }
            Token::StringLiteral(v) => {
                expect(tokens, Token::StringLiteral(Vec::new()))?;
                let mut out = v.clone();
                while matches!(peek(tokens)?, Token::StringLiteral(_)) {
                    if let Token::StringLiteral(new_v) = read(tokens)? {
                        out.append(&mut new_v.clone())
                    } else {
                        unreachable!()
                    }
                }

                Some(ExpressionWithoutType::String(out))
            }
            _ if peek(tokens)?.is_constant() => Some(ExpressionWithoutType::Constant(
                Constant::parse(tokens, context)?,
            )),
            t => {
                if allow_empty {
                    None
                } else {
                    return Err(format!("Invalid token at start of expression: {:?}", t).into());
                }
            }
        };

        Ok(expression)
    }

    pub fn match_lvalue(&self) -> bool {
        matches!(
            self,
            ExpressionWithoutType::Var(_)
                | ExpressionWithoutType::Dereference(_)
                | ExpressionWithoutType::Subscript(_, _)
                | ExpressionWithoutType::String(_) // this allows dereferencing a string
        )
    }

    fn resolve_identifier(
        name: &str,
        context: &mut ParseContext,
    ) -> Result<(String, bool), Box<dyn Error>> {
        // println!(
        //     "resolve {:?} {:?}",
        //     context.outer_scope_identifiers, context.current_scope_identifiers
        // );
        if context.do_not_validate {
            Ok((name.to_string(), false))
        } else if let Some(new_name) = context.current_scope_identifiers.get(name) {
            Ok(new_name.clone())
        } else if let Some(new_name) = context.outer_scope_identifiers.get(name) {
            Ok(new_name.clone())
        } else {
            Err(format!("Identifier used before declaration: {}", name).into())
        }
    }
}
