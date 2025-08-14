use super::{
    AbstractDeclarator, BinaryOperatorNode, ExpressionNode, ExpressionWithoutType, Identity, Parse,
    ParseContext, Type, UnaryOperatorNode,
};
use crate::compiler::lexer::{Token, TokenVector};
use std::{collections::VecDeque, error::Error};

impl Parse<ExpressionNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<ExpressionNode, Box<dyn Error>> {
        let e1: ExpressionWithoutType = self.parse(context)?;
        Ok(e1.into())
    }
}

impl Parse<Option<ExpressionNode>> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<Option<ExpressionNode>, Box<dyn Error>> {
        Ok(self
            .parse_with_level(context, 0, true)?
            .map(|res| res.into()))
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
impl Parse<ExpressionWithoutType> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<ExpressionWithoutType, Box<dyn Error>> {
        Ok(self.parse_with_level(context, 0, false)?.unwrap())
    }
}
trait ParseExpression {
    fn parse_primary(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>>;
    fn parse_postfix(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>>;
    fn parse_unary(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>>;
    fn parse_with_level(
        &mut self,
        context: &mut ParseContext,
        level: usize,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>>;
    fn parse_cast(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>>;
}

impl ParseExpression for VecDeque<Token> {
    fn parse_cast(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>> {
        let out = match self.peek()? {
            Token::OpenParen => {
                // casting
                self.expect(Token::OpenParen)?;
                if self.peek()?.is_start_of_declaration(context) {
                    let cast_type: Type = self.parse(context)?;
                    let abstract_declarator: AbstractDeclarator = self.parse(context)?;
                    let real_cast_type = abstract_declarator.apply_to_type(cast_type)?;
                    self.expect(Token::CloseParen)?;
                    let factor = self.parse_cast(context, false)?.unwrap();
                    Some(ExpressionWithoutType::Cast(
                        real_cast_type,
                        Box::new(factor.into()),
                    ))
                } else {
                    // baited, not actually a cast, go further down the chain...
                    self.push_front(Token::OpenParen);
                    let expression = self.parse_postfix(context, allow_empty)?.unwrap();
                    Some(expression)
                }
            }
            _ => self.parse_unary(context, allow_empty)?,
        };
        Ok(out)
    }

    fn parse_with_level(
        &mut self,
        context: &mut ParseContext,
        level: usize,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>> {
        let maybe_left = self.parse_cast(context, allow_empty)?;
        if maybe_left.is_none() {
            // if allow_empty is false, parse_cast will throw the relevant error for here
            return Ok(None);
        }
        let mut left = maybe_left.unwrap();
        while let Some(precedence) = BinaryOperatorNode::precedence(self)? {
            if precedence < level {
                break;
            }
            if self.peek()?.is_assignment() {
                let operator_token = self.read()?;
                let right = self.parse_with_level(context, precedence, false)?;
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
                        None,
                    )
                };
            } else {
                match self.peek()? {
                    Token::Question => {
                        self.expect(Token::Question)?;
                        // parse this expression with precedence level reset
                        let middle: ExpressionWithoutType = self.parse(context)?;
                        self.expect(Token::Colon)?;
                        let end = self.parse_with_level(context, precedence, false)?;
                        left = ExpressionWithoutType::Ternary(
                            Box::new(left.into()),
                            Box::new(middle.into()),
                            Box::new(end.unwrap().into()),
                        )
                    }
                    _ => {
                        left = ExpressionWithoutType::Binary(
                            self.parse(context)?,
                            Box::new(left.into()),
                            Box::new(
                                self.parse_with_level(context, precedence + 1, false)?
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
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>> {
        let expression = {
            match self.peek()? {
                // address-of
                Token::BitwiseAnd => {
                    self.expect(Token::BitwiseAnd)?;
                    let inner_expression = self.parse_cast(context, false)?;
                    Some(ExpressionWithoutType::AddressOf(Box::new(
                        inner_expression.unwrap().into(),
                    )))
                }
                // deref
                Token::Star => {
                    self.expect(Token::Star)?;
                    let inner_expression = self.parse_cast(context, false)?;
                    Some(ExpressionWithoutType::Dereference(Box::new(
                        inner_expression.unwrap().into(),
                    )))
                }
                Token::KeywordSizeof => {
                    self.expect(Token::KeywordSizeof)?;
                    match self.peek()? {
                        Token::OpenParen => {
                            // casting
                            self.expect(Token::OpenParen)?;
                            if self.peek()?.is_start_of_declaration(context) {
                                let target_type: Type = self.parse(context)?;
                                let abstract_declarator: AbstractDeclarator =
                                    self.parse(context)?;
                                let real_target_type =
                                    abstract_declarator.apply_to_type(target_type)?;
                                self.expect(Token::CloseParen)?;
                                Some(ExpressionWithoutType::SizeOfType(real_target_type))
                            } else {
                                self.push_front(Token::OpenParen);
                                let expression = self.parse_postfix(context, allow_empty)?.unwrap();
                                Some(ExpressionWithoutType::SizeOf(Box::new(expression.into())))
                            }
                        }
                        _ => {
                            let expression = self.parse_unary(context, allow_empty)?.unwrap();
                            Some(ExpressionWithoutType::SizeOf(Box::new(expression.into())))
                        }
                    }
                }
                _ if self.peek()?.is_unary_operator() => {
                    let operator = self.parse(context)?;
                    // unary expressions have a precedence of 55, second only to suffix increment
                    // and decrement, which are handled in parse_postfix.
                    let expression = self.parse_cast(context, false)?;
                    Some(ExpressionWithoutType::Unary(
                        operator,
                        Box::new(expression.unwrap().into()),
                    ))
                }
                _ => self.parse_postfix(context, allow_empty)?,
            }
        };
        Ok(expression)
    }

    fn parse_postfix(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>> {
        let mut left = self.parse_primary(context, allow_empty)?;
        if left.is_none() {
            return Ok(left);
        }
        while self.peek()?.is_suffix_operator() {
            match self.peek()? {
                Token::Increment | Token::Decrement => {
                    left = Some(ExpressionWithoutType::Unary(
                        UnaryOperatorNode::parse_as_suffix(self, context)?,
                        Box::new(left.unwrap().into()),
                    ));
                }
                Token::OpenSquareBracket => {
                    self.expect(Token::OpenSquareBracket)?;
                    let inner: ExpressionWithoutType = self.parse(context)?;
                    self.expect(Token::CloseSquareBracket)?;
                    left = Some(ExpressionWithoutType::Subscript(
                        Box::new(left.unwrap().into()),
                        Box::new(inner.into()),
                    ))
                }
                Token::Dot => {
                    self.expect(Token::Dot)?;
                    let loc = if let Token::Identifier(s) = self.read()? {
                        s.clone()
                    } else {
                        return Err("'.' must be followed by an identifier".into());
                    };
                    left = Some(ExpressionWithoutType::Dot(
                        Box::new(left.unwrap().into()),
                        loc,
                    ));
                }
                Token::Arrow => {
                    self.expect(Token::Arrow)?;
                    let loc = if let Token::Identifier(s) = self.read()? {
                        s.clone()
                    } else {
                        return Err("'->' must be followed by an identifier".into());
                    };
                    left = Some(ExpressionWithoutType::Arrow(
                        Box::new(left.unwrap().into()),
                        loc,
                    ));
                }
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_primary(
        &mut self,
        context: &mut ParseContext,
        allow_empty: bool,
    ) -> Result<Option<ExpressionWithoutType>, Box<dyn Error>> {
        let expression = match self.peek()? {
            Token::Identifier(name) => {
                self.expect(Token::Identifier("".to_string()))?;
                match self.peek()? {
                    // this is a function call !!
                    Token::OpenParen => {
                        let (new_name, _external_link) =
                            ExpressionWithoutType::resolve_identifier(&name, context)?;

                        self.expect(Token::OpenParen)?;
                        let mut arguments: Vec<ExpressionWithoutType> = Vec::new();
                        if !matches!(self.peek()?, Token::CloseParen) {
                            arguments.push(self.parse(context)?);
                        }
                        while matches!(self.peek()?, Token::Comma) {
                            self.expect(Token::Comma)?;
                            arguments.push(self.parse(context)?);
                        }
                        self.expect(Token::CloseParen)?;

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
                self.expect(Token::OpenParen)?;
                let expression = self.parse(context)?;
                self.expect(Token::CloseParen)?;
                Some(expression)
            }
            Token::StringLiteral(v) => {
                self.expect(Token::StringLiteral(Vec::new()))?;
                let mut out = v.clone();
                while matches!(self.peek()?, Token::StringLiteral(_)) {
                    if let Token::StringLiteral(new_v) = self.read()? {
                        out.append(&mut new_v.clone())
                    } else {
                        unreachable!()
                    }
                }

                Some(ExpressionWithoutType::String(out))
            }
            _ if self.peek()?.is_constant() => {
                Some(ExpressionWithoutType::Constant(self.parse(context)?))
            }
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
}

impl ExpressionWithoutType {
    pub fn is_lvalue(&self) -> bool {
        if let ExpressionWithoutType::Dot(left, _right) = self {
            left.0.is_lvalue()
        } else {
            matches!(
                self,
                ExpressionWithoutType::Var(_)
                | ExpressionWithoutType::Dereference(_)
                | ExpressionWithoutType::Subscript(_, _)
                | ExpressionWithoutType::String(_) // this allows dereferencing a string
                | ExpressionWithoutType::Arrow(_, _)
            )
        }
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
        } else if let Some(identity) = context.current_scope_identifiers.get(name) {
            if let Identity::Variable(new_name, link) = identity {
                Ok((new_name.clone(), *link))
            } else {
                Err(format!(
                    "Identifier '{}' is a type alias in the current scope, not a variable",
                    name
                )
                .into())
            }
        } else if let Some(identity) = context.outer_scope_identifiers.get(name) {
            if let Identity::Variable(new_name, link) = identity {
                Ok((new_name.clone(), *link))
            } else {
                Err(format!(
                    "Identifier '{}' is a type alias in the parent scope, not a variable",
                    name
                )
                .into())
            }
        } else {
            Err(format!("Identifier used before declaration: {}", name).into())
        }
    }

    pub fn resolve_type_alias(
        name: &str,
        context: &mut ParseContext,
    ) -> Result<Type, Box<dyn Error>> {
        // println!(
        //     "resolve {:?} {:?}",
        //     context.outer_scope_identifiers, context.current_scope_identifiers
        // );
        if let Some(identity) = context.current_scope_identifiers.get(name) {
            if let Identity::TypeAlias(t) = identity {
                Ok(t.clone())
            } else {
                Err(format!(
                    "Identifier '{}' is a variable in the current scope, not a type alias",
                    name
                )
                .into())
            }
        } else if let Some(identity) = context.outer_scope_identifiers.get(name) {
            if let Identity::TypeAlias(t) = identity {
                Ok(t.clone())
            } else {
                Err(format!(
                    "Identifier '{}' is a variable in the parent scope, not a type alias",
                    name
                )
                .into())
            }
        } else {
            Err(format!("Identifier used before declaration: {}", name).into())
        }
    }

    pub fn resolve_struct_name(
        name: &str,
        context: &mut ParseContext,
    ) -> Result<String, Box<dyn Error>> {
        // println!(
        //     "resolve {:?} {:?}",
        //     context.outer_struct_names, context.current_struct_names
        // );
        if let Some(info) = context.current_struct_names.get(name) {
            Ok(info.clone())
        } else if let Some(info) = context.outer_struct_names.get(name) {
            Ok(info.clone())
        } else {
            context.num_structs += 1;
            let new_name = format!("{}.{}", name, context.num_structs);

            // anonymous structs do not get saved to the scope symbols list
            if name != "anonymous.struct" {
                context
                    .current_struct_names
                    .insert(name.to_string(), new_name.clone());
            }

            Ok(new_name)
        }
    }

    pub fn new_struct_name(
        name: &str,
        context: &mut ParseContext,
    ) -> Result<String, Box<dyn Error>> {
        // println!(
        //     "resolve for new {:?} {:?}",
        //     context.outer_struct_names, context.current_struct_names
        // );
        if let Some(info) = context.current_struct_names.get(name) {
            Ok(info.clone())
        } else {
            // ignore the outer scope, this is an EXPLICIT declaration of a new struct type
            context.num_structs += 1;
            let new_name = format!("{}.{}", name, context.num_structs);

            // never store anonymous structs in the context
            if name != "anonymous.struct" {
                context
                    .current_struct_names
                    .insert(name.to_string(), new_name.clone());
            }

            Ok(new_name)
        }
    }
}
