use super::{
    BinaryOperatorNode, ExpressionNode, ExpressionWithoutType, Identity, Parse, ParseContext,
    StructKind, Type, UnaryOperatorNode,
};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{BuiltinVa, DeclaratorsWithAssignment, InlineDeclaration},
};
use std::{collections::VecDeque, error::Error};

impl Parse<ExpressionNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<ExpressionNode, Box<dyn Error>> {
        let e1: ExpressionWithoutType = self.parse(context)?;
        if !self.is_empty()
            && !matches!(
                self.peek()?,
                Token::SemiColon
                    | Token::CloseParen
                    | Token::CloseBrace
                    | Token::CloseSquareBracket
                    | Token::Comma
            )
        {
            return Err("Extra tokens found after expression".into());
        }
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
        ExpressionNode(val, None, false)
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
                    let (cast_type, mut inline_declarations): (Type, Vec<InlineDeclaration>) =
                        self.parse(context)?;
                    let (declarators, mut struct_declarations_from_declarator): (
                        DeclaratorsWithAssignment,
                        Vec<InlineDeclaration>,
                    ) = self.parse(context)?;
                    inline_declarations.append(&mut struct_declarations_from_declarator);
                    let declarator_output = declarators.apply_to_type(cast_type, context)?;
                    if declarator_output.len() != 1 {
                        return Err(
                            "Sizeof must not be used with a comma-separated declarator list".into(),
                        );
                    }
                    if declarator_output[0].name.is_some() {
                        return Err(
                            "Cast must be used with an abstract declarator, but a name was found"
                                .into(),
                        );
                    }
                    self.expect(Token::CloseParen)?;
                    let factor = self.parse_cast(context, false)?.unwrap();
                    Some(ExpressionWithoutType::Cast(
                        declarator_output[0].out_type.clone(),
                        Box::new(factor.into()),
                        inline_declarations,
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
            if !self.is_empty() && self.peek()?.is_assignment() {
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
                                let (declarators, inline_declarations): (
                                    DeclaratorsWithAssignment,
                                    Vec<InlineDeclaration>,
                                ) = self.parse(context)?;
                                let declarator_output =
                                    declarators.apply_to_type(target_type, context)?;
                                if declarator_output.len() != 1 {
                                    return Err("Sizeof must not be used with a comma-separated declarator list".into());
                                }
                                if declarator_output[0].name.is_some() {
                                    return Err("Sizeof must be used with an abstract declarator, but a name was found".into());
                                }
                                self.expect(Token::CloseParen)?;
                                Some(ExpressionWithoutType::SizeOfType(
                                    declarator_output[0].out_type.clone(),
                                    inline_declarations,
                                ))
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
        while !self.is_empty() && self.peek()?.is_suffix_operator() {
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
                Token::OpenParen => {
                    if let Some(ExpressionWithoutType::Var(name)) = left {
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

                        left = Some(ExpressionWithoutType::FunctionCall(
                            new_name,
                            arguments.into_iter().map(|a| a.into()).collect(),
                        ))
                    } else {
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

                        left = Some(ExpressionWithoutType::IndirectFunctionCall(
                            Box::new(left.unwrap().into()),
                            arguments.into_iter().map(|a| a.into()).collect(),
                        ))
                    }
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
                if self.is_empty() {
                    let (new_name, _external_link) =
                        ExpressionWithoutType::resolve_identifier(&name, context)?;

                    Some(ExpressionWithoutType::Var(new_name))
                } else {
                    match self.peek()? {
                        // this is a function call !!
                        Token::OpenParen => {
                            self.expect(Token::OpenParen)?;
                            match name.as_str() {
                                "__builtin_va_start" => {
                                    let va_list: ExpressionNode = self.parse(context)?;
                                    self.expect(Token::Comma)?;
                                    let last_arg: ExpressionNode = self.parse(context)?;
                                    self.expect(Token::CloseParen)?;

                                    Some(ExpressionWithoutType::BuiltinFunctionCall(
                                        BuiltinVa::Start(Box::new(va_list), Box::new(last_arg)),
                                    ))
                                }
                                "__builtin_va_arg" => {
                                    let va_list: ExpressionNode = self.parse(context)?;
                                    self.expect(Token::Comma)?;

                                    let (t, mut inline_declarations): (
                                        Type,
                                        Vec<InlineDeclaration>,
                                    ) = self.parse(context)?;
                                    let (d, mut declarations_from_declarator): (
                                        DeclaratorsWithAssignment,
                                        Vec<InlineDeclaration>,
                                    ) = self.parse(context)?;
                                    let declarator_output = d.apply_to_type(t, context)?;
                                    inline_declarations.append(&mut declarations_from_declarator);
                                    self.expect(Token::CloseParen)?;

                                    if declarator_output.len() != 1 {
                                        return Err("va_arg must not be used with a comma-separated declarator list".into());
                                    }

                                    if declarator_output[0].name.is_some() {
                                        return Err(
                                            "Non-abstract declarator used in __builtin_va_arg"
                                                .into(),
                                        );
                                    }

                                    Some(ExpressionWithoutType::BuiltinFunctionCall(
                                        BuiltinVa::Arg(
                                            Box::new(va_list),
                                            declarator_output[0].out_type.clone(),
                                            inline_declarations,
                                        ),
                                    ))
                                }
                                "__builtin_va_end" => {
                                    let va_list: ExpressionNode = self.parse(context)?;
                                    self.expect(Token::CloseParen)?;

                                    Some(ExpressionWithoutType::BuiltinFunctionCall(
                                        BuiltinVa::End(Box::new(va_list)),
                                    ))
                                }
                                "__builtin_va_copy" => {
                                    let va_list: ExpressionNode = self.parse(context)?;
                                    self.expect(Token::Comma)?;
                                    let va_list_2: ExpressionNode = self.parse(context)?;
                                    self.expect(Token::CloseParen)?;

                                    Some(ExpressionWithoutType::BuiltinFunctionCall(
                                        BuiltinVa::Copy(Box::new(va_list), Box::new(va_list_2)),
                                    ))
                                }
                                _ => {
                                    let (new_name, _external_link) =
                                        ExpressionWithoutType::resolve_identifier(&name, context)?;

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
                            }
                        }
                        _ => {
                            // VALIDATION STEP: Check the variable has been declared
                            let (new_name, _external_link) =
                                ExpressionWithoutType::resolve_identifier(&name, context)?;

                            Some(ExpressionWithoutType::Var(new_name))
                        }
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
                while !self.is_empty() && matches!(self.peek()?, Token::StringLiteral(_)) {
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
        //     "resolve {} {:?} {:?}",
        //     name, context.outer_scope_identifiers, context.current_scope_identifiers
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
            // Err(format!("Identifier used before declaration: {}", name).into())

            // new requirement: this can only be checked during the type-checking stage, because
            // assignment to enums can bring new names into scope. That being said, that only
            // applies to variable names which are not already found in the current scope, so the
            // majority of (valid) variables won't be affected by this change.
            Ok((format!("{}.expecting.enum.member", name), false))
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
        expecting_kind: &StructKind,
        context: &mut ParseContext,
    ) -> Result<String, Box<dyn Error>> {
        // println!(
        //     "resolve {} {:?} {:?}",
        //     name, context.outer_struct_names, context.current_struct_names
        // );
        if let Some(info) = context.current_struct_names.get(name) {
            if info.1 != *expecting_kind {
                return Err(format!(
                    "Conflicting definitions of struct and union with tag {}",
                    name
                )
                .into());
            }
            Ok(info.0.clone())
        } else if let Some(info) = context.outer_struct_names.get(name) {
            if info.1 != *expecting_kind {
                return Err(format!(
                    "Conflicting definitions of struct and union with tag {}",
                    name
                )
                .into());
            }
            Ok(info.0.clone())
        } else {
            context.num_structs += 1;
            let new_name = format!("{}.{}", name, context.num_structs);

            // anonymous structs do not get saved to the scope symbols list
            if name != "anonymous.struct" {
                context
                    .current_struct_names
                    .insert(name.to_string(), (new_name.clone(), *expecting_kind));
            }

            Ok(new_name)
        }
    }

    pub fn new_struct_name(
        name: &str,
        expecting_kind: &StructKind,
        context: &mut ParseContext,
    ) -> Result<String, Box<dyn Error>> {
        // println!(
        //     "resolve for new {:?} {:?}",
        //     context.outer_struct_names, context.current_struct_names
        // );
        if let Some(info) = context.current_struct_names.get(name) {
            if info.1 != *expecting_kind {
                return Err(format!(
                    "Conflicting definitions of struct and union with tag {}",
                    name
                )
                .into());
            }
            Ok(info.0.clone())
        } else {
            // ignore the outer scope, this is an EXPLICIT declaration of a new struct type
            context.num_structs += 1;
            let new_name = format!("{}.{}", name, context.num_structs);

            // never store anonymous structs in the context
            if name != "anonymous.struct" {
                context
                    .current_struct_names
                    .insert(name.to_string(), (new_name.clone(), *expecting_kind));
            }

            Ok(new_name)
        }
    }
}

pub trait PopForExpression {
    fn pop_tokens_for_expression(
        &mut self,
        is_param: bool,
        context: &mut ParseContext,
        // type tokens and storage class tokens
    ) -> Result<VecDeque<Token>, Box<dyn Error>>;
}
impl PopForExpression for VecDeque<Token> {
    fn pop_tokens_for_expression(
        &mut self,
        is_param: bool,
        _context: &mut ParseContext,
        // type tokens and storage class tokens
    ) -> Result<VecDeque<Token>, Box<dyn Error>> {
        let mut out = VecDeque::new();
        let mut paren_nesting = 0;
        let mut brace_nesting = 0;
        let mut square_nesting = 0;
        // expressions can't end with =
        // expression can't be completely empty
        while !(self.is_empty()
            || (paren_nesting == 0
                && square_nesting == 0
                && brace_nesting == 0
                && out.back() != Some(&Token::Assignment)
                && (is_param || !out.is_empty())
                && (matches!(
                    self.peek()?,
                    Token::CloseParen
                        | Token::CloseSquareBracket
                        | Token::OpenBrace
                        | Token::SemiColon
                ) || (is_param && matches!(self.peek()?, Token::Comma)))))
        {
            match self.peek()? {
                Token::OpenParen => paren_nesting += 1,
                Token::CloseParen => paren_nesting -= 1,
                Token::OpenSquareBracket => square_nesting += 1,
                Token::CloseSquareBracket => square_nesting -= 1,
                Token::OpenBrace => brace_nesting += 1,
                Token::CloseBrace => brace_nesting -= 1,
                _ => {}
            }
            out.push_back(self.read()?);
        }
        Ok(out)
    }
}
