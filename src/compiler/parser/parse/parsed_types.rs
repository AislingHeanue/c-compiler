use super::{declarator::OutputWithStruct, Parse, ParseContext, StructMember, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{Declarator, ExpressionWithoutType, StructDeclaration, StructKind},
};
use std::{collections::VecDeque, error::Error};

impl Parse<Type> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<Type, Box<dyn Error>> {
        if matches!(self.peek()?, Token::KeywordStruct | Token::KeywordUnion) {
            let declaration: StructDeclaration = self.parse_struct(true, context)?;
            let is_union = match declaration.kind {
                StructKind::Struct => false,
                StructKind::Union => true,
            };
            return Ok(Type::Struct(declaration.name, is_union));
        }
        let mut out = Vec::new();
        while !self.is_empty() && self.peek()?.is_type(context) {
            out.push(self.read()?)
        }
        if out.is_empty() {
            return Err("No type tokens found".into());
        }
        let mut seen = Vec::new();
        for i in out.iter() {
            if seen.contains(i) {
                return Err("Repeated token in type definition".into());
            }
            seen.push(i.clone());
        }
        if let Some(Token::Identifier(s)) = out
            .iter()
            .find(|token| matches!(token, Token::Identifier(_)))
        {
            if out.len() == 1 {
                return ExpressionWithoutType::resolve_type_alias(s, context);
            } else {
                return Err("Aliased type cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordDouble) {
            if out.len() == 1 {
                return Ok(Type::Double);
            } else {
                return Err("Double cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordVoid) {
            if out.len() == 1 {
                return Ok(Type::Void);
            } else {
                return Err("Void cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordSigned) && out.contains(&Token::KeywordUnsigned) {
            return Err("Type specified as both signed and unsigned".into());
        }
        if out.contains(&Token::KeywordUnsigned) && out.contains(&Token::KeywordLong) {
            return Ok(Type::UnsignedLong);
        }
        if out.contains(&Token::KeywordChar) {
            if out.iter().any(|t| {
                !matches!(
                    t,
                    Token::KeywordChar | Token::KeywordSigned | Token::KeywordUnsigned
                )
            }) {
                return Err("Char type definition contained another type".into());
            }
            if out.contains(&Token::KeywordUnsigned) {
                return Ok(Type::UnsignedChar);
            }
            if out.contains(&Token::KeywordSigned) {
                return Ok(Type::SignedChar);
            }
            return Ok(Type::Char);
        }
        if out.contains(&Token::KeywordUnsigned) {
            return Ok(Type::UnsignedInteger);
        }
        if out.contains(&Token::KeywordLong) {
            return Ok(Type::Long);
        }
        Ok(Type::Integer)
    }
}

impl Parse<(Type, Option<StructDeclaration>)> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Option<StructDeclaration>), Box<dyn Error>> {
        if matches!(self.peek()?, Token::KeywordStruct | Token::KeywordUnion) {
            let declaration: StructDeclaration = self.parse_struct(true, context)?;
            let is_union = match declaration.kind {
                StructKind::Struct => false,
                StructKind::Union => true,
            };
            Ok((
                Type::Struct(declaration.name.clone(), is_union),
                Some(declaration),
            ))
        } else {
            let t: Type = self.parse(context)?;
            Ok((t, None))
        }
    }
}

pub trait ParseStructDeclaration {
    fn parse_struct(
        &mut self,
        implicit: bool,
        context: &mut ParseContext,
    ) -> Result<StructDeclaration, Box<dyn Error>>;
}

impl ParseStructDeclaration for VecDeque<Token> {
    fn parse_struct(
        &mut self,
        implicit: bool,
        context: &mut ParseContext,
    ) -> Result<StructDeclaration, Box<dyn Error>> {
        let kind = match self.read()? {
            Token::KeywordStruct => StructKind::Struct,
            Token::KeywordUnion => StructKind::Union,
            _ => return Err("Invalid token, expecting 'struct' or 'union'".into()),
        };

        let name = match self.peek()? {
            Token::Identifier(name) => {
                self.read()?;
                name
            }
            Token::OpenBrace => "anonymous.struct".to_string(),
            _ => return Err("Struct type is missing name identifier".into()),
        };

        // NOTE: DURING PARSING this step here, we resolve the struct name from the current scope,
        // or declare that this is a new struct. Then, we evaluate its members with the context
        // that this struct now exists.
        let new_name = if implicit {
            ExpressionWithoutType::resolve_struct_name(&name, context)?
        } else {
            ExpressionWithoutType::new_struct_name(&name, context)?
        };
        let members = if !self.is_empty() && self.peek()? == Token::OpenBrace {
            self.expect(Token::OpenBrace)?;
            let mut members: Vec<StructMember> = Vec::new();
            while self.peek()? != Token::CloseBrace {
                members.push(self.parse(context)?)
            }
            self.expect(Token::CloseBrace)?;

            if members.is_empty() {
                return Err(
                    "Struct definition (eg. with '{}') must have at least one member".into(),
                );
            } else {
                Some(members)
            }
        } else {
            None
        };

        Ok(StructDeclaration {
            name: new_name,
            members,
            kind,
            // nameless,
        })
    }
}

impl Parse<StructMember> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<StructMember, Box<dyn Error>> {
        let mut type_tokens = self.pop_tokens_for_type(context)?.0;
        match self.peek()? {
            Token::SemiColon => {
                let embedded_struct_declaration: StructDeclaration =
                    type_tokens.parse_struct(true, context)?;
                self.expect(Token::SemiColon)?;
                let is_union = match embedded_struct_declaration.kind {
                    StructKind::Struct => false,
                    StructKind::Union => true,
                };
                Ok(StructMember {
                    member_type: Type::Struct(embedded_struct_declaration.name.clone(), is_union),
                    name: None,
                    struct_declaration: Some(embedded_struct_declaration),
                })
            }
            _ => {
                // attempt to parse a declarator with the remaining tokens
                type_tokens.append(self);
                // put the tokens back the way they were
                *self = type_tokens;
                let (base_type, declarator, struct_declaration): (
                    Type,
                    Declarator,
                    Option<StructDeclaration>,
                ) = self.parse(context)?;
                self.expect(Token::SemiColon)?;
                let declarator_output = declarator.apply_to_type(base_type)?;

                if let Type::Function(_, _) = declarator_output.out_type {
                    Err("A struct member may not have a function type".into())
                } else {
                    Ok(StructMember {
                        member_type: declarator_output.out_type,
                        name: Some(declarator_output.name),
                        struct_declaration,
                    })
                }
            }
        }
    }
}

impl Parse<OutputWithStruct> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<OutputWithStruct, Box<dyn Error>> {
        let (mut types_deque, storage) = self.pop_tokens_for_type(context)?;

        // DeclarationNode should already filter out any instances of static and extern from this
        // list. Otherwise thrown an error here since that means there are either too many
        // specifiers, or a specifier is used in a bad place.
        if storage.is_some() {
            return Err("Illegal use of storage specifiers in this type".into());
        }

        let mut declarator_deque = VecDeque::new();
        let mut paren_nesting = 0;
        while !(self.is_empty()
            || matches!(
                self.peek()?,
                Token::OpenBrace | Token::Assignment | Token::SemiColon
            )
            || (paren_nesting == 0 && matches!(self.peek()?, Token::CloseParen | Token::Comma)))
        {
            match self.peek()? {
                Token::OpenParen => paren_nesting += 1,
                Token::CloseParen => paren_nesting -= 1,
                _ => {}
            }
            declarator_deque.push_back(self.read()?);
        }

        while !types_deque.is_empty() {
            // try and get a valid type and declarator for the given expression
            let mut new_types_deque = types_deque.clone();
            let mut new_declarator_deque = declarator_deque.clone();
            let type_result: Result<(Type, Option<StructDeclaration>), Box<dyn Error>> =
                new_types_deque.parse(context);
            let declarator = new_declarator_deque.parse(context);
            if let Ok((ref t, ref struct_declaration)) = type_result {
                if new_types_deque.is_empty()
                    && declarator.is_ok()
                    && new_declarator_deque.is_empty()
                {
                    return Ok((t.clone(), declarator?, struct_declaration.clone()));
                }
            }
            // Entering this section means that we failed to properly split the type and the
            // declarator, so try again with a different split

            // println!(
            //     "Parsing failure, trying another combination, type: {:?} => {:?} declarator: {:?} => {:?}",
            //     types_deque,type_result, declarator_deque, declarator
            // );
            let move_this_to_the_declarator = types_deque.pop_back().unwrap();
            declarator_deque.push_front(move_this_to_the_declarator);
        }

        Err("Declaration could not be reconciled to match both a type and a declarator".into())
        // Ok((this_type, storage, declarator))
    }
}

pub trait PopForType {
    fn pop_tokens_for_type(
        &mut self,
        context: &mut ParseContext,
        // type tokens and storage class tokens
    ) -> Result<(VecDeque<Token>, Option<usize>), Box<dyn Error>>;
}

impl PopForType for VecDeque<Token> {
    fn pop_tokens_for_type(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(VecDeque<Token>, Option<usize>), Box<dyn Error>> {
        let mut types_deque = VecDeque::new();
        let mut storage_loc = None;
        while self.peek()?.is_start_of_declaration(context) {
            match self.peek()? {
                Token::KeywordStruct | Token::KeywordUnion => {
                    // READING A STRUCT TYPE //
                    // red the keyword
                    types_deque.push_back(self.read()?);
                    // read the struct name if it exists. Anonymous structs omit this name
                    if !matches!(self.peek()?, Token::OpenBrace | Token::Identifier(_)) {
                        return Err("Malformed struct type".into());
                    }
                    if matches!(self.peek()?, Token::Identifier(_)) {
                        types_deque.push_back(self.read()?); // read the name
                    }
                    if self.peek()? == Token::OpenBrace {
                        types_deque.push_back(self.read()?);
                        let mut nesting_count = 1;
                        while nesting_count != 0 {
                            match self.peek()? {
                                Token::OpenBrace => nesting_count += 1,
                                Token::CloseBrace => nesting_count -= 1,
                                _ => {}
                            }
                            // read everything contained in the curly braces
                            types_deque.push_back(self.read()?);
                        }
                    }
                    // DONE READING A STRUCT TYPE //
                }
                Token::KeywordStatic | Token::KeywordExtern => {
                    if storage_loc.is_some() {
                        return Err("Encountered more than one storage specifier in a type".into());
                    } else {
                        storage_loc = Some(types_deque.len());
                    }
                    types_deque.push_back(self.read()?);
                }
                _ => {
                    types_deque.push_back(self.read()?);
                }
            }
        }
        Ok((types_deque, storage_loc))
    }
}

impl Token {
    pub fn is_type(&self, context: &mut ParseContext) -> bool {
        if let Token::Identifier(s) = self {
            ExpressionWithoutType::resolve_type_alias(s, context).is_ok()
        } else {
            matches!(
                self,
                Token::KeywordLong
                    | Token::KeywordInt
                    | Token::KeywordUnsigned
                    | Token::KeywordSigned
                    | Token::KeywordDouble
                    | Token::KeywordChar
                    | Token::KeywordVoid
            )
        }
    }

    pub fn is_specifier(&self, context: &mut ParseContext) -> bool {
        self.is_type(context) || matches!(self, Token::KeywordStatic | Token::KeywordExtern)
    }

    pub fn is_start_of_declaration(&self, context: &mut ParseContext) -> bool {
        self.is_specifier(context)
            || matches!(
                self,
                Token::KeywordTypedef | Token::KeywordStruct | Token::KeywordUnion
            )
    }
}
