use super::{Parse, ParseContext, StructKind, StructMember, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{
        BlockItemNode, DeclarationNode, Declarator, EnumDeclaration, ExpressionWithoutType,
        InlineDeclaration, StructDeclaration,
    },
    types::EnumMember,
};
use std::{
    collections::{hash_map::Entry, HashMap, VecDeque},
    error::Error,
};

impl Parse<Type> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<Type, Box<dyn Error>> {
        if matches!(
            self.peek()?,
            Token::KeywordStruct | Token::KeywordUnion | Token::KeywordEnum
        ) {
            let declaration: InlineDeclaration = self.parse_struct(true, context)?;
            match declaration {
                InlineDeclaration::Struct(s) => {
                    return Ok(Type::Struct(s.name, s.is_union));
                }
                InlineDeclaration::Enum(_e) => {
                    return Ok(Type::Integer);
                }
            }
        }
        let mut out = Vec::new();
        while !self.is_empty() && self.peek()?.is_type(context) {
            out.push(self.read()?)
        }
        if out.is_empty() {
            return Err("No type tokens found".into());
        }
        let mut seen = Vec::new();
        let mut is_long_long = false;
        for i in out.iter() {
            if seen.contains(i) {
                if *i == Token::KeywordLong && !is_long_long {
                    is_long_long = true;
                } else {
                    return Err("Repeated token in type definition".into());
                }
            } else {
                seen.push(i.clone());
            }
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
            } else if out.len() == 2 && out.contains(&Token::KeywordLong) {
                return Ok(Type::LongDouble);
            } else {
                return Err("Double cannot be used with other type specifiers".into());
            }
        }
        if out.contains(&Token::KeywordFloat) {
            if out.len() == 1 {
                return Ok(Type::Float);
            } else {
                return Err("Float cannot be used with other type specifiers".into());
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
        if out.contains(&Token::KeywordShort) && out.contains(&Token::KeywordLong) {
            return Err("Type specified as both long and short".into());
        }
        if out.contains(&Token::KeywordUnsigned) && out.contains(&Token::KeywordShort) {
            return Ok(Type::UnsignedShort);
        }
        if out.contains(&Token::KeywordUnsigned) && out.contains(&Token::KeywordLong) {
            if is_long_long {
                return Ok(Type::UnsignedLongLong);
            } else {
                return Ok(Type::UnsignedLong);
            }
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
            if is_long_long {
                return Ok(Type::LongLong);
            } else {
                return Ok(Type::Long);
            }
        }
        if out.contains(&Token::KeywordShort) {
            return Ok(Type::Short);
        }
        Ok(Type::Integer)
    }
}

impl Parse<(Type, Vec<InlineDeclaration>)> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Vec<InlineDeclaration>), Box<dyn Error>> {
        if matches!(
            self.peek()?,
            Token::KeywordStruct | Token::KeywordUnion | Token::KeywordEnum
        ) {
            let declaration: InlineDeclaration = self.parse_struct(true, context)?;
            match declaration {
                InlineDeclaration::Struct(ref s) => {
                    Ok((Type::Struct(s.name.clone(), s.is_union), vec![declaration]))
                }
                InlineDeclaration::Enum(ref m) => {
                    Ok((Type::Enum(m.members.clone()), vec![declaration]))
                }
            }
        } else {
            let t: Type = self.parse(context)?;
            Ok((t, Vec::new()))
        }
    }
}

pub trait ParseStructDeclaration {
    fn parse_struct(
        &mut self,
        implicit: bool,
        context: &mut ParseContext,
    ) -> Result<InlineDeclaration, Box<dyn Error>>;
}

impl ParseStructDeclaration for VecDeque<Token> {
    fn parse_struct(
        &mut self,
        implicit: bool,
        context: &mut ParseContext,
    ) -> Result<InlineDeclaration, Box<dyn Error>> {
        let kind = match self.read()? {
            Token::KeywordStruct => StructKind::Struct,
            Token::KeywordUnion => StructKind::Union,
            Token::KeywordEnum => StructKind::Enum,
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
        let new_struct_name = if implicit {
            ExpressionWithoutType::resolve_struct_name(&name, &kind, context)?
        } else {
            ExpressionWithoutType::new_struct_name(&name, &kind, context)?
        };

        match kind {
            StructKind::Enum => {
                let members = if !self.is_empty() && self.peek()? == Token::OpenBrace {
                    self.expect(Token::OpenBrace)?;
                    context.last_enum_number = -1;
                    let mut members: Vec<EnumMember> = Vec::new();
                    while self.peek()? != Token::CloseBrace {
                        let mut member: EnumMember = self.parse(context)?;
                        // NOTE: enum options are brought into scope here!
                        member.internal_name = Some(DeclarationNode::new_identifier(
                            member.name.clone(),
                            false,
                            context,
                            None,
                        )?);
                        members.push(member);
                    }
                    self.expect(Token::CloseBrace)?;

                    context
                        .enums
                        .insert(new_struct_name.clone(), members.clone());
                    if members.is_empty() {
                        return Err(
                            "Enum definition (eg. with '{}') must have at least one member".into(),
                        );
                    } else {
                        members
                    }
                } else {
                    // NOTE: this gets the enum from the enums map and sandwiches its members into
                    // this declaration. This is the only type that does this in the parser step,
                    // and it needs to do this at the moment because the type-checker lacks the
                    // required scope information to do it there.
                    let e = context.enums.get(&new_struct_name);
                    if let Some(found_members) = e {
                        found_members.to_vec()
                    } else {
                        return Err("Could not find a definition for enum options".into());
                    }
                };

                Ok(InlineDeclaration::Enum(EnumDeclaration {
                    name: new_struct_name,
                    members,
                }))
            }
            _ => {
                let members = if !self.is_empty() && self.peek()? == Token::OpenBrace {
                    // enter a new scope for struct body
                    let scopes = BlockItemNode::enter_scope(context);

                    self.expect(Token::OpenBrace)?;
                    let mut members: Vec<StructMember> = Vec::new();
                    while self.peek()? != Token::CloseBrace {
                        let member: StructMember = self.parse(context)?;
                        members.push(member);
                    }
                    self.expect(Token::CloseBrace)?;

                    BlockItemNode::leave_scope(scopes, context);

                    if members.is_empty() {
                        return Err(
                            "Struct definition (eg. with '{}') must have at least one member"
                                .into(),
                        );
                    } else {
                        Some(members)
                    }
                } else {
                    None
                };
                Ok(InlineDeclaration::Struct(StructDeclaration {
                    name: new_struct_name,
                    members,
                    is_union: kind == StructKind::Union,
                }))
            }
        }
    }
}

impl Parse<StructMember> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<StructMember, Box<dyn Error>> {
        let mut type_tokens = self.pop_tokens_for_type(context)?.0;
        match (type_tokens.front(), self.peek()?) {
            // this deals with anonymous/inline structs which don't have a field name
            (
                Some(Token::KeywordStruct | Token::KeywordUnion | Token::KeywordEnum),
                Token::SemiColon,
            ) => {
                let embedded_struct_declaration: InlineDeclaration =
                    type_tokens.parse_struct(true, context)?;
                self.expect(Token::SemiColon)?;
                match embedded_struct_declaration {
                    InlineDeclaration::Struct(ref s) => Ok(StructMember {
                        member_type: Type::Struct(s.name.clone(), s.is_union),
                        name: None,
                        inline_declarations: vec![embedded_struct_declaration],
                    }),
                    InlineDeclaration::Enum(ref m) => Ok(StructMember {
                        member_type: Type::Enum(m.members.clone()),
                        name: None,
                        inline_declarations: vec![embedded_struct_declaration],
                    }),
                }
            }
            // choosing to ignore this case for now because it's complicated and confusing
            // (Some(Token::KeywordEnum), Token::SemiColon) => {
            //     let embedded_enum_declaration: EnumDeclaration = self.parse_enum(context)?;
            //     Ok(StructMember {
            //         // special case, this declaration adds no fields to the struct
            //         member_type: Type::Void,
            //         name: None,
            //         inline_declarations: vec![InlineDeclaration::Enum(embedded_enum_declaration)],
            //     })
            // }
            _ => {
                // attempt to parse a declarator with the remaining tokens
                type_tokens.append(self);
                // put the tokens back the way they were
                *self = type_tokens;
                let (base_type, declarator, inline_declarations): (
                    Type,
                    Declarator,
                    Vec<InlineDeclaration>,
                ) = self.parse(context)?;
                self.expect(Token::SemiColon)?;
                let declarator_output = declarator.apply_to_type(base_type, context)?;

                if let Type::Function(_, _, _) = declarator_output.out_type {
                    Err("A struct member may not have a function type".into())
                } else {
                    Ok(StructMember {
                        member_type: declarator_output.out_type,
                        name: declarator_output.name,
                        inline_declarations,
                    })
                }
            }
        }
    }
}

impl Parse<EnumMember> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<EnumMember, Box<dyn Error>> {
        let name = if let Token::Identifier(name) = self.read()? {
            name
        } else {
            return Err("Expected identifier while reading an enum declaration".into());
        };
        let value = if let Token::Assignment = self.peek()? {
            self.expect(Token::Assignment)?;
            let c: ExpressionWithoutType = self.parse(context)?;
            c.fold_to_constant(context)?.value_int()
        } else {
            context.last_enum_number + 1
        };
        context.last_enum_number = value;

        if let Token::Comma = self.peek()? {
            self.expect(Token::Comma)?;
        }

        Ok(EnumMember {
            name,
            internal_name: None,
            init: value,
        })
    }
}

pub type OutputWithInline = (Type, Declarator, Vec<InlineDeclaration>);
impl Parse<OutputWithInline> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<OutputWithInline, Box<dyn Error>> {
        let (mut types_deque, specifier_locations) = self.pop_tokens_for_type(context)?;

        // DeclarationNode should already filter out any instances of static and extern from this
        // list. Otherwise thrown an error here since that means there are either too many
        // specifiers, or a specifier is used in a bad place.
        if specifier_locations.contains_key(&SpecifierKind::Storage)
            || specifier_locations.contains_key(&SpecifierKind::Inline)
        // || specifier_locations.contains_key(&SpecifierKind::Const)
        {
            return Err("Illegal use of storage specifiers in this type".into());
        }
        let _is_constant = if let Some(loc) = specifier_locations.get(&SpecifierKind::Const) {
            types_deque.remove(*loc);
            true
        } else {
            false
        };

        let mut declarator_deque = VecDeque::new();
        let mut paren_nesting = 0;
        let mut brace_nesting = 0;
        let mut square_nesting = 0;
        while !(self.is_empty()
            || (paren_nesting == 0
                && square_nesting == 0
                && brace_nesting == 0
                && matches!(
                    self.peek()?,
                    Token::CloseParen
                        | Token::CloseSquareBracket
                        | Token::Comma
                        | Token::SemiColon
                        | Token::Assignment
                        | Token::OpenBrace
                )))
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
            declarator_deque.push_back(self.read()?);
        }

        while !types_deque.is_empty() {
            // try and get a valid type and declarator for the given expression
            let mut new_types_deque = types_deque.clone();
            let mut new_declarator_deque = declarator_deque.clone();
            let mut type_result: Result<(Type, Vec<InlineDeclaration>), Box<dyn Error>> =
                new_types_deque.parse(context);
            let mut declarator_result: Result<
                (Declarator, Vec<InlineDeclaration>),
                Box<dyn Error>,
            > = new_declarator_deque.parse(context);
            if let (
                Ok((ref t, ref mut struct_declarations)),
                Ok((ref declarator, ref mut structs_from_declarator)),
            ) = (&mut type_result, &mut declarator_result)
            {
                if new_types_deque.is_empty() && new_declarator_deque.is_empty() {
                    struct_declarations.append(structs_from_declarator);
                    return Ok((t.clone(), declarator.clone(), struct_declarations.clone()));
                }
            }
            // Entering this section means that we failed to properly split the type and the
            // declarator, so try again with a different split

            println!(
                "Parsing failure, trying another combination, type: {:?} => {:?} declarator: {:?} => {:?} and {:?}",
                types_deque,type_result, declarator_deque, declarator_result, new_declarator_deque
            );
            let move_this_to_the_declarator = types_deque.pop_back().unwrap();
            declarator_deque.push_front(move_this_to_the_declarator);
        }

        Err(format!("Declaration could not be reconciled to match both a type and a declarator, types: {:?}", declarator_deque).into())
        // Ok((this_type, storage, declarator))
    }
}

pub type TypeResults = (VecDeque<Token>, HashMap<SpecifierKind, usize>);
#[derive(PartialEq, Eq, Hash)]
pub enum SpecifierKind {
    Const,
    Storage,
    Inline,
    Volatile,
}

pub trait PopForType {
    fn pop_tokens_for_type(
        &mut self,
        context: &mut ParseContext,
        // type tokens and storage class tokens
    ) -> Result<TypeResults, Box<dyn Error>>;
}

impl PopForType for VecDeque<Token> {
    fn pop_tokens_for_type(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<TypeResults, Box<dyn Error>> {
        let mut types_deque = VecDeque::new();
        let mut specifier_locations = HashMap::new();
        while self.peek()?.is_start_of_declaration(context) {
            match self.peek()? {
                Token::KeywordStruct | Token::KeywordUnion | Token::KeywordEnum => {
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
                    if let Entry::Vacant(e) = specifier_locations.entry(SpecifierKind::Storage) {
                        e.insert(types_deque.len());
                    } else {
                        return Err("Encountered more than one storage specifier in a type".into());
                    }
                    types_deque.push_back(self.read()?);
                }
                Token::KeywordConst => {
                    if let Entry::Vacant(e) = specifier_locations.entry(SpecifierKind::Const) {
                        e.insert(types_deque.len());
                    } else {
                        return Err("Encountered more than one const specifier in a type".into());
                    }
                    types_deque.push_back(self.read()?);
                }
                Token::KeywordInline => {
                    if let Entry::Vacant(e) = specifier_locations.entry(SpecifierKind::Inline) {
                        e.insert(types_deque.len());
                    } else {
                        return Err("Encountered more than one inline specifier in a type".into());
                    }
                    types_deque.push_back(self.read()?);
                }
                Token::KeywordVolatile => {
                    if let Entry::Vacant(e) = specifier_locations.entry(SpecifierKind::Volatile) {
                        e.insert(types_deque.len());
                    } else {
                        return Err("Encountered more than one inline specifier in a type".into());
                    }
                    types_deque.push_back(self.read()?);
                }
                _ => {
                    types_deque.push_back(self.read()?);
                }
            }
        }
        Ok((types_deque, specifier_locations))
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
                    | Token::KeywordShort
                    | Token::KeywordFloat
            )
        }
    }

    pub fn is_specifier(&self, context: &mut ParseContext) -> bool {
        self.is_type(context)
            || matches!(
                self,
                Token::KeywordStatic
                    | Token::KeywordExtern
                    | Token::KeywordConst
                    | Token::KeywordInline
                    | Token::KeywordVolatile
            )
    }

    pub fn is_start_of_declaration(&self, context: &mut ParseContext) -> bool {
        self.is_specifier(context)
            || matches!(
                self,
                Token::KeywordTypedef
                    | Token::KeywordStruct
                    | Token::KeywordUnion
                    | Token::KeywordEnum
            )
    }
}
