use itertools::Itertools;

use super::{
    parsed_types::{OutputWithInline, ParseStructDeclaration, PopForType},
    Identity, Parse, ParseContext, Type,
};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{
        BlockItemNode, DeclarationNode, Declarator, DeclaratorWithInline, FunctionDeclaration,
        InlineDeclaration, TypeDeclaration, VariableDeclaration,
    },
    types::StorageClass,
};
use std::{collections::VecDeque, error::Error};

impl Parse<DeclarationNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<DeclarationNode, Box<dyn Error>> {
        // take the storage class out of the type definition (assuming there is only one, since
        // otherwise that's an error)
        let (_, specifier_locations) = self.clone().pop_tokens_for_type(context)?;

        let mut is_inline = false;
        let mut is_constant = false;
        let mut is_volatile = false;
        let mut storage_class = None;
        specifier_locations
            .values()
            .sorted_by(|v1, v2| v1.cmp(v2))
            .rev()
            .for_each(|v| {
                let removed = self.remove(*v);
                match removed.unwrap() {
                    Token::KeywordVolatile => is_volatile = true,
                    Token::KeywordInline => is_inline = true,
                    Token::KeywordConst => is_constant = true,
                    Token::KeywordStatic => storage_class = Some(StorageClass::Static),
                    Token::KeywordExtern => storage_class = Some(StorageClass::Extern),
                    _ => unreachable!(),
                }
            });

        let must_have_body = is_constant && storage_class != Some(StorageClass::Extern);

        let (base_type, declarator, inline_declarations): (
            Type,
            Declarator,
            Vec<InlineDeclaration>,
        ) = match self.peek()? {
            Token::KeywordTypedef => {
                if storage_class.is_some() {
                    return Err("Cannot use static or extern in a typedef alias".into());
                }
                self.expect(Token::KeywordTypedef)?;
                let (base_type, declarator, struct_declarations): OutputWithInline =
                    self.parse(context)?;
                self.expect(Token::SemiColon)?;
                let declarator_output = declarator.apply_to_type(base_type, context)?;

                let out_type = declarator_output.out_type;
                let name = declarator_output.name;
                let name = if let Some(name) = name {
                    name
                } else {
                    return Err("typedef specified without a target name".into());
                };
                context
                    .current_scope_identifiers
                    .insert(name.clone(), Identity::TypeAlias(out_type.clone()));

                return Ok(DeclarationNode::Type(TypeDeclaration {
                    target_type: out_type,
                    name,
                    inline_declarations: struct_declarations,
                }));
            }
            Token::KeywordStruct | Token::KeywordUnion | Token::KeywordEnum => {
                // parsing the StructDeclaration here does the following
                // 1. Consumes the entire struct type
                // 2. If the struct type had a definition, adds it to the scope's struct map
                // 3. Return the uniquely-generated name corresponding to this name in this scope
                // 4. Return the list of struct members tied to THIS SPECIFIC instance of the
                //    declaration.

                // let struct_declaration = self.parse(context)?;
                let mut struct_declaration_tokens = self.pop_tokens_for_type(context)?.0;

                match self.peek()? {
                    Token::SemiColon => {
                        let struct_declaration =
                            struct_declaration_tokens.parse_struct(false, context)?;
                        if !struct_declaration_tokens.is_empty() {
                            return Err(
                                "Leftover tokens parsing an explicit struct declaration".into()
                            );
                        }

                        self.expect(Token::SemiColon)?;
                        return Ok(struct_declaration.into_declaration());
                    }
                    _ => {
                        // this is a variable declaration, so continue to parse the declarator and
                        // the init

                        let struct_declaration =
                            struct_declaration_tokens.parse_struct(true, context)?;
                        if !struct_declaration_tokens.is_empty() {
                            return Err("Leftover tokens parsing a struct declaration".into());
                        }
                        let mut all_declarations = vec![struct_declaration.clone()];

                        let (declarator, mut variable_structs): DeclaratorWithInline =
                            self.parse(context)?;

                        all_declarations.append(&mut variable_structs);

                        match struct_declaration {
                            InlineDeclaration::Struct(s) => (
                                Type::Struct(s.name.clone(), s.is_union),
                                declarator,
                                all_declarations,
                            ),
                            InlineDeclaration::Enum(m) => {
                                (Type::Enum(m.members.clone()), declarator, all_declarations)
                            }
                        }
                    }
                }
            }
            _ => self.parse(context)?,
        };

        let declarator_output = declarator.apply_to_type(base_type, context)?;

        let out_type = declarator_output.out_type;
        let param_names = declarator_output.param_names.unwrap_or(Vec::new());

        let out = match out_type {
            Type::Function(_, ref param_types) => {
                let declaration_name = if let Some(name) = declarator_output.name {
                    name
                } else {
                    return Err("function declared without a name".into());
                };
                let name = DeclarationNode::new_identifier(
                    declaration_name,
                    true,
                    context,
                    storage_class.clone(),
                )?;
                if param_names.len() != param_types.len() {
                    return Err("Mismatched length of parameter types and parameter names".into());
                }
                if !context.do_not_validate
                    && param_names.iter().unique().collect_vec().len() != param_names.len()
                {
                    return Err("Duplicate param name in function declaration".into());
                }

                if self.peek()? == Token::OpenBrace {
                    let original_outer_scope_variables = BlockItemNode::enter_scope(context);

                    // resolve param names into new identifiers tied to the current scope
                    let new_params_names = if !param_names.is_empty() {
                        let mut new_param_names = Vec::new();
                        for param_name in param_names {
                            new_param_names.push(DeclarationNode::new_identifier(
                                param_name, false, context, None,
                            )?);
                        }
                        new_param_names
                    } else {
                        Vec::new()
                    };

                    // parse block in *not* a new scope
                    context.current_block_is_function_body = true;
                    let body = self.parse(context)?;
                    BlockItemNode::leave_scope(original_outer_scope_variables, context);

                    Ok(DeclarationNode::Function(FunctionDeclaration {
                        function_type: out_type,
                        name,
                        params: new_params_names,
                        body: Some(body),
                        storage_class,
                        inline_declarations,
                        _inline: is_inline,
                        output_const: is_constant,
                        output_volatile: is_volatile,
                    }))
                } else {
                    self.expect(Token::SemiColon)?;
                    Ok(DeclarationNode::Function(FunctionDeclaration {
                        function_type: out_type,
                        name,
                        params: param_names,
                        body: None,
                        storage_class,
                        inline_declarations,
                        _inline: is_inline,
                        output_const: is_constant,
                        output_volatile: is_volatile,
                    }))
                }
            }
            _ => {
                let declaration_name = if let Some(name) = declarator_output.name {
                    name
                } else {
                    return Err("variable declared without a name".into());
                };
                let name = DeclarationNode::new_identifier(
                    declaration_name,
                    context.current_scope_is_file,
                    context,
                    storage_class.clone(),
                )?;
                // variable assignment
                match self.read()? {
                    Token::Assignment => {
                        // file scope variable = linkage
                        let initialiser = self.parse(context)?;
                        self.expect(Token::SemiColon)?;
                        Ok(DeclarationNode::Variable(VariableDeclaration {
                            variable_type: out_type,
                            name,
                            init: Some(initialiser),
                            storage_class,
                            inline_declarations,
                            constant: is_constant,
                            volatile: is_volatile,
                        }))
                    }
                    Token::SemiColon => Ok(DeclarationNode::Variable(VariableDeclaration {
                        variable_type: out_type,
                        name,
                        init: None,
                        storage_class,
                        inline_declarations,
                        constant: is_constant,
                        volatile: is_volatile,
                    })),
                    _ => Err("Invalid token in variable declaration".into()),
                }
            }
        };
        if must_have_body {
            if let Ok(ref out) = out {
                match out {
                    DeclarationNode::Variable(v) if v.init.is_none() => {
                        return Err("Const variable declaration must have a value".into())
                    }
                    DeclarationNode::Function(f) if f.body.is_none() => {
                        return Err("Const function declaration must have a value".into())
                    }
                    DeclarationNode::Type(_) | DeclarationNode::Struct(_) => {
                        return Err("Const declaration must have a value".into())
                    }
                    _ => {}
                }
            }
        }
        out
    }
}

impl DeclarationNode {
    pub fn new_identifier(
        name: String,
        is_file_scope: bool,
        context: &mut ParseContext,
        storage_class: Option<StorageClass>,
    ) -> Result<String, Box<dyn Error>> {
        let is_linked = is_file_scope || matches!(storage_class, Some(StorageClass::Extern));

        let new_name = if is_linked || context.do_not_validate {
            name.clone()
        } else {
            context.num_variables += 1;
            format!("{}.{}", name, context.num_variables)
        };

        if !context.do_not_validate {
            if let Some(Identity::Variable(_new_name, other_has_linkage)) =
                context.current_scope_identifiers.get(&name)
            {
                if !(*other_has_linkage && is_linked) {
                    return Err(format!(
                        "Identifier named {} already exists in the current scope",
                        name
                    )
                    .into());
                }
            }
            context.current_scope_identifiers.insert(
                name.clone(),
                Identity::Variable(new_name.clone(), is_linked),
            );
        }

        Ok(new_name)
    }
}

impl InlineDeclaration {
    fn into_declaration(self) -> DeclarationNode {
        match self {
            InlineDeclaration::Struct(s) => DeclarationNode::Struct(s),
            InlineDeclaration::Enum(e) => DeclarationNode::Enum(e),
        }
    }
}
