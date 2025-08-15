use itertools::Itertools;

use super::{
    declarator::OutputWithStruct,
    parsed_types::{ParseStructDeclaration, PopForType},
    Identity, Parse, ParseContext, Type,
};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{
        BlockItemNode, DeclarationNode, Declarator, FunctionDeclaration, StructDeclaration,
        TypeDeclaration, VariableDeclaration,
    },
    types::StorageClass,
};
use std::{collections::VecDeque, error::Error};

impl Parse<DeclarationNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<DeclarationNode, Box<dyn Error>> {
        // take the storage class out of the type definition (assuming there is only one, since
        // otherwise that's an error)
        let specifier_loc = self.clone().pop_tokens_for_type(context)?.1;

        let storage_class = match specifier_loc {
            None => None,
            Some(i) => {
                let removed = self.remove(i);
                match removed.unwrap() {
                    Token::KeywordStatic => Some(StorageClass::Static),
                    Token::KeywordExtern => Some(StorageClass::Extern),
                    _ => unreachable!(),
                }
            }
        };

        let (base_type, declarator, struct_declaration): (
            Type,
            Declarator,
            Option<StructDeclaration>,
        ) = match self.peek()? {
            Token::KeywordTypedef => {
                if storage_class.is_some() {
                    return Err("Cannot use static or extern in a typedef alias".into());
                }
                self.expect(Token::KeywordTypedef)?;
                let (base_type, declarator, struct_declaration): OutputWithStruct =
                    self.parse(context)?;
                self.expect(Token::SemiColon)?;
                let declarator_output = declarator.apply_to_type(base_type)?;

                let out_type = declarator_output.out_type;
                let name = declarator_output.name;
                context
                    .current_scope_identifiers
                    .insert(name.clone(), Identity::TypeAlias(out_type.clone()));

                return Ok(DeclarationNode::Type(TypeDeclaration {
                    target_type: out_type,
                    name,
                    struct_declaration,
                }));
            }
            Token::KeywordStruct | Token::KeywordUnion => {
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
                        return Ok(DeclarationNode::Struct(struct_declaration));
                    }
                    _ => {
                        // this is a variable declaration, so continue to parse the declarator and
                        // the init

                        let struct_declaration =
                            struct_declaration_tokens.parse_struct(true, context)?;
                        if !struct_declaration_tokens.is_empty() {
                            return Err("Leftover tokens parsing a struct declaration".into());
                        }

                        let declarator: Declarator = self.parse(context)?;
                        (
                            Type::Struct(
                                struct_declaration.name.clone(),
                                struct_declaration.is_union,
                            ),
                            declarator,
                            Some(struct_declaration),
                        )
                    }
                }
            }
            _ => {
                let (out, declarator, struct_declaration): OutputWithStruct =
                    self.parse(context)?;
                (out, declarator, struct_declaration)
            }
        };

        let declarator_output = declarator.apply_to_type(base_type)?;

        let out_type = declarator_output.out_type;
        let param_names = declarator_output.param_names.unwrap_or(Vec::new());

        match out_type {
            Type::Function(_, ref param_types) => {
                let name = DeclarationNode::new_identifier(
                    declarator_output.name,
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
                        struct_declarations: declarator_output.struct_declarations,
                    }))
                } else {
                    self.expect(Token::SemiColon)?;
                    Ok(DeclarationNode::Function(FunctionDeclaration {
                        function_type: out_type,
                        name,
                        params: param_names,
                        body: None,
                        storage_class,
                        struct_declarations: declarator_output.struct_declarations,
                    }))
                }
            }
            _ => {
                let name = DeclarationNode::new_identifier(
                    declarator_output.name,
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
                            struct_declaration,
                        }))
                    }
                    Token::SemiColon => Ok(DeclarationNode::Variable(VariableDeclaration {
                        variable_type: out_type,
                        name,
                        init: None,
                        storage_class,
                        struct_declaration,
                    })),
                    _ => Err("Invalid token in variable declaration".into()),
                }
            }
        }
    }
}

impl DeclarationNode {
    fn new_identifier(
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
