use itertools::Itertools;

use super::{Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{
        BlockItemNode, DeclarationNode, Declarator, FunctionDeclaration, VariableDeclaration,
    },
    types::StorageClass,
};
use std::{collections::VecDeque, error::Error};

impl Parse<DeclarationNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<DeclarationNode, Box<dyn Error>> {
        let (base_type, storage_class) = self.parse(context)?;

        let declarator: Declarator = self.parse(context)?;
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
                    }))
                } else {
                    self.expect(Token::SemiColon)?;
                    Ok(DeclarationNode::Function(FunctionDeclaration {
                        function_type: out_type,
                        name,
                        params: param_names,
                        body: None,
                        storage_class,
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
                        }))
                    }
                    Token::SemiColon => Ok(DeclarationNode::Variable(VariableDeclaration {
                        variable_type: out_type,
                        name,
                        init: None,
                        storage_class,
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
            if let Some((_new_name, other_has_linkage)) =
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
            // not entirely sure how this is compatible with the future type-checking step, but okay
            context
                .current_scope_identifiers
                .insert(name.clone(), (new_name.clone(), is_linked));
        }

        Ok(new_name)
    }
}

impl Parse<(Type, Option<StorageClass>)> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Option<StorageClass>), Box<dyn Error>> {
        // Specifiers may be one of "static", "int" or "extern"
        // ... and there may be any non-zero amount of them
        let mut storage_classes = Vec::new();
        let mut types = Vec::new();
        while self.peek()?.is_specifier() {
            if self.peek()?.is_type() {
                types.push(self.read()?);
            } else {
                storage_classes.push(self.read()?);
            }
        }

        if storage_classes.len() > 1 {
            return Err(format!(
                "Got multiple storage classes in declaration: {:?}",
                storage_classes
            )
            .into());
        }

        let storage = storage_classes.first().map(|t| match t {
            Token::KeywordStatic => StorageClass::Static,
            Token::KeywordExtern => StorageClass::Extern,
            _ => unreachable!(),
        });

        let types_deque: &mut VecDeque<Token> = &mut types.into();
        let this_type = types_deque.parse(context)?;
        Ok((this_type, storage))
    }
}
