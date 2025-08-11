use itertools::Itertools;

use super::{Identity, Parse, ParseContext, Type};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{
        BlockItemNode, DeclarationNode, Declarator, FunctionDeclaration, TypeDeclaration,
        VariableDeclaration,
    },
    types::StorageClass,
};
use std::{collections::VecDeque, error::Error};

impl Parse<DeclarationNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<DeclarationNode, Box<dyn Error>> {
        if matches!(self.peek()?, Token::KeywordTypedef) {
            self.expect(Token::KeywordTypedef)?;
            let (base_type, storage_class, declarator) = self.parse(context)?;
            self.expect(Token::SemiColon)?;
            if storage_class.is_some() {
                return Err("Cannot use static or extern in a typedef alias".into());
            }
            let declarator_output = declarator.apply_to_type(base_type)?;

            let out_type = declarator_output.out_type;
            let name = declarator_output.name;
            // let mut statement_tokens = VecDeque::new();
            // // read until semicolon
            // while !matches!(self.peek()?, Token::SemiColon) {
            //     statement_tokens.push_back(self.read()?);
            // }
            // self.expect(Token::SemiColon)?;
            //
            // let name_token = statement_tokens
            //     .pop_back()
            //     .ok_or::<Box<dyn Error>>("Typedef instruction has too few variables".into())?;
            // let name = if let Token::Identifier(s) = name_token {
            //     s
            // } else {
            //     return Err("Type alias name is not an identifier".into());
            // };
            // let t: Type = statement_tokens.parse(context)?;
            context
                .current_scope_identifiers
                .insert(name.clone(), Identity::TypeAlias(out_type.clone()));

            return Ok(DeclarationNode::Type(TypeDeclaration {
                target_type: out_type,
                name,
            }));
        }

        let (base_type, storage_class, declarator) = self.parse(context)?;

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

impl Parse<(Type, Option<StorageClass>, Declarator)> for VecDeque<Token> {
    fn parse(
        &mut self,
        context: &mut ParseContext,
    ) -> Result<(Type, Option<StorageClass>, Declarator), Box<dyn Error>> {
        // Specifiers may be one of "static", "int" or "extern"
        // ... and there may be any non-zero amount of them
        let mut storage_classes = Vec::new();
        let mut types_deque = VecDeque::new();
        while self.peek()?.is_specifier(context) {
            if self.peek()?.is_type(context) {
                types_deque.push_back(self.read()?);
            } else {
                storage_classes.push(self.read()?);
            }
        }
        let mut declarator_deque = VecDeque::new();
        while !self.is_empty()
            && !matches!(
                self.peek()?,
                Token::OpenBrace | Token::Assignment | Token::SemiColon
            )
        {
            declarator_deque.push_back(self.read()?);
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

        while !types_deque.is_empty() {
            // try and get a valid type and declarator for the given expression
            let mut new_types_deque = types_deque.clone();
            let mut new_declarator_deque = declarator_deque.clone();
            let this_type = new_types_deque.parse(context);
            let declarator = new_declarator_deque.parse(context);
            if this_type.is_ok()
                && new_types_deque.is_empty()
                && declarator.is_ok()
                && new_declarator_deque.is_empty()
            {
                return Ok((this_type?, storage, declarator?));
            }
            // Entering this section means that we failed to properly split the type and the
            // declarator, so try again with a different split

            println!(
                "Parsing failure, trying another combination, type: {:?} => {:?} declarator: {:?} => {:?}",
                types_deque,this_type,  declarator_deque, declarator
            );
            let move_this_to_the_declarator = types_deque.pop_back().unwrap();
            declarator_deque.push_front(move_this_to_the_declarator);
        }

        Err("Declaration could not be reconciled to match both a type and a declarator".into())
        // Ok((this_type, storage, declarator))
    }
}
