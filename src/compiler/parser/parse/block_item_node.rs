use super::{Parse, ParseContext};
use crate::compiler::{
    lexer::{Token, TokenVector},
    parser::{BlockItemNode, DeclarationNode},
    types::StorageClass,
};
use std::{
    collections::{HashMap, VecDeque},
    error::Error,
};

type Scopes = (
    HashMap<String, (String, bool)>,
    HashMap<String, (String, bool)>,
);

impl Parse<Vec<BlockItemNode>> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<Vec<BlockItemNode>, Box<dyn Error>> {
        self.expect(Token::OpenBrace)?;

        // (and I'm feeling... good)
        let mut original_outer_scope_variables = None;
        // if this is a function body, this is not in fact a new scope, since the actual new scope
        // should also include the parameter list
        if !context.current_block_is_function_body {
            original_outer_scope_variables = Some(BlockItemNode::enter_scope(context));
        } else {
            context.current_scope_is_file = false;
        }

        context.current_block_is_function_body = false;

        let mut items: Vec<BlockItemNode> = Vec::new();
        while !matches!(self.peek()?, Token::CloseBrace) {
            items.push(self.parse(context)?)
        }

        if let Some(original) = original_outer_scope_variables {
            BlockItemNode::leave_scope(original, context);
        } else {
            context.current_scope_is_file = true;
        }

        self.expect(Token::CloseBrace)?;

        Ok(items)
    }
}

impl Parse<BlockItemNode> for VecDeque<Token> {
    fn parse(&mut self, context: &mut ParseContext) -> Result<BlockItemNode, Box<dyn Error>> {
        if self.is_empty() {
            return Err("Block item has no tokens".into());
        }

        if self.peek()?.is_specifier() {
            let declaration = self.parse(context)?;
            if let DeclarationNode::Function(ref f) = declaration {
                if f.body.is_some() && !context.do_not_validate {
                    return Err("Block-scope function declaration may not have a body".into());
                }
                if matches!(f.storage_class, Some(StorageClass::Static)) && !context.do_not_validate
                {
                    return Err("Block-scope function declaration may not be static".into());
                }
            }
            Ok(BlockItemNode::Declaration(declaration))
        } else {
            Ok(BlockItemNode::Statement(self.parse(context)?))
        }
    }
}

impl BlockItemNode {
    pub fn enter_scope(context: &mut ParseContext) -> Scopes {
        // println!(
        //     "enter {:?} {:?}",
        //     context.outer_scope_identifiers, context.current_scope_identifiers
        // );
        let original_current_scope_variables = context.current_scope_identifiers.clone();
        let original_outer_scope_variables = context.outer_scope_identifiers.clone();
        context
            .outer_scope_identifiers
            .extend(context.current_scope_identifiers.clone());
        context.current_scope_identifiers = HashMap::new();

        (
            original_current_scope_variables,
            original_outer_scope_variables,
        )
    }

    pub fn leave_scope(previous_scopes: Scopes, context: &mut ParseContext) {
        context.current_scope_identifiers = previous_scopes.0;
        context.outer_scope_identifiers = previous_scopes.1;
        // println!(
        //     "leave {:?} {:?}",
        //     context.outer_scope_identifiers, context.current_scope_identifiers
        // );
    }
}
