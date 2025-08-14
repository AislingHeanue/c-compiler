use std::collections::HashMap;
use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    parser::{BinaryOperatorNode, BlockItemNode, ProgramNode, StructInfo},
    types::{StorageInfo, SymbolInfo, Type},
};

use super::{
    BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsResult, BirdsValueNode, Destination,
};

mod declaration;
mod expression_node;
mod program_node;
mod statement_node;

pub fn do_birds(
    parsed: ProgramNode,
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
) -> Result<BirdsResult, Box<dyn Error>> {
    let mut context = ConvertContext {
        last_end_label_number: 0,
        last_else_label_number: 0,
        last_false_label_number: 0,
        last_stack_number: 0,
        last_true_label_number: 0,
        current_initialiser_offset: 0,
        num_block_strings: 0,
        symbols,
        structs,
    };

    let result = parsed.convert(&mut context)?;
    Ok((result, context.symbols, context.structs))
}

trait Convert<T>
where
    Self: Sized,
{
    fn convert(self, context: &mut ConvertContext) -> Result<T, Box<dyn Error>>;
}

pub struct ConvertContext {
    last_end_label_number: i32,
    last_else_label_number: i32,
    last_false_label_number: i32,
    last_stack_number: i32,
    last_true_label_number: i32,
    current_initialiser_offset: i32,
    num_block_strings: i32,
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
}

impl<U, V> Convert<Option<U>> for Option<V>
where
    V: Convert<U>,
{
    fn convert(self, context: &mut ConvertContext) -> Result<Option<U>, Box<dyn Error>> {
        match self {
            Some(e) => Ok(Some(e.convert(context)?)),
            None => Ok(None),
        }
    }
}

fn new_temp_variable(type_to_store: &Type, context: &mut ConvertContext) -> BirdsValueNode {
    context.last_stack_number += 1;
    let new_name = format!("stack.{}", context.last_stack_number);
    context.symbols.insert(
        new_name.clone(),
        SymbolInfo {
            symbol_type: type_to_store.clone(),
            storage: StorageInfo::Automatic,
        },
    );

    BirdsValueNode::Var(new_name)
}

impl Convert<Vec<BirdsInstructionNode>> for Vec<BlockItemNode> {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|node| match node {
                BlockItemNode::Statement(statement) => statement.convert(context),
                BlockItemNode::Declaration(declaration) => declaration.convert(context),
            }),
            |iter| iter.flatten().collect(),
        )
    }
}

impl Destination {
    fn evaluate(
        self,
        target_type: &Type,
        context: &mut ConvertContext,
    ) -> (Vec<BirdsInstructionNode>, BirdsValueNode) {
        match self {
            Destination::Direct(val) => (Vec::new(), val),
            Destination::Dereference(val) => {
                let new_dst = new_temp_variable(target_type, context);
                (
                    vec![BirdsInstructionNode::LoadFromPointer(val, new_dst.clone())],
                    new_dst,
                )
            }
            Destination::StructEntry(base, offset) => {
                let new_dst = new_temp_variable(target_type, context);
                (
                    vec![BirdsInstructionNode::CopyFromOffset(
                        base,
                        offset,
                        new_dst.clone(),
                    )],
                    new_dst,
                )
            }
        }
    }
}

impl From<BirdsValueNode> for Destination {
    fn from(value: BirdsValueNode) -> Self {
        Destination::Direct(value)
    }
}

impl BirdsBinaryOperatorNode {
    pub fn is_relational(&self) -> bool {
        matches!(
            self,
            BirdsBinaryOperatorNode::Equal
                | BirdsBinaryOperatorNode::NotEqual
                | BirdsBinaryOperatorNode::Greater
                | BirdsBinaryOperatorNode::GreaterEqual
                | BirdsBinaryOperatorNode::Less
                | BirdsBinaryOperatorNode::LessEqual
        )
    }
}

impl Convert<BirdsBinaryOperatorNode> for BinaryOperatorNode {
    fn convert(
        self,
        _context: &mut ConvertContext,
    ) -> Result<BirdsBinaryOperatorNode, Box<dyn Error>> {
        let bird_op = match self {
            BinaryOperatorNode::Add => BirdsBinaryOperatorNode::Add,
            BinaryOperatorNode::Subtract => BirdsBinaryOperatorNode::Subtract,
            BinaryOperatorNode::Multiply => BirdsBinaryOperatorNode::Multiply,
            BinaryOperatorNode::Divide => BirdsBinaryOperatorNode::Divide,
            BinaryOperatorNode::Mod => BirdsBinaryOperatorNode::Mod,
            BinaryOperatorNode::BitwiseAnd => BirdsBinaryOperatorNode::BitwiseAnd,
            BinaryOperatorNode::BitwiseXor => BirdsBinaryOperatorNode::BitwiseXor,
            BinaryOperatorNode::BitwiseOr => BirdsBinaryOperatorNode::BitwiseOr,
            BinaryOperatorNode::ShiftLeft => BirdsBinaryOperatorNode::ShiftLeft,
            BinaryOperatorNode::ShiftRight => BirdsBinaryOperatorNode::ShiftRight,
            BinaryOperatorNode::Equal => BirdsBinaryOperatorNode::Equal,
            BinaryOperatorNode::NotEqual => BirdsBinaryOperatorNode::NotEqual,
            BinaryOperatorNode::Less => BirdsBinaryOperatorNode::Less,
            BinaryOperatorNode::Greater => BirdsBinaryOperatorNode::Greater,
            BinaryOperatorNode::LessEqual => BirdsBinaryOperatorNode::LessEqual,
            BinaryOperatorNode::GreaterEqual => BirdsBinaryOperatorNode::GreaterEqual,
            // process these in the 'else' block below instead
            BinaryOperatorNode::And | BinaryOperatorNode::Or => unreachable!(),
        };
        Ok(bird_op)
    }
}
