use std::collections::HashMap;
use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    parser::{BinaryOperatorNode, Block, BlockItemNode, ForInitialiserNode, ProgramNode},
    types::{Constant, StorageInfo, SymbolInfo, Type},
};

use super::{
    BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsValueNode, Destination,
};

mod declaration;
mod expression_node;
mod initialiser_node;
mod program_node;
mod statement_node;

trait Convert
where
    Self: Sized,
{
    type Output;
    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>>;
}

trait ConvertEvaluate
where
    Self: Sized,
{
    type Output;
    fn convert_and_evaluate(
        self,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>>;
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
}

impl<T, U, V> Convert for Option<T>
where
    T: Convert<Output = (U, V)>,
{
    type Output = Option<(U, V)>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            Some(e) => {
                let (instructions, value) = e.convert(context)?;
                Ok(Some((instructions, value)))
            }
            None => Ok(None),
        }
    }
}

impl<T, U, V> ConvertEvaluate for Option<T>
where
    T: ConvertEvaluate<Output = (U, V)>,
{
    type Output = Option<(U, V)>;

    fn convert_and_evaluate(
        self,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            Some(e) => {
                let (instructions, value) = e.convert_and_evaluate(context)?;
                Ok(Some((instructions, value)))
            }
            None => Ok(None),
        }
    }
}

pub fn do_birds(
    parsed: ProgramNode,
    symbols: HashMap<String, SymbolInfo>,
) -> Result<(BirdsProgramNode, HashMap<String, SymbolInfo>), Box<dyn Error>> {
    let mut context = ConvertContext {
        last_end_label_number: 0,
        last_else_label_number: 0,
        last_false_label_number: 0,
        last_stack_number: 0,
        last_true_label_number: 0,
        current_initialiser_offset: 0,
        num_block_strings: 0,
        symbols,
    };

    let result = parsed.convert(&mut context)?;
    Ok((result, context.symbols))
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

// purely-for-utility function for getting constants (almost always 0 or 1) in the appropriate type
fn get_typed_constant(value: i64, target: &Type) -> BirdsValueNode {
    match target {
        Type::Integer => BirdsValueNode::Constant(Constant::Integer(value.try_into().unwrap())),
        Type::Long => BirdsValueNode::Constant(Constant::Long(value)),
        Type::UnsignedInteger => {
            BirdsValueNode::Constant(Constant::UnsignedInteger(value.try_into().unwrap()))
        }
        Type::UnsignedLong => {
            BirdsValueNode::Constant(Constant::UnsignedLong(value.try_into().unwrap()))
        }
        // adding a constant to a pointer is only reasonable if the second operand is Long
        Type::Pointer(_) => BirdsValueNode::Constant(Constant::Long(value)),
        Type::Array(..) => unreachable!(),
        Type::Function(_, _) => unreachable!(),
        Type::Char => BirdsValueNode::Constant(Constant::Char(value.try_into().unwrap())),
        Type::SignedChar => BirdsValueNode::Constant(Constant::Char(value.try_into().unwrap())),
        Type::UnsignedChar => {
            BirdsValueNode::Constant(Constant::UnsignedChar(value.try_into().unwrap()))
        }
        Type::Double => panic!("Can't use get_typed_constant to generate a double"),
    }
}

fn get_double(value: f64) -> BirdsValueNode {
    BirdsValueNode::Constant(Constant::Double(value))
}

impl Convert for Block {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|node| match node {
                BlockItemNode::Statement(statement) => statement.convert(context),
                BlockItemNode::Declaration(declaration) => declaration.convert(context),
            }),
            |iter| iter.flatten().collect(),
        )
    }
}

impl Convert for ForInitialiserNode {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(d) => Ok(d.convert(context)?),
            ForInitialiserNode::Expression(Some(expression)) => Ok(expression.convert(context)?.0),
            ForInitialiserNode::Expression(None) => Ok(Vec::new()),
        }
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

impl Convert for BinaryOperatorNode {
    type Output = BirdsBinaryOperatorNode;

    fn convert(self, _context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
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
