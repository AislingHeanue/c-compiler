use super::{lexer::Type, parser::ProgramNode};
use std::error::Error;

mod convert;

// BIRDS: Bodacious Intermediate Representation Design Spec
#[derive(Debug)]
pub struct BirdsProgramNode {
    pub function: BirdsFunctionNode,
}

#[derive(Debug)]
pub struct BirdsFunctionNode {
    pub name: String,
    pub instructions: BirdsInstructions,
}

#[derive(Debug)]
pub struct BirdsInstructions(pub Vec<BirdsInstructionNode>);

#[derive(Debug)]
pub enum BirdsInstructionNode {
    Return(BirdsValueNode),
    // op, src, dst
    Unary(BirdsUnaryOperatorNode, BirdsValueNode, BirdsValueNode),
    // op, left, right, dst
    Binary(
        BirdsBinaryOperatorNode,
        BirdsValueNode,
        BirdsValueNode,
        BirdsValueNode,
    ),
}

#[derive(Clone, Debug)]
pub enum BirdsValueNode {
    Constant(Type),
    Var(String),
}

#[derive(Debug)]
pub enum BirdsUnaryOperatorNode {
    Complement,
    Negate,
}

#[derive(Debug)]
pub enum BirdsBinaryOperatorNode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
}

trait Convert
where
    Self: Sized,
{
    type Input;
    type Output;
    fn convert(parsed: Self::Input) -> Result<Self::Output, Box<dyn Error>>;
}

pub fn birds(parsed: ProgramNode) -> Result<BirdsProgramNode, Box<dyn Error>> {
    BirdsProgramNode::convert(parsed)
}
