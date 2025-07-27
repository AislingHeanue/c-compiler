use super::parser::ProgramNode;
use std::error::Error;

mod convert;
mod display;

// BIRDS: Bodacious Intermediate Representation Design Spec
#[derive(Debug)]
pub struct BirdsProgramNode {
    pub body: Vec<BirdsFunctionNode>,
}

pub struct BirdsFunctionNode {
    pub name: String,
    pub params: Vec<String>,
    pub instructions: Vec<BirdsInstructionNode>,
}

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
    Copy(BirdsValueNode, BirdsValueNode),
    Jump(String),
    JumpZero(BirdsValueNode, String),
    JumpNotZero(BirdsValueNode, String),
    Label(String),
    // name, args, dst
    FunctionCall(String, Vec<BirdsValueNode>, BirdsValueNode),
}

#[derive(Clone, Debug)]
pub enum BirdsValueNode {
    IntegerConstant(usize),
    Var(String),
}

#[derive(Debug)]
pub enum BirdsUnaryOperatorNode {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BirdsBinaryOperatorNode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
}

trait Convert
where
    Self: Sized,
{
    type Output;
    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>>;
}

pub struct ConvertContext {
    last_end_label_number: i32,
    last_else_label_number: i32,
    last_false_label_number: i32,
    last_stack_number: i32,
    last_true_label_number: i32,
}

pub fn birds(parsed: ProgramNode) -> Result<BirdsProgramNode, Box<dyn Error>> {
    parsed.convert(&mut ConvertContext {
        last_end_label_number: 0,
        last_else_label_number: 0,
        last_false_label_number: 0,
        last_stack_number: 0,
        last_true_label_number: 0,
    })
}
