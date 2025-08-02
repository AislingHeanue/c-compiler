use super::parser::{Constant, ProgramNode, StaticInitial, SymbolInfo, Type};
use std::{collections::HashMap, error::Error};

mod convert;
mod display;

// BIRDS: Bodacious Intermediate Representation Design Spec
#[derive(Debug)]
pub struct BirdsProgramNode {
    pub body: Vec<BirdsTopLevel>,
}

pub enum BirdsTopLevel {
    // name params instructions global
    Function(String, Vec<String>, Vec<BirdsInstructionNode>, bool),
    // name init global
    StaticVariable(Type, String, StaticInitial, bool),
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
    // src (32 bits) to dst (64 bits)
    SignedExtend(BirdsValueNode, BirdsValueNode),
    // src (64 bits) to dst (32 bits)
    Truncate(BirdsValueNode, BirdsValueNode),
    // src (32 bits) to dst (64 bits) for unsigned values
    ZeroExtend(BirdsValueNode, BirdsValueNode),
    DoubleToInt(BirdsValueNode, BirdsValueNode),
    DoubleToUint(BirdsValueNode, BirdsValueNode),
    IntToDouble(BirdsValueNode, BirdsValueNode),
    UintToDouble(BirdsValueNode, BirdsValueNode),
}

#[derive(Clone, Debug)]
pub enum BirdsValueNode {
    Constant(Constant),
    Var(String),
}

#[derive(Debug, PartialEq)]
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
    symbols: HashMap<String, SymbolInfo>,
}

pub fn birds(
    parsed: ProgramNode,
    symbols: HashMap<String, SymbolInfo>,
) -> Result<(BirdsProgramNode, HashMap<String, SymbolInfo>), Box<dyn Error>> {
    let mut context = ConvertContext {
        last_end_label_number: 0,
        last_else_label_number: 0,
        last_false_label_number: 0,
        last_stack_number: 0,
        last_true_label_number: 0,
        symbols,
    };

    let result = parsed.convert(&mut context)?;
    Ok((result, context.symbols))
}
