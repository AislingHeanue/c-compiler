use std::{collections::HashMap, error::Error};

use convert::do_birds;

use super::{
    parser::ProgramNode,
    types::{Constant, StaticInitialiser, SymbolInfo, Type},
};

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
    StaticVariable(Type, String, Vec<StaticInitialiser>, bool),
    StaticConstant(Type, String, StaticInitialiser),
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
    GetAddress(BirdsValueNode, BirdsValueNode),
    LoadFromPointer(BirdsValueNode, BirdsValueNode),
    StoreInPointer(BirdsValueNode, BirdsValueNode),
    // src, index, scale (sizeof type in bytes), dst
    AddPointer(BirdsValueNode, BirdsValueNode, i32, BirdsValueNode),
    // src, name of some aggregate type variable, offset (bytes)
    CopyToOffset(BirdsValueNode, String, i32),
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Debug, Clone)]
pub enum Destination {
    Direct(BirdsValueNode),
    Dereference(BirdsValueNode),
}

pub fn birds(
    parsed: ProgramNode,
    symbols: HashMap<String, SymbolInfo>,
) -> Result<(BirdsProgramNode, HashMap<String, SymbolInfo>), Box<dyn Error>> {
    do_birds(parsed, symbols)
}
