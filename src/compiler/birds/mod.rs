use std::{collections::HashMap, error::Error};

use convert::do_birds;
use optimize::do_optimize;

use crate::OptimizeConfig;

use super::{
    parser::{ProgramNode, StructInfo},
    types::{Constant, StaticInitialiser, SymbolInfo, Type},
};

mod convert;
mod display;
mod optimize;

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

#[derive(Debug, Clone, PartialEq)]
pub enum BirdsInstructionNode {
    Return(Option<BirdsValueNode>),
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
    JumpCondition(
        BirdsBinaryOperatorNode,
        BirdsValueNode,
        BirdsValueNode,
        String,
    ),
    Label(String),
    // name, args, dst, is_variadic
    FunctionCall(String, Vec<BirdsValueNode>, Option<BirdsValueNode>, bool),
    IndirectFunctionCall(
        BirdsValueNode,
        Vec<BirdsValueNode>,
        Option<BirdsValueNode>,
        bool,
    ),
    // src (32 bits) to dst (64 bits)
    SignedExtend(BirdsValueNode, BirdsValueNode),
    // src (64 bits) to dst (32 bits)
    Truncate(BirdsValueNode, BirdsValueNode),
    // src (32 bits) to dst (64 bits) for unsigned values
    ZeroExtend(BirdsValueNode, BirdsValueNode),
    FloatToInt(BirdsValueNode, BirdsValueNode),
    FloatToUint(BirdsValueNode, BirdsValueNode),
    IntToFloat(BirdsValueNode, BirdsValueNode),
    UintToFloat(BirdsValueNode, BirdsValueNode),
    FloatToDouble(BirdsValueNode, BirdsValueNode),
    DoubleToFloat(BirdsValueNode, BirdsValueNode),
    // IntToFloat(BirdsValueNode, BirdsValueNode),
    // UintToFloat(BirdsValueNode, BirdsValueNode),
    // FloatToInt(BirdsValueNode, BirdsValueNode),
    // FloatToUint(BirdsValueNode, BirdsValueNode),
    GetAddress(BirdsValueNode, BirdsValueNode),
    LoadFromPointer(BirdsValueNode, BirdsValueNode),
    StoreInPointer(BirdsValueNode, BirdsValueNode),
    // src, index, scale (sizeof type in bytes), dst
    AddPointer(BirdsValueNode, BirdsValueNode, u64, BirdsValueNode),
    // src, name of some aggregate type variable, offset (bytes)
    CopyToOffset(BirdsValueNode, String, i32),
    // src name, offset, dst
    CopyFromOffset(String, i32, BirdsValueNode),
    VaStart(BirdsValueNode),
    // VaArg(BirdsValueNode, BirdsValueNode),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum BirdsValueNode {
    Constant(Constant),
    Var(String),
}

impl BirdsValueNode {
    pub fn get_constant_value(&self) -> Option<i32> {
        match self {
            BirdsValueNode::Constant(constant) => match constant {
                Constant::Integer(c) => Some(*c),
                Constant::Long(c) => Some((*c).try_into().unwrap()),
                Constant::UnsignedInteger(c) => Some((*c).try_into().unwrap()),
                Constant::UnsignedLong(c) => Some((*c).try_into().unwrap()),
                Constant::Float(_) => None,
                Constant::Double(_) => None,
                Constant::LongDouble(_) => None,
                Constant::Char(c) => Some((*c).into()),
                Constant::UnsignedChar(c) => Some((*c).into()),
                Constant::Short(c) => Some((*c).into()),
                Constant::UnsignedShort(c) => Some((*c).into()),
                Constant::LongLong(c) => Some((*c).try_into().unwrap()),
                Constant::UnsignedLongLong(c) => Some((*c).try_into().unwrap()),
                Constant::AddressOf(_) => unreachable!(),
            },
            BirdsValueNode::Var(_) => None,
        }
    }

    pub fn get_type(&self, symbols: &HashMap<String, SymbolInfo>) -> Type {
        match self {
            BirdsValueNode::Constant(constant) => constant.get_type(),
            BirdsValueNode::Var(v) => symbols.get(v).unwrap().symbol_type.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BirdsUnaryOperatorNode {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
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
    // base name, offset
    StructEntry(String, i32),
}

type BirdsResult = (
    BirdsProgramNode,
    HashMap<String, SymbolInfo>,
    HashMap<String, StructInfo>,
);

pub fn birds(
    parsed: ProgramNode,
    ignore_stack_gaps: bool,
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
) -> Result<BirdsResult, Box<dyn Error>> {
    do_birds(parsed, ignore_stack_gaps, symbols, structs)
}

type OptimizeResult = (
    BirdsProgramNode,
    HashMap<String, SymbolInfo>,
    HashMap<String, StructInfo>,
);

pub fn optimize(
    birds: BirdsProgramNode,
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
    optimize_config: OptimizeConfig,
) -> Result<OptimizeResult, Box<dyn Error>> {
    do_optimize(birds, symbols, structs, optimize_config)
}
