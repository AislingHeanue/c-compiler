use std::{collections::VecDeque, error::Error};

use super::birds::BirdsProgramNode;

mod convert;
mod display;

pub struct AssemblyProgramNode {
    function: AssemblyFunctionNode,
    footer: ExtraStrings,
    has_comments: bool,
}

struct ExtraStrings(Vec<String>);

struct AssemblyFunctionNode {
    header: ExtraStrings,
    name: String,
    instructions: Instructions,
    is_mac: bool,
}

struct Instructions(VecDeque<InstructionNode>);

#[derive(Clone)]
enum InstructionNode {
    Mov(OperandNode, OperandNode),
    Unary(AssemblyUnaryOperatorNode, OperandNode), // Operand here is both the src and dst.
    // op src, dst. dst is the *first* number in the operation
    Binary(AssemblyBinaryOperatorNode, OperandNode, OperandNode),
    // dividend comes from EDX+EAX. quotient -> EDX, remainder -> EAX.
    Idiv(OperandNode),
    // expand a 32 bit number to 64 bits. EAX -> EDX+EAX.
    Cdq,
    AllocateStack(i32), // number of bytes to allocate
    Custom(String),
    Ret,
}

#[derive(Clone)]
enum OperandNode {
    Imm(i32),        //constant numeric value
    Reg(Register),   // register in assembly
    MockReg(String), // mocked register for temporary use.
    Stack(i32),      // Stack entry whose value is the offset from RSP.
}

#[derive(Clone)]
enum Register {
    AX, // eax or rax
    DX, // edx or rdx
    R10,
    R11,
}

#[derive(Clone)]
enum AssemblyUnaryOperatorNode {
    Neg,
    Not,
}

#[derive(Clone)]
enum AssemblyBinaryOperatorNode {
    Add,
    Sub,
    Mult,
}

pub struct ConvertConfig {
    comments: bool,
    is_mac: bool,
    is_linux: bool,
}

trait Convert
where
    Self: Sized,
{
    type Input;
    type Output;
    fn convert(parsed: Self::Input, config: &ConvertConfig)
        -> Result<Self::Output, Box<dyn Error>>;
}

pub fn codegen(
    parsed: BirdsProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
) -> Result<AssemblyProgramNode, Box<dyn Error>> {
    AssemblyProgramNode::convert(
        parsed,
        &ConvertConfig {
            comments,
            is_linux: linux,
            is_mac: mac,
        },
    )
}
