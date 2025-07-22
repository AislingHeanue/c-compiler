use std::{collections::VecDeque, error::Error};

use super::birds::BirdsProgramNode;

mod convert;
mod display;

pub struct Program {
    function: Function,
    footer: ExtraStrings,
    has_comments: bool,
}

struct ExtraStrings(Vec<String>);

struct Function {
    header: ExtraStrings,
    name: String,
    instructions: Instructions,
    is_mac: bool,
}

struct Instructions(VecDeque<Instruction>);

#[derive(Clone)]
enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand), // Operand here is both the src and dst.
    // op src, dst. dst is the *first* number in the operation
    Binary(BinaryOperator, Operand, Operand),
    // dividend comes from EDX+EAX. quotient -> EDX, remainder -> EAX.
    Idiv(Operand),
    // expand a 32 bit number to 64 bits. EAX -> EDX+EAX.
    Cdq,
    AllocateStack(i32), // number of bytes to allocate
    Custom(String), // custom assembly strings, so I can avoid templating them in at a later stage
    Ret,
}

#[derive(Clone)]
enum Operand {
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
enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Clone)]
enum BinaryOperator {
    Add,
    Sub,
    Mult,
}

trait Convert
where
    Self: Sized,
{
    type Input;
    type Output;
    fn convert(
        parsed: Self::Input,
        config: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>>;
}

pub struct ConvertContext {
    comments: bool,
    is_mac: bool,
    is_linux: bool,
}

pub fn codegen(
    parsed: BirdsProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
) -> Result<Program, Box<dyn Error>> {
    Program::convert(
        parsed,
        &mut ConvertContext {
            comments,
            is_linux: linux,
            is_mac: mac,
        },
    )
}
