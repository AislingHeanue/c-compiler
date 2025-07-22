use std::{collections::VecDeque, error::Error};

use super::birds::BirdsProgramNode;

mod convert;
mod display;

pub struct Program {
    function: Function,
    footer: ExtraStrings,
    displaying_context: DisplayContext,
}

struct ExtraStrings(Vec<String>);

struct Function {
    header: ExtraStrings,
    name: String,
    instructions: Instructions,
}

struct Instructions(VecDeque<Instruction>);

enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand), // Operand here is both the src and dst.
    // op src, dst. dst is the *first* number in the operation
    Binary(BinaryOperator, Operand, Operand),
    // compare left and right and set "ZF" (zero), "SF" (sign) and "OF" (signed overflow)
    // based on the result, so that they can be read by JmpCondition and SetCondition
    Cmp(Operand, Operand),
    // dividend comes from EDX+EAX. quotient -> EDX, remainder -> EAX.
    Idiv(Operand),
    // expand a 32 bit number to 64 bits. EAX -> EDX+EAX.
    Cdq,
    Jmp(String),
    JmpCondition(ConditionCode, String),
    // write 0 or 1 to the first byte of dst based on Cmp output.
    SetCondition(ConditionCode, Operand),
    // named label to jump to. Follows different indentation rules to others.
    Label(String),
    AllocateStack(i32), // number of bytes to allocate
    Ret,
    Custom(String), // custom assembly strings, so I can avoid templating them in at a later stage
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

enum ConditionCode {
    E,
    Ne,
    G,
    Ge,
    L,
    Le,
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

trait CodeDisplay {
    fn show(&self, context: &mut DisplayContext) -> String;
}

#[derive(Clone)]
pub struct DisplayContext {
    comments: bool,
    indent: usize,
    is_mac: bool,
    word_length_bytes: usize,
    // is_linux: bool,
}

impl DisplayContext {
    fn indent(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.indent += 4;
        s
    }
    fn unindent(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.indent -= 4;
        s
    }
    fn short(&mut self) -> DisplayContext {
        let mut s = self.clone();
        s.word_length_bytes = 1;
        s
    }
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
