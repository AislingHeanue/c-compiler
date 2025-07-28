use std::{collections::HashMap, error::Error};

use super::{birds::BirdsProgramNode, parser::SymbolInfo};

mod convert;
mod display;

pub struct Program {
    body: Vec<TopLevel>,
    displaying_context: DisplayContext,
}

enum TopLevel {
    // header instructions, name, body, global
    Function(String, Vec<Instruction>, bool),
    // name init global
    StaticVariable(String, usize, bool),
}

enum Instruction {
    // Mov src, dst
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
    FreeStack(i32),
    Push(Operand),
    Call(String),
    Ret,
    Custom(String), // custom assembly strings, so I can avoid templating them in at a later stage
}

#[derive(Clone)]
enum Operand {
    Imm(usize),      //constant numeric value
    Reg(Register),   // register in assembly
    MockReg(String), // mocked register for temporary use.
    Stack(i32),      // Stack entry whose value is the offset from RSP.
    Data(String),    // used for static and global variables
}

#[derive(Clone)]
enum Register {
    AX, // eax or rax
    CX,
    DX, // edx or rdx
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

static FUNCTION_PARAM_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

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
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
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
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>>;
}

pub struct ConvertContext {
    comments: bool,
    is_mac: bool,
    is_linux: bool,
    symbols: HashMap<String, SymbolInfo>,
}

trait CodeDisplay {
    fn show(&self, context: &mut DisplayContext) -> String;
}

#[derive(Clone)]
pub struct DisplayContext {
    comments: bool,
    indent: usize,
    is_linux: bool,
    is_mac: bool,
    word_length_bytes: usize,
    symbols: HashMap<String, SymbolInfo>,
}

impl DisplayContext {
    fn indent(&mut self) -> &mut DisplayContext {
        self.indent += 4;
        self
    }
    fn unindent(&mut self) -> &mut DisplayContext {
        self.indent -= 4;
        self
    }
    fn short(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 1;
        self
    }
    fn regular(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 4;
        self
    }
    fn long(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 8;
        self
    }
}

pub fn codegen(
    parsed: BirdsProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
    types: HashMap<String, SymbolInfo>,
) -> Result<Program, Box<dyn Error>> {
    Program::convert(
        parsed,
        &mut ConvertContext {
            comments,
            is_linux: linux,
            is_mac: mac,
            symbols: types,
        },
    )
}
