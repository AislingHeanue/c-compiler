use std::{cell::RefCell, collections::HashMap, error::Error, mem::swap};

use convert::{Convert, ConvertContext};
use display::DisplayContext;
use validate::{Validate, ValidateContext, VALIDATION_PASSES};

use super::{
    birds::BirdsProgramNode,
    parser::StructInfo,
    types::{StaticInitialiser, StorageInfo, SymbolInfo, Type},
};

mod convert;
mod display;
mod validate;

pub fn align_stack_size(initial: u64, alignment: u64) -> u64 {
    initial + (alignment as i64 - initial as i64).rem_euclid(alignment as i64) as u64
}

#[derive(Debug)]
pub struct Program {
    body: Vec<TopLevel>,
    displaying_context: Option<RefCell<DisplayContext>>,
}

#[derive(Debug)]
enum TopLevel {
    // header instructions, name, body, global, return_value_uses_memory
    Function(String, Vec<Instruction>, bool),
    // name global alignment init
    StaticVariable(String, bool, u32, Vec<StaticInitialiser>),
    // used for all floating point constants. Needed anytime we need eg.
    // 'float(1)' or 'float(0)' or 'float(i64::MAX+1)'
    // will be extended to other types later.
    // name alignment init
    StaticConstant(String, u32, StaticInitialiser),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum AssemblyType {
    Byte,     // byte
    Longword, // i32, int
    Quadword, // i64, long
    Double,   // oh boy
    // size, alignment
    ByteArray(u64, u32), // 'opaque chunk of memory' eg arrays
}

#[derive(Debug)]
pub enum AssemblySymbolInfo {
    // type, is_static is_a_top_level_constant
    Object(AssemblyType, bool, bool),
    // is_defined, return_value_uses_memory
    Function(bool, bool),
}

#[derive(Debug)]
enum Instruction {
    // Mov type, src, dst
    Mov(AssemblyType, Operand, Operand),
    // Mov Signed Extend src (32-bit) dst (64-bit)
    Movsx(AssemblyType, AssemblyType, Operand, Operand),
    // Unsigned counterpart to Movsx
    MovZeroExtend(AssemblyType, AssemblyType, Operand, Operand),
    // load effective address of src to dst
    // src MUST be Memory or Data. Data is a quadword
    Lea(Operand, Operand),
    // named after a good friend of mine with the same name
    // Double to Signed Int
    // dst_type, src, dst
    Cvttsd2si(AssemblyType, Operand, Operand),
    // Signed Int to Double
    // src_type, src, dst
    Cvtsi2sd(AssemblyType, Operand, Operand),
    // operand is both src and dst
    Unary(UnaryOperator, AssemblyType, Operand), // Operand here is both the src and dst.
    // op src, dst. dst is the *first* number in the operation
    Binary(BinaryOperator, AssemblyType, Operand, Operand),
    // compare left and right and set "ZF" (zero), "SF" (sign) and "OF" (signed overflow)
    // based on the result, so that they can be read by JmpCondition and SetCondition
    Cmp(AssemblyType, Operand, Operand),
    // dividend comes from EDX+EAX. quotient -> EDX, remainder -> EAX.
    Idiv(AssemblyType, Operand),
    // unsigned div
    Div(AssemblyType, Operand),
    // expand a 32 bit number to 64 bits. EAX -> EDX+EAX.
    Cdq(AssemblyType),
    Jmp(String),
    JmpCondition(ConditionCode, String),
    // write 0 or 1 to the first byte of dst based on Cmp output.
    // only takes 1-byte operands at dst
    SetCondition(ConditionCode, Operand),
    // named label to jump to. Follows different indentation rules to others.
    Label(String),
    // only takes 4-byte operands at dst
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Clone, Debug)]
enum Operand {
    Imm(ImmediateValue), //constant numeric value
    Reg(Register),       // register in assembly
    MockReg(String),     // mocked register for temporary use.
    // corresponds to READING A MEMORY ADDRESS from reg, not offsetting from r.
    Memory(Register, i32),
    MockMemory(String, i32), // for memory objects which may fit into a register instead.
    Data(String, i32),       // used for static and global variables, RIP-relative
    // used to index into compound objects during initialisation (base, index, scale)
    // computes base + index * scale in 1 cycle :)
    Indexed(Register, Register, i32),
}

#[derive(Clone, Debug)]
enum ImmediateValue {
    Signed(i64),
    Unsigned(u64),
}

impl ImmediateValue {
    fn can_fit_in_longword(&self) -> bool {
        match self {
            ImmediateValue::Signed(value) => *value <= i32::MAX.into() && *value >= i32::MIN.into(),
            // the boundary for immediate values being too big is ALWAYS 2^31 - 1, not 2^32 - 1 for
            // unsigned
            ImmediateValue::Unsigned(value) => *value <= i32::MAX as u64,
        }
    }
    fn can_fit_in_byte(&self) -> bool {
        match self {
            ImmediateValue::Signed(value) => *value <= i8::MAX.into() && *value >= i8::MIN.into(),
            ImmediateValue::Unsigned(value) => *value <= u8::MAX as u64,
        }
    }
    fn truncate(&mut self) {
        match self {
            ImmediateValue::Signed(ref mut value) => *value = (*value as i32).into(),
            ImmediateValue::Unsigned(ref mut value) => *value = (*value as u32).into(),
        }
    }
    fn truncate_to_byte(&mut self) {
        match self {
            ImmediateValue::Signed(ref mut value) => *value = (*value as u8).into(),
            ImmediateValue::Unsigned(ref mut value) => *value = (*value as u8).into(),
        }
    }
}

#[derive(Clone, Debug)]
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
    SP, // the stack pointer!! eg %rsp
    BP, // %rbp for function stack frame setup
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

static FUNCTION_PARAM_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];
static FUNCTION_RETURN_REGISTERS: [Register; 2] = [Register::AX, Register::DX];

static DOUBLE_PARAM_REGISTERS: [Register; 8] = [
    Register::XMM0,
    Register::XMM1,
    Register::XMM2,
    Register::XMM3,
    Register::XMM4,
    Register::XMM5,
    Register::XMM6,
    Register::XMM7,
];
static DOUBLE_RETURN_REGISTERS: [Register; 2] = [Register::XMM0, Register::XMM1];

#[derive(Clone, Debug)]
enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftLeft,  //shl
    UnsignedShiftRight, //shr
    DivDouble,
    And,
    Xor,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum ConditionCode {
    E,
    Ne,
    G,
    Ge,
    L,
    Le,
    // unsigned counterparts to g and l, 'above' and 'below'
    A,
    Ae,
    B,
    Be,
    P, // parity, needed for NaN
}

pub fn codegen(
    parsed: BirdsProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
) -> Result<Program, Box<dyn Error>> {
    let mut context = ConvertContext::new(comments, linux, mac, symbols, structs);
    let mut converted = parsed.convert(&mut context)?;

    let mut assembly_map: HashMap<String, AssemblySymbolInfo> = HashMap::new();

    // swap the symbols map out of the ConvertContext so we can iterate over it and build a new map
    let mut stolen_map = HashMap::new();
    swap(&mut stolen_map, &mut context.symbols);
    for (k, v) in stolen_map {
        if let Type::Function(_, _) = v.symbol_type {
            if let StorageInfo::Function(defined, _) = v.storage {
                let uses_memory = matches!(
                    context.functions_which_return_using_memory.get(&k),
                    Some(true)
                );

                assembly_map.insert(
                    k.clone(),
                    AssemblySymbolInfo::Function(defined, uses_memory),
                );
            } else {
                panic!("Function storage info has the wrong type")
            }
        } else {
            let is_constant = matches!(v.storage, StorageInfo::Constant(_));
            let is_static = is_constant || matches!(v.storage, StorageInfo::Static(_, _));

            // do not try and convert incomplete types to assembly types, it will break everything
            // since the storage size of an incomplete type is indeterminate
            if v.symbol_type.is_complete(&mut context.structs) {
                let assembly_type = v.symbol_type.convert(&mut context).unwrap();

                assembly_map.insert(
                    k.clone(),
                    AssemblySymbolInfo::Object(assembly_type, is_static, is_constant),
                );
            }
        }
    }
    for top_level in &converted.body {
        // there are some Constant doubles left over after the conversion, so we add them to the
        // symbols map.
        if let TopLevel::StaticConstant(name, _, _) = top_level {
            assembly_map.insert(
                name.clone(),
                AssemblySymbolInfo::Object(AssemblyType::Double, true, true),
            );
        }
    }

    let mut validate_context = ValidateContext::new(assembly_map, &context);

    for pass in VALIDATION_PASSES.iter() {
        validate_context.pass = Some(pass.clone());
        converted.validate(&mut validate_context)?;
    }
    // println!("{:?}", converted);

    // store the DisplayContext in a RefCell so that the Display trait can pick out
    // the value and modify it while it is rendering the actual Assembly code.
    converted.displaying_context = Some(RefCell::new(DisplayContext::new(
        &context,
        validate_context,
    )));

    Ok(converted)
}
