use std::{cell::RefCell, collections::HashMap, error::Error, mem::swap};

use super::{
    birds::BirdsProgramNode,
    types::{StaticInitial, StorageInfo, SymbolInfo, Type},
};

mod convert;
mod display;
mod validate;

#[derive(Debug)]
pub struct Program {
    body: Vec<TopLevel>,
    displaying_context: Option<RefCell<DisplayContext>>,
}

#[derive(Debug)]
enum TopLevel {
    // header instructions, name, body, global
    Function(String, Vec<Instruction>, bool),
    // name global alignment init
    StaticVariable(String, bool, u32, Vec<StaticInitial>),
    // used for all floating point constants. Needed anytime we need eg.
    // 'float(1)' or 'float(0)' or 'float(i64::MAX+1)'
    // will be extended to other types later.
    // name alignment init
    StaticConstant(String, u32, StaticInitial),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum AssemblyType {
    Byte,     // byte
    Longword, // i32, int
    Quadword, // i64, long
    Double,   // oh boy
    // size, alignment
    ByteArray(u32, u32), // 'opaque chunk of memory' eg arrays
}

#[derive(Debug)]
pub enum AssemblySymbolInfo {
    // type, is_static is_a_top_level_constant
    Object(AssemblyType, bool, bool),
    // is_defined
    Function(bool),
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
    Imm(ImmediateValue),     //constant numeric value
    Reg(Register),           // register in assembly
    MockReg(String),         // mocked register for temporary use.
    Memory(Register, i32),   // entry whose value is the offset from the specified register.
    MockMemory(String, i32), // for memory objects which may fit into a register instead.
    Data(String),            // used for static and global variables
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
            // the boundary for immediate values being too big is ALWAYS 2^31 - 1, not 2^32 - 1 for
            // unsigned
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

#[derive(Clone, Debug)]
enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
enum BinaryOperator {
    Add,
    Sub,
    Mult,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftLeft,
    UnsignedShiftRight, //shr
    DivDouble,
    And,
    Xor,
    Or,
}

#[derive(Debug, PartialEq)]
enum ConditionCode {
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
    constants: HashMap<String, (u32, StaticInitial)>,
    num_labels: u32,
}

trait CodeDisplay {
    fn show(&self, context: &mut DisplayContext) -> String;
}

#[derive(Debug)]
pub struct DisplayContext {
    comments: bool,
    indent: usize,
    is_linux: bool,
    is_mac: bool,
    word_length_bytes: i32,
    instruction_suffix: String,
    symbols: HashMap<String, AssemblySymbolInfo>,
}

pub fn codegen(
    parsed: BirdsProgramNode,
    comments: bool,
    linux: bool,
    mac: bool,
    symbols: HashMap<String, SymbolInfo>,
) -> Result<Program, Box<dyn Error>> {
    let mut context = ConvertContext {
        comments,
        is_linux: linux,
        is_mac: mac,
        symbols,
        constants: HashMap::new(),
        num_labels: 0,
    };
    let mut converted = Program::convert(parsed, &mut context)?;

    let mut assembly_map: HashMap<String, AssemblySymbolInfo> = HashMap::new();

    // swap the symbols map out of the ConvertContext so we can iterate over it and build a new map
    let mut stolen_map = HashMap::new();
    swap(&mut stolen_map, &mut context.symbols);
    for (k, v) in stolen_map {
        if let Type::Function(_, _) = v.symbol_type {
            if let StorageInfo::Function(defined, _) = v.storage {
                assembly_map.insert(k.clone(), AssemblySymbolInfo::Function(defined));
            } else {
                panic!("Function storage info has the wrong type")
            }
        } else {
            let is_constant = matches!(v.storage, StorageInfo::Constant(_));
            let is_static = is_constant || matches!(v.storage, StorageInfo::Static(_, _));

            let assembly_type = AssemblyType::convert(v.symbol_type, &mut context).unwrap();

            assembly_map.insert(
                k.clone(),
                AssemblySymbolInfo::Object(assembly_type, is_static, is_constant),
            );
        }
    }
    for top_level in &converted.body {
        // there are some Constant doubles left over after the conversion, so we add them to the
        // symbols map.
        // FIXME: This should also apply to strings but I'm not sure how
        if let TopLevel::StaticConstant(name, _, _) = top_level {
            assembly_map.insert(
                name.clone(),
                AssemblySymbolInfo::Object(AssemblyType::Double, true, true),
            );
        }
    }

    let mut validate_context = ValidateContext {
        symbols: assembly_map,
        pass: None,
        current_stack_size: 0,
        current_stack_locations: HashMap::new(),
        stack_sizes: HashMap::new(),
        current_function_name: None,
        num_labels: context.num_labels,
    };

    for pass in VALIDATION_PASSES.iter() {
        validate_context.pass = Some(pass.clone());
        converted.validate(&mut validate_context)?;
    }
    // println!("{:?}", converted);

    // store the DisplayContext in a RefCell so that the Display trait can pick out
    // the value and modify it while it is rendering the actual Assembly code.
    converted.displaying_context = Some(RefCell::new(DisplayContext {
        comments: context.comments,
        indent: 0,
        word_length_bytes: 4,
        instruction_suffix: "l".to_string(),
        is_linux: context.is_linux,
        is_mac: context.is_mac,
        symbols: validate_context.symbols,
    }));

    Ok(converted)
}

trait Validate
where
    Self: Sized,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
}

static VALIDATION_PASSES: [ValidationPass; 8] = [
    // always first
    ValidationPass::ReplaceMockRegisters,
    // always second, sets stack sizes for each function based on the previous pass.
    ValidationPass::AllocateFunctionStack,
    ValidationPass::CheckNaNComparisons,
    ValidationPass::FixShiftOperatorRegister,
    ValidationPass::RewriteMovZeroExtend,
    ValidationPass::FixBadSrc,
    ValidationPass::FixBadDst,
    // moving the double memory access instruction lower because many other passes may fix the
    // issue that this addresses, saving a Mov instruction.
    ValidationPass::FixTwoMemoryAccesses,
];

#[derive(Clone, Debug)]
pub enum ValidationPass {
    ReplaceMockRegisters,
    AllocateFunctionStack,
    CheckNaNComparisons,
    FixShiftOperatorRegister,
    RewriteMovZeroExtend,
    FixBadSrc,
    FixBadDst,
    FixTwoMemoryAccesses,
}

struct ValidateContext {
    symbols: HashMap<String, AssemblySymbolInfo>,
    pass: Option<ValidationPass>,
    current_stack_size: u32,
    current_stack_locations: HashMap<String, i32>,
    stack_sizes: HashMap<String, u32>,
    current_function_name: Option<String>,
    num_labels: u32,
}
