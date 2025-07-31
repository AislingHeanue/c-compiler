use std::{cell::RefCell, collections::HashMap, error::Error, mem::swap};

use super::{
    birds::BirdsProgramNode,
    parser::{StaticInitial, StorageInfo, SymbolInfo, Type},
};

mod convert;
mod display;
mod validate;

pub struct Program {
    body: Vec<TopLevel>,
    displaying_context: Option<RefCell<DisplayContext>>,
}

enum TopLevel {
    // header instructions, name, body, global
    Function(String, Vec<Instruction>, bool),
    // name global alignment init
    StaticVariable(String, bool, u64, StaticInitial),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum AssemblyType {
    Longword, // i32, int
    Quadword, // i64, long
}

pub enum AssemblySymbolInfo {
    // type, is_static
    Object(AssemblyType, bool),
    // is_defined
    Function(bool),
}

#[derive(Debug)]
enum Instruction {
    // Mov type, src, dst
    Mov(AssemblyType, Operand, Operand),
    // Mov Signed Extend src (32-bit) dst (64-bit)
    Movsx(Operand, Operand),
    Unary(UnaryOperator, AssemblyType, Operand), // Operand here is both the src and dst.
    // op src, dst. dst is the *first* number in the operation
    Binary(BinaryOperator, AssemblyType, Operand, Operand),
    // compare left and right and set "ZF" (zero), "SF" (sign) and "OF" (signed overflow)
    // based on the result, so that they can be read by JmpCondition and SetCondition
    Cmp(AssemblyType, Operand, Operand),
    // dividend comes from EDX+EAX. quotient -> EDX, remainder -> EAX.
    Idiv(AssemblyType, Operand),
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
    Imm(i64),        //constant numeric value
    Reg(Register),   // register in assembly
    MockReg(String), // mocked register for temporary use.
    Stack(i32),      // Stack entry whose value is the offset from RSP.
    Data(String),    // used for static and global variables
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
}

static FUNCTION_PARAM_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
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
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
}

#[derive(Debug)]
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

pub struct DisplayContext {
    comments: bool,
    indent: usize,
    is_linux: bool,
    is_mac: bool,
    word_length_bytes: i32,
    instruction_suffix: String,
    symbols: HashMap<String, AssemblySymbolInfo>,
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
        self.instruction_suffix = "".to_string(); // unused
        self
    }
    fn regular(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 4;
        self.instruction_suffix = "l".to_string();
        self
    }
    fn long(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 8;
        self.instruction_suffix = "q".to_string();
        self
    }
    fn suffix_for_type(&mut self, t: &AssemblyType) -> String {
        match t {
            AssemblyType::Longword => {
                self.regular();
                "l".to_string()
            }
            AssemblyType::Quadword => {
                self.long();
                "q".to_string()
            }
        }
    }
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
            // only remaining options are int and long
            let is_static = matches!(v.storage, StorageInfo::Static(_, _));
            let assembly_type = AssemblyType::convert(v.symbol_type, &mut context).unwrap();

            assembly_map.insert(
                k.clone(),
                AssemblySymbolInfo::Object(assembly_type, is_static),
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
    };

    let validation_passes = vec![
        ValidationPass::ReplaceMockRegisters,
        ValidationPass::AllocateFunctionStack,
        ValidationPass::FixShiftOperatorRegister,
        ValidationPass::FixTwoMemoryAccesses,
        ValidationPass::FixBadImmValues,
        ValidationPass::FixMemoryAsDst,
        ValidationPass::FixConstantAsDst,
        ValidationPass::FixLargeInts,
    ];

    for pass in validation_passes {
        validate_context.pass = Some(pass);
        converted.validate(&mut validate_context)?;
    }

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

#[derive(Clone, Debug)]
pub enum ValidationPass {
    ReplaceMockRegisters,
    AllocateFunctionStack,
    FixTwoMemoryAccesses,
    FixBadImmValues,
    FixMemoryAsDst,
    FixConstantAsDst,
    FixShiftOperatorRegister,
    FixLargeInts,
}

struct ValidateContext {
    symbols: HashMap<String, AssemblySymbolInfo>,
    pass: Option<ValidationPass>,
    current_stack_size: i32,
    current_stack_locations: HashMap<String, i32>,
    stack_sizes: HashMap<String, i32>,
    current_function_name: Option<String>,
}
