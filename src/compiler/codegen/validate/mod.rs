use std::{
    collections::{HashMap, HashSet},
    error::Error,
};

use super::{
    convert::Args, AssemblySymbolInfo, AssemblyType, BinaryOperator, ConditionCode, ConvertContext,
    ImmediateValue, Instruction, Operand, Program, Register, TopLevel,
};

mod allocation;
mod flow_graph;
mod instruction;
mod interference_graph;
mod operand;

pub trait Validate
where
    Self: Sized,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
}

pub static VALIDATION_PASSES: [ValidationPass; 11] = [
    // always first
    ValidationPass::AllocateRegisters,
    ValidationPass::ReplaceMockRegisters,
    // always second, sets stack sizes for each function based on the previous pass.
    // also saves and restores any callee-saved registers used while allocating registers
    ValidationPass::AllocateFunctionStack,
    ValidationPass::RewriteRet,
    ValidationPass::RewriteVaStart,
    // other validation steps
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
    AllocateRegisters,
    ReplaceMockRegisters,
    AllocateFunctionStack,
    RewriteRet,
    RewriteVaStart,
    CheckNaNComparisons,
    FixShiftOperatorRegister,
    RewriteMovZeroExtend,
    FixBadSrc,
    FixBadDst,
    FixTwoMemoryAccesses,
}

pub struct ValidateContext {
    pub pass: Option<ValidationPass>,
    pub symbols: HashMap<String, AssemblySymbolInfo>,
    current_stack_size: u32,
    num_labels: u32,

    block_live_variables: HashMap<usize, HashSet<Operand>>,
    aliased_variables: Vec<Operand>,
    static_variables: Vec<Operand>,
    register_map: HashMap<String, Register>,

    current_function_is_variadic: bool,
    current_stack_locations: HashMap<String, i32>,
    current_function_name: Option<String>,
    current_function_args: Option<Args>,

    function_stack_sizes: HashMap<String, u32>,
    function_callee_saved_registers: HashMap<String, Vec<Register>>,
    function_va_lists: HashMap<String, VaList>,
}

impl ValidateContext {
    pub fn new(
        symbols: HashMap<String, AssemblySymbolInfo>,
        context: &ConvertContext,
    ) -> ValidateContext {
        ValidateContext {
            symbols,
            pass: None,
            current_stack_size: 0,
            num_labels: context.num_labels,

            block_live_variables: HashMap::new(),
            aliased_variables: Vec::new(),
            static_variables: Vec::new(),
            register_map: HashMap::new(),

            current_stack_locations: HashMap::new(),
            current_function_name: None,
            current_function_is_variadic: false,
            current_function_args: None,

            function_stack_sizes: HashMap::new(),
            function_callee_saved_registers: HashMap::new(),
            function_va_lists: HashMap::new(),
        }
    }
}

pub struct VaList {
    gp_offset: u64,         // positive bytes
    fp_offset: u64,         // positive bytes
    overflow_arg_area: i32, // negative, only used if args spill over in function CALL
    reg_save_area: i32,     // negative, relative to RBP. 6xGP(8 bytes) + 8xFP(16 bytes) = 176 bytes
}

impl Validate for Program {
    fn validate(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        for d in self.body.iter_mut() {
            d.validate(context)?;
        }
        Ok(())
    }
}

impl Validate for TopLevel {
    fn validate(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            TopLevel::Function(name, instructions, _global, args, is_variadic) => {
                context.current_function_name = Some(name.to_string());
                context.current_function_args = Some(args.clone());
                context.current_function_is_variadic = *is_variadic;

                instructions.validate(context)?;
                context.current_function_name = None;
                context.current_function_args = None;
            }
            TopLevel::StaticVariable(_name, _global, _alignment, _init) => {}
            TopLevel::StaticConstant(_name, _alignment, _init) => {}
        }
        Ok(())
    }
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
