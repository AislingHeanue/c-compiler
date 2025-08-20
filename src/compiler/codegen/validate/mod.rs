use std::{collections::HashMap, error::Error};

use super::{
    AssemblySymbolInfo, AssemblyType, BinaryOperator, ConditionCode, ConvertContext,
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

pub static VALIDATION_PASSES: [ValidationPass; 10] = [
    // always first
    ValidationPass::AllocateRegisters,
    ValidationPass::ReplaceMockRegisters,
    // always second, sets stack sizes for each function based on the previous pass.
    // also saves and restores any callee-saved registers used while allocating registers
    ValidationPass::AllocateFunctionStack,
    ValidationPass::RewriteRet,
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
    current_stack_locations: HashMap<String, i32>,
    stack_sizes: HashMap<String, u32>,
    current_function_name: Option<String>,
    num_labels: u32,
    block_live_variables: HashMap<usize, Vec<Operand>>,
    aliased_variables: Vec<Operand>,
    static_variables: Vec<Operand>,
    function_callee_saved_registers: HashMap<String, Vec<Register>>,
    register_map: HashMap<String, Register>,
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
            current_stack_locations: HashMap::new(),
            stack_sizes: HashMap::new(),
            current_function_name: None,
            num_labels: context.num_labels,
            block_live_variables: HashMap::new(),
            aliased_variables: Vec::new(),
            static_variables: Vec::new(),
            function_callee_saved_registers: HashMap::new(),
            register_map: HashMap::new(),
        }
    }
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
            TopLevel::Function(name, instructions, _global) => {
                context.current_function_name = Some(name.to_string());
                instructions.validate(context)?;
                context.current_function_name = None
            }
            TopLevel::StaticVariable(_name, _global, _alignment, _init) => {}
            TopLevel::StaticConstant(_name, _alignment, _init) => {}
        }
        Ok(())
    }
}
