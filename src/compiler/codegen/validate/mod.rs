use std::{collections::HashMap, error::Error};

use super::{
    AssemblySymbolInfo, AssemblyType, BinaryOperator, ConditionCode, ConvertContext,
    ImmediateValue, Instruction, Operand, Program, Register, TopLevel,
};

mod instruction;
mod operand;

pub trait Validate
where
    Self: Sized,
{
    fn validate(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;
}

pub static VALIDATION_PASSES: [ValidationPass; 8] = [
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

pub struct ValidateContext {
    pub pass: Option<ValidationPass>,
    pub symbols: HashMap<String, AssemblySymbolInfo>,
    current_stack_size: u32,
    current_stack_locations: HashMap<String, i32>,
    stack_sizes: HashMap<String, u32>,
    current_function_name: Option<String>,
    num_labels: u32,
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
        }
    }
}

pub fn align_stack_size(initial: u32, alignment: u32) -> u32 {
    initial + (alignment as i32 - initial as i32).rem_euclid(alignment as i32) as u32
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
