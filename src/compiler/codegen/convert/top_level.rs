use crate::compiler::{
    birds::{BirdsTopLevel, BirdsValueNode},
    types::Type,
};
use itertools::{process_results, Itertools};
use std::error::Error;

use super::{
    classify_return_by_type, pass_args_to_callee, AssemblyType, Convert, ConvertContext,
    Instruction, Operand, Register, TopLevel, DOUBLE_PARAM_REGISTERS, FUNCTION_PARAM_REGISTERS,
};

impl Convert<TopLevel> for BirdsTopLevel {
    fn convert(self, context: &mut ConvertContext) -> Result<TopLevel, Box<dyn Error>> {
        match self {
            BirdsTopLevel::Function(..) => self.convert_function(context),
            BirdsTopLevel::StaticVariable(t, name, init, global) => Ok(TopLevel::StaticVariable(
                name,
                global,
                t.convert(context)?.get_alignment(),
                init,
            )),
            BirdsTopLevel::StaticConstant(t, name, init) => Ok(TopLevel::StaticConstant(
                name,
                t.convert(context)?.get_alignment(),
                init,
            )),
        }
    }
}

impl BirdsTopLevel {
    fn convert_function(self, context: &mut ConvertContext) -> Result<TopLevel, Box<dyn Error>> {
        let (name, params, parsed_instructions, global) = match self {
            BirdsTopLevel::Function(a, b, c, d) => (a, b, c, d),
            _ => unreachable!(),
        };
        let params_vars = params
            .iter()
            .map(|name| BirdsValueNode::Var(name.to_string()))
            .collect_vec();
        let return_uses_memory = if let Some(Type::Function(return_type, _)) =
            context.symbols.get(&name).map(|a| &a.symbol_type)
        {
            classify_return_by_type(&return_type.clone(), context)?.0
        } else {
            unreachable!()
        };

        let params = pass_args_to_callee(params_vars, return_uses_memory, context)?;

        let mut instructions: Vec<Instruction> = Vec::new();
        let register_offset = if return_uses_memory {
            instructions.push(Instruction::Mov(
                AssemblyType::Quadword,
                Operand::Reg(FUNCTION_PARAM_REGISTERS[0].clone()),
                Operand::Memory(Register::BP, -8),
            ));
            1
        } else {
            0
        };

        // ZEROTH PASS: Copy all the params out of registers onto the stack.
        for (i, (param, _, param_type)) in params.integer.into_iter().enumerate() {
            if let AssemblyType::ByteArray(size, _) = param_type {
                Instruction::copy_bytes_from_register(
                    size as i32,
                    &mut instructions,
                    &FUNCTION_PARAM_REGISTERS[i + register_offset],
                    &param,
                )
            } else {
                instructions.push(Instruction::Mov(
                    param_type,
                    Operand::Reg(FUNCTION_PARAM_REGISTERS[i + register_offset].clone()),
                    param,
                ));
            }
        }

        for (i, (param, param_type)) in params.double.into_iter().enumerate() {
            instructions.push(Instruction::Mov(
                param_type,
                Operand::Reg(DOUBLE_PARAM_REGISTERS[i].clone()),
                param,
            ));
        }

        for (i, (param, _, param_type)) in params.stack.into_iter().enumerate() {
            if let AssemblyType::ByteArray(size, _) = param_type {
                Instruction::copy_bytes(
                    size as i32,
                    &mut instructions,
                    &Operand::Memory(Register::BP, 16 + 8 * (i as i32)),
                    &param,
                    0,
                    0,
                )
            } else {
                instructions.push(Instruction::Mov(
                    param_type,
                    Operand::Memory(Register::BP, 16 + 8 * (i as i32)),
                    param,
                ));
            }
        }

        // FIRST PASS: create a bunch of mock registers to be replaced with stack entries later
        instructions.append(&mut process_results(
            parsed_instructions
                .into_iter()
                .map(|instruction| instruction.convert(context)),
            |iter| iter.flatten().collect(),
        )?);

        Ok(TopLevel::Function(name, instructions, global))
    }
}
