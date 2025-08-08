use crate::compiler::{
    birds::{BirdsProgramNode, BirdsValueNode},
    types::StaticInitialiser,
};
use itertools::process_results;
use std::error::Error;

use super::{
    AssemblyType, BinaryOperator, ConditionCode, Convert, ConvertContext, ImmediateValue,
    Instruction, Operand, Program, Register, TopLevel, UnaryOperator, DOUBLE_PARAM_REGISTERS,
    FUNCTION_PARAM_REGISTERS,
};

mod assembly_type;
mod instruction;
mod operand;
mod operators;
mod top_level;

struct Args {
    double: Vec<Operand>,
    integer: Vec<(Operand, AssemblyType)>,
    stack: Vec<(Operand, AssemblyType)>,
}

fn classify_function_args(
    values: Vec<BirdsValueNode>,
    context: &mut ConvertContext,
) -> Result<Args, Box<dyn Error>> {
    let mut args = Args {
        integer: Vec::new(),
        double: Vec::new(),
        stack: Vec::new(),
    };

    for v in values {
        let t = AssemblyType::infer(&v, context)?.0;
        if t == AssemblyType::Double {
            if args.double.len() < 8 {
                args.double.push(v.convert(context)?);
            } else {
                args.stack.push((v.convert(context)?, t));
            }
        } else if args.integer.len() < 6 {
            args.integer.push((v.convert(context)?, t));
        } else {
            args.stack.push((v.convert(context)?, t));
        }
    }

    Ok(args)
}

fn create_static_constant(
    alignment: u32,
    value: StaticInitialiser,
    context: &mut ConvertContext,
) -> Operand {
    if let Some(key) = context
        .constants
        .iter()
        .find(|(_, v)| **v == (alignment, value.clone()))
        .map(|(k, _)| k)
    {
        return Operand::MockReg(key.clone());
    }

    let new_name = format!("constant_{}", context.constants.len() + 1);
    context
        .constants
        .insert(new_name.clone(), (alignment, value));

    Operand::MockReg(new_name)
}

impl<U, V> Convert<Vec<U>> for Vec<V>
where
    V: Convert<U>,
{
    fn convert(self, context: &mut ConvertContext) -> Result<Vec<U>, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|function| function.convert(context)),
            |iter| iter.collect(),
        )
    }
}

impl Convert<Program> for BirdsProgramNode {
    fn convert(self, context: &mut ConvertContext) -> Result<Program, Box<dyn Error>> {
        let mut body = self.body.convert(context)?;

        for (name, (align, value)) in context.constants.iter() {
            body.push(TopLevel::StaticConstant(
                name.clone(),
                *align,
                value.clone(),
            ))
        }

        Ok(Program {
            body,
            displaying_context: None,
        })
    }
}
